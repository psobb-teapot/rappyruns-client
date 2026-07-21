(in-package :ephinea-ta-client)

;;; Automatic quest recording. Pure orchestration: the recorder is a
;;; small state machine fed from the poll loop, and every side effect
;;; (spawning/stopping ffmpeg, renaming/deleting files) goes through the
;;; capture-backend protocol below, mirroring the READ-BLOCK split
;;; between win32.lisp and MOCK-READER. The live backend lives in
;;; ffmpeg-win32.lisp (LispWorks only); tests drive a mock.
;;;
;;; Lifecycle: one quest stay = one capture. Recording starts when the
;;; detector enters :in-quest and stops when it returns to :idle (all
;;; trackers done, quest unloaded, or the game vanished). The file is
;;; kept only when at least one run completed during the capture;
;;; abandoned quests are deleted. Output is fragmented MP4, so even a
;;; TerminateProcess leaves a playable file. A kept file is then
;;; remuxed into a regular MP4 with the moov up front (fragmented MP4
;;; carries no duration/seek index, so browsers playing the hosted
;;; video would show a seekbar that grows as it loads); the same pass
;;; loudness-normalizes the audio, which must not happen live (see
;;; BUILD-FFMPEG-ARGS). The fragmented original is kept as-is when the
;;; remux fails - playable, just quiet when the mixer volume is low.

;;; Capture-backend protocol

(defgeneric backend-start-capture (backend ffmpeg-path args output-path
                                   &key audio-pipe audio-pid)
  (:documentation
   "Spawn ffmpeg with ARGS (a list of argv strings; OUTPUT-PATH is the
last of them, passed separately so the backend can create the
directory). When AUDIO-PIPE is non-NIL, ARGS reference it as a second
input and the backend must serve AUDIO-PID's game audio on it (fixing
the format tokens in ARGS up to match, see RETARGET-AUDIO-ARGS).
Returns a capture token, or (values nil error-string)."))

(defgeneric backend-capture-alive-p (backend capture))

(defgeneric backend-request-stop (backend capture)
  (:documentation "Ask ffmpeg to finish gracefully (\"q\" on stdin)."))

(defgeneric backend-kill-capture (backend capture)
  (:documentation "Force-terminate ffmpeg (TerminateProcess)."))

(defgeneric backend-close-capture (backend capture)
  (:documentation "Release process/pipe handles once the process is dead."))

(defgeneric backend-start-remux (backend ffmpeg-path args)
  (:documentation
   "Spawn ffmpeg with ARGS (see BUILD-REMUX-ARGS) to rewrite a finished
recording. Returns a capture token usable with BACKEND-CAPTURE-ALIVE-P /
BACKEND-CAPTURE-SUCCEEDED-P / BACKEND-KILL-CAPTURE /
BACKEND-CLOSE-CAPTURE, or (values nil error-string)."))

(defgeneric backend-capture-succeeded-p (backend capture)
  (:documentation "Did the (dead) process exit with status 0?"))

(defgeneric backend-rename-file (backend from to))

(defgeneric backend-delete-file (backend path))

(defgeneric backend-capture-monitor (backend)
  (:documentation
   "Plist (:output-idx N :width W :height H [:crop (x y w h)])
describing the monitor the game window sits on - the 0-based DXGI
output index and the monitor rect's pixel size - or NIL when the
window is absent or the monitor cannot be resolved (BUILD-FFMPEG-ARGS
then falls back to gdigrab). Queried once per capture start. :CROP,
present for a window smaller than its monitor, is the client area in
monitor-relative pixels. Monitor capture (ddagrab) is the primary path
for windowed games too, not just fullscreen: GDI cannot see an
exclusive-fullscreen Direct3D surface (black frames, the Boot Camp
machine 2026-07-07) NOR a flip-model-composited window (black frames
again, run 949 on a Windows 11 machine, 2026-07-16) - and a window
that gdigrab happens to read fine is captured by ddagrab just as
well.")
  (:method (backend) nil))

(defgeneric backend-list-stale-files (backend dir)
  (:documentation "Leftover rec-tmp-*.mp4 files from a previous session."))

(defgeneric backend-list-recordings (backend dir)
  (:documentation
   "Kept recordings in DIR (the finished, run-named files - not the
rec-tmp-* work files), each as a (namestring size-bytes write-date)
triple, for the local storage budget (APPLY-RECORDING-RETENTION)."))

;;; Encoder settings. Not user-facing config: veryfast/29 capped at
;;; 1080p costs a few percent CPU at PSOBB resolutions and the quality
;;; is fine for run verification. Measured on a real 3200x1800 capture:
;;; CRF 23 uncapped produced ~90 MB per minute (a 10-minute quest stay
;;; was ~1 GB, and the 2 GiB upload cap was only ~22 minutes away);
;;; 1080p + CRF 29 without B-frames is ~21 MB per minute at the same
;;; frame count. No B-frames (-bf 0) because the fragmented-MP4 live
;;; capture bakes the reorder delay in as a 2-frame video start offset
;;; that local players honor and browsers normalize away - the same
;;; recording played 67 ms differently on the site than on disk (the
;;; whole run-92 saga). pts = dts from zero is the one timestamp shape
;;; every player agrees on; CRF 29 rather than 28 pays for the ~14%
;;; B-frames used to save, keeping sizes level. Smoothness and sync
;;; beat sharpness here, and the HUD stays legible.

(defparameter +record-framerate+ 30)
(defparameter +record-preset+ "veryfast")
(defparameter +record-crf+ 29)
(defparameter +record-max-threads+ 8
  "Upper bound on the x264 thread cap; beyond this more threads only
add contention, veryfast 1080p30 never needs them.")

(defun logical-processor-count ()
  "Logical processors per Windows, or 4 when undetectable."
  (or (ignore-errors
        (parse-integer (uiop:getenv "NUMBER_OF_PROCESSORS")))
      4))

(defun encoder-thread-count (&optional (cores (logical-processor-count)))
  "libx264 -threads for the live capture: half the logical processors,
at least 2 and at most +RECORD-MAX-THREADS+. x264's default is
1.5 x cores, which floods every core with encoder threads and - even
at below-normal process priority - costs the game cache and scheduling
churn. Half the cores keeps veryfast 1080p30 comfortably real time
(field numbers put one core near 60 fps) while leaving the other half
to PSOBB; the floor of 2 protects the encode on small machines because
recording smoothness outranks everything else here."
  (max 2 (min +record-max-threads+ (floor cores 2))))
(defparameter +record-max-height+ 1080
  "Downscale cap. Windows larger than this record at 1080p; smaller
ones are left alone (min(), never an upscale). Height is forced even
for yuv420p, and -2 keeps the aspect ratio with an even width.")

;;; Hardware encoding. libx264 at veryfast still costs several cores
;;; of CPU next to a live game - the field report that Xbox Game Bar
;;; records the same game without lag comes down to its GPU encoder.
;;; A startup probe (PROBE-HW-ENCODER, ffmpeg-win32) finds the first
;;; vendor encoder this machine's ffmpeg can actually open, and
;;; BUILD-FFMPEG-ARGS swaps it in: same capture, same fragmented MP4,
;;; only the encode moves off the CPU (bench: 10 s of 1080p30 desktop
;;; fell from 7.2 s to 3.1 s of CPU time on an AMD iGPU, and the
;;; remaining cost is the shared download+scale, not the encoder).
;;; On Intel that remaining cost goes too: a second probe verifies the
;;; zero-copy fullscreen chain (ddagrab -> hwmap -> vpp_qsv ->
;;; h264_qsv) and captures then skip hwdownload + the CPU scale
;;; entirely (*HW-FULLSCREEN-GPU-CHAIN*). The field machine this
;;; targets - a 2-core Boot Camp MacBook Air recording a Retina
;;; fullscreen - paid ~500 MB/s of GPU->CPU copy plus a CPU scale as a
;;; fixed tax, and reported the game slowing late in each quest and
;;; worsening across laps: exactly the shape of thermal throttling
;;; under a sustained load that Game Bar's all-GPU pipeline avoids.

(defparameter +hw-encoder-candidates+ '("h264_nvenc" "h264_amf" "h264_qsv")
  "Vendor H.264 encoders in probe order. h264_mf is deliberately
absent: MediaFoundation silently falls back to a SOFTWARE MFT on
machines without a hardware one, which would burn CPU like x264 but at
bitrate-mode quality.")

(defvar *hw-video-encoder* nil
  "Encoder name chosen by the startup probe (ffmpeg-win32's
START-HW-ENCODER-PROBE), or NIL for libx264. Read at capture start, so
a capture that begins before the probe finishes just uses x264 once.")

(defvar *hw-encoder-probe-state* nil
  "NIL until the probe has run, :DONE when it produced a verdict (even
a negative one - *HW-VIDEO-ENCODER* stays NIL on a machine with no
hardware encoder), :SPAWN-FAILED when ffmpeg itself never started for
any candidate. The latter is not a missing encoder but a block outside
the probe - Smart App Control refusing the unsigned ffmpeg.exe with
\"Windows error 4551\" pinned a whole session to libx264 in the field
(2026-07-18) - and such blocks lift, so START-RECORDING re-probes.")

(defvar *hw-fullscreen-gpu-chain* nil
  "T when the startup probe verified the zero-copy fullscreen chain
\(ddagrab -> hwmap -> vpp_qsv -> h264_qsv) works on this machine.
The hwdownload fallback costs a full GPU->CPU frame copy plus a CPU
scale every frame - a fixed tax a weak machine (the 2-core MacBook Air
field report) pays on top of the game - so when the whole pipeline can
stay on the GPU it should. Intel only: the AMD equivalents (scale_d3d11,
vpp_amf) are broken in the bundled ffmpeg build (tried for PR 141).")

(defparameter +hw-record-bitrate+ "3500k"
  "Hardware encoders have no CRF; this VBR target keeps sizes near the
x264 CRF-29 field figure (~21 MB/min) at 1080p30.")
(defparameter +hw-record-maxrate+ "7M")
(defparameter +hw-record-bufsize+ "14M")

;;; Low-memory profile. Recording churns the Windows file cache with
;;; every byte it produces - the growing capture tmp, the remux that
;;; re-reads it and writes the final file, the upload that reads that -
;;; which an 8 GB machine (the Boot Camp MacBook Air, ~4 GB free beside
;;; Windows) feels as mounting memory pressure over a quest and across
;;; laps. A lower bitrate shrinks all of it proportionally; quality is
;;; the priority this project spends last (smoothness > hosting size >
;;; quality), and 2.5 Mbps 1080p30 keeps the HUD legible.

(defparameter +low-memory-threshold-gb+ 12
  "Machines with less physical RAM than this get the low-memory
recording profile. Catches 8 GB machines (reported totals run slightly
under the nominal size) without touching 16 GB ones.")
(defparameter +hw-record-bitrate-low+ "2500k")
(defparameter +hw-record-maxrate-low+ "5M")
(defparameter +hw-record-bufsize-low+ "10M")
(defparameter +record-crf-low+ 31
  "The x264 fallback's low-memory CRF: two steps over +RECORD-CRF+,
roughly a 25-30% smaller file.")

(defun physical-memory-bytes ()
  "Total physical RAM in bytes, or NIL when unknown (non-LispWorks)."
  #+lispworks (ignore-errors (%physical-memory-bytes))
  #-lispworks nil)

(defun low-memory-machine-p (&optional (bytes (physical-memory-bytes)))
  "T when this machine's RAM is under +LOW-MEMORY-THRESHOLD-GB+.
Unknown (NIL) means not low: never degrade quality on a machine that
was merely unreadable."
  (and bytes (< bytes (* +low-memory-threshold-gb+ (expt 2 30)))))

(defun hw-encoder-probe-args (encoder)
  "ffmpeg argv testing whether ENCODER can open on this machine: a few
black frames into the null muxer. Exit 0 means usable - vendor
encoders fail to open (fast) without the matching GPU/driver."
  (list "-hide_banner" "-loglevel" "error"
        "-f" "lavfi" "-i" "color=black:size=256x256:rate=30"
        "-frames:v" "8" "-c:v" encoder "-f" "null" "-"))

(defun hw-gpu-chain-probe-args ()
  "ffmpeg argv testing the zero-copy fullscreen chain: a few desktop
frames through ddagrab -> hwmap -> vpp_qsv -> h264_qsv into the null
muxer. Exit 0 means the whole D3D11->QSV handoff works on this
machine's driver stack; anything broken along it (hwmap derive, the
VPP session with its explicit bt709 conversion, the encoder sharing
the device) fails here at startup instead of failing a capture
mid-quest. vpp_qsv, not scale_qsv: only the full VPP filter exposes
out_color_matrix/out_range, and the probe must exercise the exact
options BUILD-FFMPEG-ARGS will use."
  (list "-hide_banner" "-loglevel" "error"
        "-f" "lavfi" "-i" "ddagrab=output_idx=0:framerate=30:draw_mouse=0"
        "-frames:v" "8"
        "-vf" "hwmap=derive_device=qsv,vpp_qsv=w=1280:h=720:format=nv12:out_color_matrix=bt709:out_range=tv"
        "-c:v" "h264_qsv"
        "-f" "null" "-"))

(defun record-scale-dimensions (width height &optional (cap +record-max-height+))
  "Even target WxH for a capture of a WIDTHxHEIGHT source: HEIGHT
capped at CAP (never upscaled), aspect kept. The GPU scale chain
\(vpp_qsv) gets literal dimensions - the monitor rect is known at
capture start - where the CPU path lets ffmpeg evaluate its
scale=-2:trunc(min(...)/2)*2 expression itself. Pure."
  (if (<= height cap)
      (values (* 2 (floor width 2)) (* 2 (floor height 2)))
      (values (* 2 (round (* width cap) (* 2 height)))
              (* 2 (floor cap 2)))))

;;; Color. Every capture source hands over sRGB desktop pixels (BGRA),
;;; and the RGB->YUV conversion used to happen implicitly - swscale's
;;; BT.601 default, no matrix tag in the file. Browsers assume BT.709
;;; for untagged HD, so hosted videos played with shifted hues (run
;;; 1368: the orange lamps toward red, verified by decoding the same
;;; frame both ways). Every chain now converts with an explicit
;;; bt709/limited on the filter that does the conversion (a YUV format
;;; right after it keeps an untagged auto-insert from sneaking back)
;;; and stamps the remaining color properties with setparams, so the
;;; encoder writes complete VUI metadata every player reads the same.

(defparameter +record-color-tags-filter+
  "setparams=color_primaries=bt709:color_trc=iec61966-2-1"
  "Tail of every capture filter chain: what the pixels really are -
sRGB transfer on bt709 primaries (desktop content; also what ddagrab
already propagated on the hw paths). Matrix and range ride in from the
converting filter's out_color_matrix/out_range. Frame properties, not
codec-level -color_trc/-color_primaries flags: the encoders in the
bundled ffmpeg take VUI from the frames and silently ignored the
codec options (verified against ffmpeg 8).")

(defun record-scale-filter ()
  ;; fast_bilinear, NOT lanczos: the grab -> scale -> encode loop is
  ;; serial per frame, and under live game load the window BitBlt
  ;; alone runs ~30 ms. Lanczos added ~16 ms per 3200x1800 frame
  ;; (field-measured: average frame interval went 40 ms -> 56 ms,
  ;; i.e. 25 fps -> 18 fps with second-long stalls), which starved
  ;; the capture and desynced the audio. The sharpness difference at
  ;; a 1080p downscale is negligible for run verification.
  ;; out_color_matrix/out_range take effect when this scale is the
  ;; RGB->YUV conversion, which the format filter right after it in
  ;; BUILD-FFMPEG-ARGS guarantees (see the color note above).
  (format nil "scale=-2:trunc(min(~d\\,ih)/2)*2:flags=fast_bilinear:out_color_matrix=bt709:out_range=tv"
          +record-max-height+))

(defvar *audio-target-pid* nil
  "PID of the attached PSOBB process, maintained by the poll loop.
The audio backend captures this process tree's sound (process
loopback), so only the game is heard - not Discord or system sounds.")

(defun audio-pipe-name ()
  "Named pipe ffmpeg reads raw captured audio from (second input)."
  "\\\\.\\pipe\\ephinea-ta-audio")
(defparameter +stop-grace-seconds+ 8
  "How long to wait for ffmpeg to exit after a stop request before
killing it. Must exceed the audio drain delay (ffmpeg-win32) plus
ffmpeg's own finalization time.")

(defparameter +remux-grace-seconds+ 180
  "How long the post-capture remux may run before it is killed and the
fragmented original is kept instead. The video is stream-copied but
the audio re-encodes through loudnorm, which still runs far faster
than real time; the margin covers an hour-long recording on a slow
machine under load.")

;;; Diagnostics report. The capture pipeline writes its decisions to
;;; %TEMP%\ephinea-ta-recording.log (RECORDING-LOG, ffmpeg-win32); after
;;; each video upload the client sends the log's tail to the server
;;; (SEND-RUN-DIAGNOSTICS!, store.lisp) so capture problems on machines
;;; we cannot reach - the run-949 all-black recordings took a week to
;;; even see - can be investigated from the run page.

(defun recording-log-path ()
  "The capture diagnostics log, shared with RECORDING-LOG's writer.
NOT uiop:temporary-directory on LispWorks: UIOP caches that directory
in a variable when it loads, so a delivered exe ships the BUILD
machine's path baked in - on every other machine that directory does
not exist and the log was silently never written (the first field
diagnostics all read \"(no recording log)\"; v0.41.0, 2026-07-16).
hcl:get-temp-directory asks Windows at call time instead."
  (merge-pathnames "ephinea-ta-recording.log"
                   #+lispworks (hcl:get-temp-directory)
                   #-lispworks (uiop:temporary-directory)))

(defparameter +diagnostics-tail-chars+ 65536
  "How much of the recording log's tail rides in a diagnostics report.
Comfortably several sessions' worth of RECORDING-LOG lines, and small
next to the video upload it follows.")

(defun file-tail (path max-chars)
  "The last MAX-CHARS characters of the UTF-8 text file at PATH; NIL
when the file is missing or unreadable. Reads forward in chunks (the
file is capped near +RECORDING-LOG-MAX-BYTES+, so this stays cheap)
rather than seeking, which would land mid-character."
  (handler-case
      (with-open-file (in path :external-format :utf-8)
        (let ((chunk (make-string 65536))
              (kept nil))
          (loop
            (let ((end (read-sequence chunk in)))
              (when (zerop end) (return))
              (let ((tail (concatenate 'string (or kept "")
                                       (subseq chunk 0 end))))
                (setf kept (if (> (length tail) max-chars)
                               (subseq tail (- (length tail) max-chars))
                               tail)))))
          kept))
    (error () nil)))

(defun diagnostics-report ()
  "The capture diagnostics to attach to an uploaded run: a machine
summary (the values BUILD-FFMPEG-ARGS decides by) plus the recording
log tail. Never signals; every field degrades to NIL text."
  (let ((path (ignore-errors (recording-log-path))))
    (format nil "client ~a~%os ~a ~a~%ram-gb ~a cores ~a~%~
                 hw-encoder ~a gpu-chain ~a low-memory ~a~%~
                 --- recording log tail (~a, exists ~a) ---~%~a"
            (client-version)
            (ignore-errors (software-type)) (ignore-errors (software-version))
            (let ((bytes (physical-memory-bytes)))
              (and bytes (round bytes (expt 2 30))))
            (logical-processor-count)
            *hw-video-encoder* *hw-fullscreen-gpu-chain*
            (low-memory-machine-p)
            ;; The path and its existence ride along so an empty tail is
            ;; diagnosable from the server: "the log is where we thought
            ;; and empty" reads very differently from "the path resolved
            ;; somewhere that does not exist" (exactly how the baked
            ;; build-machine path above was caught).
            path (and path (ignore-errors (and (probe-file path) t)))
            (or (and path (file-tail path +diagnostics-tail-chars+))
                "(no recording log)"))))

;;; Paths and filenames

(defun default-record-dir-choice (old-exists-p new-exists-p)
  "Which default recordings folder RESOLVE-RECORD-DIR should use, given
what exists on disk (pure, so the tests can pin it): :USE-NEW for a
fresh install or an already-migrated one - never rename onto an
existing folder - and :MIGRATE when only the pre-rename EphineaTA
folder exists."
  (if (or new-exists-p (not old-exists-p))
      :use-new
      :migrate))

(defun resolve-record-dir ()
  "Config :record-dir, or <user home>/Videos/RappyRuns/ when blank.
The pre-rename default was Videos/EphineaTA/; the first resolution
finding only that folder renames it in place, recordings included, and
keeps using it under the old name when the rename fails (say, a file
in it is open) so recordings never split across two folders."
  (let ((configured (string-trim " " (or (config-value :record-dir) ""))))
    (if (plusp (length configured))
        (uiop:ensure-directory-pathname configured)
        (let* ((home (uiop:ensure-directory-pathname
                      (or (uiop:getenv "USERPROFILE")
                          (namestring (user-homedir-pathname)))))
               (old (merge-pathnames "Videos/EphineaTA/" home))
               (new (merge-pathnames "Videos/RappyRuns/" home)))
          (ecase (default-record-dir-choice
                  (and (uiop:directory-exists-p old) t)
                  (and (uiop:directory-exists-p new) t))
            (:use-new new)
            (:migrate (if (ignore-errors (rename-file old new) t)
                          new
                          old)))))))

(defun resolve-ffmpeg-path ()
  "Config :ffmpeg-path, else ffmpeg/ffmpeg.exe next to the executable
(the bundled copy), else bare \"ffmpeg.exe\" (PATH search)."
  (let ((configured (string-trim " " (or (config-value :ffmpeg-path) ""))))
    (if (plusp (length configured))
        configured
        (let* ((exe (ignore-errors (first (uiop:raw-command-line-arguments))))
               (bundled (and exe
                             (probe-file
                              (merge-pathnames
                               "ffmpeg/ffmpeg.exe"
                               (uiop:pathname-directory-pathname exe))))))
          (if bundled (namestring bundled) "ffmpeg.exe")))))

(defun sanitize-filename (string)
  "Replace characters Windows forbids in filenames with dashes."
  (map 'string
       (lambda (char)
         (if (or (find char "\\/:*?\"<>|") (char< char #\Space))
             #\-
             char))
       string))

(defvar *recording-token-state* nil
  "Random state seeded lazily at RUNTIME (never at image-dump time, or a
delivered exe would ship one frozen sequence and every launched instance
would draw identical tokens - the very collision this guards against) so
two client instances never generate the same recording filename.")

(defun recording-token ()
  "A short random hex tag, unique per process, appended to each capture's
temp filename. With deterministic timestamp names two instances recording
the same game session (the pre-0.34.1 tray pile-up) produced the SAME
rec-tmp path and remuxed to the SAME final name, so their ffmpeg
processes wrote one file at once and corrupted it (good remux head +
foreign fragmented tail, seen on runs 418-424). The token keeps each
process's capture on its own path even if instances ever run again."
  (unless *recording-token-state*
    (setf *recording-token-state* (make-random-state t)))
  (format nil "~(~8,'0x~)" (random (expt 2 32) *recording-token-state*)))

(defun deduplicate-path (path)
  "PATH when it is free, else the same name with an Explorer-style
' (2)', ' (3)'... counter before the extension. Guards the user-facing
final recording name against a second run resolving to an existing file
(two runs of one quest finishing the same minute at the same time).
Best-effort against concurrent processes - the per-process temp token
already keeps their captures apart; this only tidies sequential
collisions - so the single writer that survives a race never silently
overwrites the earlier recording."
  (if (not (ignore-errors (probe-file path)))
      path
      (let* ((dot (position #\. path :from-end t))
             (stem (if dot (subseq path 0 dot) path))
             (ext (if dot (subseq path dot) "")))
        (loop :for n :from 2
              :for candidate := (format nil "~a (~d)~a" stem n ext)
              :unless (ignore-errors (probe-file candidate))
                :return candidate))))

(defun recording-tmp-path (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (namestring
     (merge-pathnames
      (format nil "rec-tmp-~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d-~a.mp4"
              year month day hour min sec (recording-token))
      (resolve-record-dir)))))

(defun run-video-filename (run)
  "\"Towards the Future 9'59.123 (2026-07-04 2130).mp4\" from a
completed run plist. Uses the in-game quest name (what the player
recognizes when browsing the folder), not the site slug."
  (multiple-value-bind (total-seconds msec) (floor (getf run :time-ms 0) 1000)
    (multiple-value-bind (minutes seconds) (floor total-seconds 60)
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (or (getf run :finished-at)
                                     (get-universal-time)))
        (declare (ignore sec))
        (sanitize-filename
         (format nil "~a ~d'~2,'0d.~3,'0d (~4,'0d-~2,'0d-~2,'0d ~2,'0d~2,'0d).mp4"
                 (or (getf run :quest-name) (getf run :quest-slug) "run")
                 minutes seconds msec year month day hour min))))))

(defun best-session-run (runs)
  "The run that names the video and receives it on the site: the
longest COMPLETED run, i.e. the full clear when segments completed
alongside it. Aborted runs are considered only when nothing completed:
their entries never upload the recording, so a completed segment must
win over the (slightly longer) aborted quest stay it was cut from -
e.g. a finished gdv-reset segment inside an abandoned GDV."
  (let ((completed (remove-if (lambda (run) (getf run :aborted)) runs)))
    (first (sort (copy-list (or completed runs)) #'>
                 :key (lambda (run) (getf run :time-ms 0))))))

(defun rect-covers-p (window monitor)
  "T when the WINDOW rect contains the whole MONITOR rect. Rects are
(left top right bottom) lists; pure so the tests can pin it. This is
the fullscreen test: an exclusive-fullscreen (or borderless) game
window spans its monitor exactly and the monitor capture needs no
crop; a smaller window gets a :CROP instead (CAPTURE-CROP-RECT)."
  (destructuring-bind (wl wt wr wb) window
    (destructuring-bind (ml mt mr mb) monitor
      (and (<= wl ml) (<= wt mt) (>= wr mr) (>= wb mb)))))

(defparameter +capture-crop-min-pixels+ 64
  "Smallest crop side worth recording. Below this the \"window\" is a
minimized or degenerate rect, and gdigrab of the real window - however
imperfect - beats a sliver of desktop.")

(defun capture-crop-rect (client-rect monitor-rect)
  "Where the game window's CLIENT-RECT sits on MONITOR-RECT, as crop
(values x y width height) in monitor-relative pixels for ffmpeg's crop
filter. Rects are (left top right bottom) screen-coordinate lists;
pure so the tests can pin it. The crop is clamped to the monitor (a
half-dragged-off window records its visible part) and the size floored
to even numbers (yuv420p subsampling). NIL when the visible part is
smaller than +CAPTURE-CROP-MIN-PIXELS+ a side."
  (destructuring-bind (cl ct cr cb) client-rect
    (destructuring-bind (ml mt mr mb) monitor-rect
      (let* ((left (max cl ml))
             (top (max ct mt))
             (width (* 2 (floor (- (min cr mr) left) 2)))
             (height (* 2 (floor (- (min cb mb) top) 2))))
        (when (and (>= width +capture-crop-min-pixels+)
                   (>= height +capture-crop-min-pixels+))
          (values (- left ml) (- top mt) width height))))))

;; ddagrab's output index is resolved by DXGI enumeration
;; (DXGI-OUTPUT-INDEX-FOR-DEVICE, ffmpeg-win32): the obvious
;; "DISPLAYn -> index n-1" guess captured a NEIGHBORING monitor on the
;; first multi-monitor field machine (run 1047 recorded the player's
;; Discord instead of the game), and a wrong monitor is a privacy
;; leak, so nothing may ever guess it again.

(defun build-ffmpeg-args (&key window-title output-path audio-pipe
                               capture-monitor video-encoder gpu-chain
                               low-memory
                               (framerate +record-framerate+))
  "ffmpeg argv (without the program itself). Fragmented MP4 keeps the
file playable even when ffmpeg is killed instead of quitting on \"q\".
With AUDIO-PIPE, raw 16-bit 48 kHz stereo game audio arrives on that
named pipe as a second input and is encoded as AAC. With
CAPTURE-MONITOR (a plist :output-idx :width :height [:crop], see
BACKEND-CAPTURE-MONITOR), the video comes from ddagrab (Desktop
Duplication): GDI cannot see an exclusive-fullscreen Direct3D surface,
nor a flip-model-composited window (run 949's all-black recordings on
Windows 11), so gdigrab - which remains only as the fallback when the
monitor cannot be resolved - records black there. A fullscreen window
covers its whole monitor, so the monitor capture IS the game; a
smaller window rides a :CROP (monitor-relative client area) cut before
the scale. With VIDEO-ENCODER (a +HW-ENCODER-CANDIDATES+ name), that
GPU encoder replaces libx264 at a VBR target; -bf 0 stays - the
B-frame reorder delay skewed A/V sync per player (run 92) regardless
of who encodes - and the encoder gets nv12 frames via the filter
chain, every vendor's native input. With GPU-CHAIN (QSV,
probe-verified: *HW-FULLSCREEN-GPU-CHAIN*) on a cropless (fullscreen)
capture, the frames never leave the GPU - hwmap hands ddagrab's D3D11
frames to a vpp_qsv sized from the monitor rect - dropping the
per-frame GPU->CPU copy and CPU scale that taxed weak machines; a
cropped capture keeps the hwdownload path (no crop_qsv in the bundled
build). With LOW-MEMORY (LOW-MEMORY-MACHINE-P), the bitrate/CRF drops
to the low-memory profile so recording churns less of a small
machine's file cache. Every variant converts RGB->YUV with an explicit
bt709 matrix at limited range and tags the frames
\(+RECORD-COLOR-TAGS-FILTER+): the untagged BT.601 that swscale
defaulted to played back through the browser's BT.709 assumption for
HD, shifting every saturated color on the site (run 1368)."
  (append
   (list "-y" "-loglevel" "error"
         ;; Minimal probing: ffmpeg opens the audio pipe right after
         ;; the video-input probe finishes, and the audio clock is
         ;; anchored to that pipe-connect instant (audio-win32).
         ;; Probing one frame instead of probesize-worth keeps video
         ;; time 0 and audio time 0 within a frame of each other.
         "-probesize" "32" "-analyzeduration" "0")
   (if capture-monitor
       ;; ddagrab is a lavfi source filter; it creates its own D3D11
       ;; device and outputs GPU frames at a constant FRAMERATE
       ;; (duplicating frames on static screens), which the -vf chain
       ;; either downloads for the CPU scale or maps straight to QSV.
       (list "-f" "lavfi"
             "-i" (format nil "ddagrab=output_idx=~d:framerate=~d:draw_mouse=0"
                          (getf capture-monitor :output-idx) framerate))
       (list "-f" "gdigrab"
             "-framerate" (princ-to-string framerate)
             "-draw_mouse" "0"
             "-i" (format nil "title=~a" window-title)))
   (when audio-pipe
     (list "-f" "s16le" "-ar" "48000" "-ac" "2"
           "-thread_queue_size" "1024"
           "-i" audio-pipe))
   (if video-encoder
       (list "-c:v" video-encoder
             "-b:v" (if low-memory +hw-record-bitrate-low+ +hw-record-bitrate+)
             "-maxrate" (if low-memory +hw-record-maxrate-low+ +hw-record-maxrate+)
             "-bufsize" (if low-memory +hw-record-bufsize-low+ +hw-record-bufsize+)
             "-bf" "0")
       (list "-c:v" "libx264"
             "-preset" +record-preset+
             "-threads" (princ-to-string (encoder-thread-count))
             "-crf" (princ-to-string (if low-memory +record-crf-low+ +record-crf+))
             ;; No B-frames: see the encoder-settings note. The fragmented
             ;; muxer would bake their reorder delay in as a video start
             ;; offset that browsers and local players interpret
             ;; differently, skewing A/V sync by 2 frames on the site.
             "-bf" "0"
             "-pix_fmt" "yuv420p"))
   (list "-vf"
         (let ((crop (and capture-monitor (getf capture-monitor :crop))))
           (concatenate
            'string
            (if (and capture-monitor (not crop) video-encoder gpu-chain)
                (multiple-value-bind (width height)
                    (record-scale-dimensions (getf capture-monitor :width)
                                             (getf capture-monitor :height))
                  ;; vpp_qsv, not scale_qsv: same VPP underneath, but
                  ;; only the full filter exposes the explicit color
                  ;; conversion (see the color note; the chain probe
                  ;; verifies these exact options).
                  (format nil "hwmap=derive_device=qsv,vpp_qsv=w=~d:h=~d:format=nv12:out_color_matrix=bt709:out_range=tv"
                          width height))
                (let ((base (cond
                              (crop
                               ;; The window's client area, cut out of the
                               ;; monitor frame before the scale sees it.
                               (destructuring-bind (x y width height) crop
                                 (format nil "hwdownload,format=bgra,crop=~d:~d:~d:~d,~a"
                                         width height x y
                                         (record-scale-filter))))
                              (capture-monitor
                               (format nil "hwdownload,format=bgra,~a"
                                       (record-scale-filter)))
                              (t (record-scale-filter)))))
                  ;; The YUV format directly after the scale makes the
                  ;; scale itself the RGB->YUV conversion, so its
                  ;; out_color_matrix applies (the x264 path's -pix_fmt
                  ;; alone would leave it to an untagged auto-insert).
                  (concatenate 'string base
                               (if video-encoder ",format=nv12" ",format=yuv420p"))))
            ","
            +record-color-tags-filter+)))
   (when audio-pipe
     ;; NO -af here, and in particular no loudnorm: its multi-second
     ;; lookahead makes the audio output lag the video permanently,
     ;; and ffmpeg's A/V interleaving then throttles the video path -
     ;; field-measured at 17 fps (vs 29 without) with second-long
     ;; freezes. Loudness normalization happens offline in
     ;; BUILD-REMUX-ARGS instead, where latency costs nothing.
     (list "-c:a" "aac" "-b:a" "160k"))
   (list "-movflags" "+frag_keyframe+empty_moov"
         output-path)))

(defparameter +record-loudness-lufs+ -24
  "loudnorm integrated-loudness target for finished recordings. The
initial -16 (streaming-platform level) played back too loud for the
first party testers (issue 84), especially with several perspectives
of one attempt open at once; -20 helped but was still reported too
loud, so -24 drops another ~4 dB. Kept above the -70..-24 floor where
loudnorm starts fighting to raise near-silent captures, so quiet
recordings stay audible while sitting well below typical web video.")

(defun build-remux-args (input-path output-path)
  "ffmpeg argv rewriting the fragmented recording as a regular MP4 with
the moov (duration + seek index) at the front. Video is stream-copied;
audio is loudness-normalized here, NOT during capture (loudnorm's
lookahead throttled the live pipeline, see BUILD-FFMPEG-ARGS): loopback
capture is post-mixer, so a low per-app volume slider (observed at 5%
in the field) would otherwise leave recordings inaudible. loudnorm
outputs 192 kHz internally; resample back down. NO timestamp games
here: an -itsoffset shift died in browsers (leading negative pts,
run 88), and the 67 ms atrim that replaced it turned out to be
compensating the B-frame video start offset that only SOME players
honored - that offset is gone at the source now (-bf 0 in
BUILD-FFMPEG-ARGS), so the streams need no correction. The offline
pass runs far faster than real time."
  (list "-y" "-loglevel" "error"
        "-i" input-path
        "-c:v" "copy"
        "-af" (format nil "loudnorm=I=~d:TP=-1.5:LRA=11,aresample=48000"
                      +record-loudness-lufs+)
        "-c:a" "aac" "-b:a" "160k"
        "-movflags" "+faststart"
        output-path))

(defun remove-subseq (list subseq)
  "LIST without the first occurrence of the consecutive SUBSEQ."
  (let ((position (search subseq list :test #'equal)))
    (if position
        (append (subseq list 0 position)
                (nthcdr (+ position (length subseq)) list))
        list)))

(defun strip-audio-args (args audio-pipe)
  "ARGS without the audio input/codec arguments BUILD-FFMPEG-ARGS added
for AUDIO-PIPE - the video-only fallback when the audio session cannot
start (the pipe would never be served, and ffmpeg would hang opening it)."
  (remove-subseq
   (remove-subseq args (list "-f" "s16le" "-ar" "48000" "-ac" "2"
                             "-thread_queue_size" "1024" "-i" audio-pipe))
   (list "-c:a" "aac" "-b:a" "160k")))

(defun retarget-audio-args (args &key sample-format rate channels)
  "ARGS with the audio input's placeholder format tokens replaced by
the session's actual capture format (the endpoint mix format is only
known once the audio session is activated)."
  (let ((position (position "s16le" args :test #'equal)))
    (if (not position)
        args
        (let ((new (copy-list args)))
          ;; ... "-f" "s16le" "-ar" "48000" "-ac" "2" ...
          (setf (nth position new) sample-format
                (nth (+ position 2) new) (princ-to-string rate)
                (nth (+ position 4) new) (princ-to-string channels))
          new))))

;;; The recorder state machine

(defstruct recorder
  backend              ; capture-backend protocol object
  (state :idle)        ; :idle | :recording | :stopping | :remuxing
  capture              ; backend token while :recording / :stopping
  capture-start-real   ; internal real time when the capture started
  tmp-path             ; file ffmpeg is writing (namestring)
  session-runs         ; runs completed during this capture
  (last-detector-state :idle)
  stop-deadline        ; internal real time to give up on "q" and kill
  pending-keep-p       ; decided when the stop begins
  pending-run          ; the kept file's best run, for ON-KEEP
  final-path           ; remux/rename target when keeping
  remux-capture        ; backend token while :remuxing
  remux-deadline       ; internal real time to give up on the remux
  on-keep              ; (lambda (final-path run)) after a successful save
  last-error)          ; string for the GUI, or NIL

(defvar *capture-failure-notified* nil
  "T once the tray has warned about the current streak of capture-start
failures; reset by the next successful start. One balloon per streak,
not per run - a Smart App Control block can last hours.")

(defvar *software-fallback-notified* nil
  "T once the tray has warned that captures run on libx264 because the
encoder probe never got to run (ffmpeg blocked at startup). Once per
process: machines that genuinely lack a hardware encoder (probe state
:DONE) are never nagged.")

(defun notify-user (title text &key (icon :warning))
  "Tray balloon when the tray code is loaded (tray-win32.lisp,
LispWorks-only, loads after this file) - a silent no-op on SBCL test
runs, where TRAY-NOTIFY is never defined."
  (when (fboundp 'tray-notify)
    (ignore-errors (funcall 'tray-notify title text :icon icon))))

(defun start-recording (recorder window-title)
  ;; A :SPAWN-FAILED probe verdict is provisional (see the defvar):
  ;; ffmpeg may have been unblocked since startup, so retry the probe
  ;; off-thread each time a capture starts. This capture still uses the
  ;; current verdict; the next one picks up whatever the re-probe finds.
  (when (and (config-value :hw-encode)
             (eq *hw-encoder-probe-state* :spawn-failed))
    ;; FUNCALL: defined in ffmpeg-win32.lisp, which loads after this
    ;; file (and only on LispWorks, like every path reaching here).
    (funcall 'start-hw-encoder-probe))
  (let* ((ffmpeg (resolve-ffmpeg-path))
         (output (recording-tmp-path))
         (audio-pid (and (config-value :record-audio) *audio-target-pid*))
         (audio-pipe (and audio-pid (audio-pipe-name)))
         (encoder (and (config-value :hw-encode) *hw-video-encoder*))
         (args (build-ffmpeg-args :window-title window-title
                                  :output-path output
                                  :audio-pipe audio-pipe
                                  :capture-monitor
                                  (backend-capture-monitor
                                   (recorder-backend recorder))
                                  :video-encoder encoder
                                  :gpu-chain
                                  (and (equal encoder "h264_qsv")
                                       *hw-fullscreen-gpu-chain*)
                                  :low-memory (low-memory-machine-p))))
    (multiple-value-bind (capture error)
        (backend-start-capture (recorder-backend recorder) ffmpeg args output
                               :audio-pipe audio-pipe :audio-pid audio-pid)
      (if capture
          (progn
            (setf (recorder-capture recorder) capture
                  (recorder-capture-start-real recorder) (get-internal-real-time)
                  (recorder-tmp-path recorder) output
                  (recorder-session-runs recorder) '()
                  (recorder-last-error recorder) nil
                  (recorder-state recorder) :recording
                  *capture-failure-notified* nil)
            ;; The capture spawned but on libx264 only because the
            ;; startup probe never got to ask the hardware - tell the
            ;; user why the game may feel heavy (field case 2026-07-18).
            (when (and (config-value :hw-encode)
                       (null encoder)
                       (eq *hw-encoder-probe-state* :spawn-failed)
                       (not *software-fallback-notified*))
              (setf *software-fallback-notified* t)
              (notify-user (tr :notify-software-encode-title)
                           (tr :notify-software-encode-text)
                           :icon :info)))
          ;; Stay :idle; the edge trigger retries on the next quest.
          (let ((message (or error "could not start ffmpeg")))
            (setf (recorder-last-error recorder) message)
            (unless *capture-failure-notified*
              (setf *capture-failure-notified* t)
              (notify-user (tr :notify-capture-failed-title)
                           ;; 4551 = "an Application Control policy has
                           ;; blocked this file": tell the user the fix
                           ;; is in Windows Security, not the client.
                           (tr (if (search "(Windows error 4551)" message)
                                   :notify-capture-blocked-text
                                   :notify-capture-failed-text)))))))))

(defun annotate-video-offsets (recorder runs)
  "Stamp each of RUNS (completed this poll frame) with :VIDEO-OFFSET-MS,
the video timestamp where the run's timer started: capture elapsed at
completion minus the run's own duration. The site's telemetry timeline
seeks the hosted video with this (video_offset_ms); without it a death
at quest time T seeks to video time T, missing by however long the
capture ran before the start trigger. NCONC, not (SETF GETF): the run
plist is shared with the submission queue, which must see the key.
Accuracy is ~the ffmpeg spin-up (a few hundred ms, video time 0 is the
first grabbed frame); the run page's manual sync form can refine it."
  (let ((start (recorder-capture-start-real recorder)))
    (when start
      (let ((elapsed-ms (round (* 1000 (- (get-internal-real-time) start))
                               internal-time-units-per-second)))
        (dolist (run runs)
          (let ((offset (- elapsed-ms (getf run :time-ms 0))))
            (when (and (>= offset 0)
                       (null (getf run :video-offset-ms)))
              (nconc run (list :video-offset-ms offset)))))))))

(defun begin-stop (recorder)
  "Ask ffmpeg to finish and decide the file's fate: keep it under the
best completed run's name, or delete it when nothing completed."
  (let ((best (best-session-run (recorder-session-runs recorder))))
    (setf (recorder-pending-keep-p recorder) (and best t)
          (recorder-pending-run recorder) best
          (recorder-final-path recorder)
          (and best (deduplicate-path
                     (namestring (merge-pathnames (run-video-filename best)
                                                  (resolve-record-dir)))))
          (recorder-stop-deadline recorder)
          (+ (get-internal-real-time)
             (* +stop-grace-seconds+ internal-time-units-per-second))
          (recorder-state recorder) :stopping)
    (ignore-errors (backend-request-stop (recorder-backend recorder)
                                         (recorder-capture recorder)))))

(defun reset-recorder (recorder)
  "Back to :idle with every per-capture field cleared."
  (setf (recorder-capture recorder) nil
        (recorder-capture-start-real recorder) nil
        (recorder-tmp-path recorder) nil
        (recorder-session-runs recorder) '()
        (recorder-pending-keep-p recorder) nil
        (recorder-pending-run recorder) nil
        (recorder-final-path recorder) nil
        (recorder-remux-capture recorder) nil
        (recorder-remux-deadline recorder) nil
        (recorder-stop-deadline recorder) nil
        (recorder-state recorder) :idle))

(defun finalize-capture (recorder)
  "The recording process is dead: release handles, then remux a kept
file (or delete an abandoned one)."
  (let ((backend (recorder-backend recorder)))
    (ignore-errors (backend-close-capture backend (recorder-capture recorder)))
    (if (and (recorder-pending-keep-p recorder) (recorder-final-path recorder))
        (begin-remux recorder)
        (progn
          (ignore-errors
            (backend-delete-file backend (recorder-tmp-path recorder)))
          (reset-recorder recorder)))))

(defun begin-remux (recorder)
  "Start the background ffmpeg rewriting the kept fragmented file into
FINAL-PATH; SAVE-RECORDING runs once it exits. When ffmpeg cannot even
start, keep the fragmented original rather than lose the recording."
  (multiple-value-bind (capture error)
      (backend-start-remux (recorder-backend recorder)
                           (resolve-ffmpeg-path)
                           (build-remux-args (recorder-tmp-path recorder)
                                             (recorder-final-path recorder)))
    (declare (ignore error))
    (if capture
        (setf (recorder-remux-capture recorder) capture
              (recorder-remux-deadline recorder)
              (+ (get-internal-real-time)
                 (* +remux-grace-seconds+ internal-time-units-per-second))
              (recorder-state recorder) :remuxing)
        (save-recording recorder :remuxed nil))))

(defun finish-remux (recorder)
  "The remux process is dead: keep its output when it exited cleanly,
else drop the partial output and fall back to the fragmented original."
  (let* ((backend (recorder-backend recorder))
         (capture (recorder-remux-capture recorder))
         (ok (ignore-errors (backend-capture-succeeded-p backend capture))))
    (ignore-errors (backend-close-capture backend capture))
    (unless ok
      (ignore-errors
        (backend-delete-file backend (recorder-final-path recorder))))
    (save-recording recorder :remuxed ok)))

(defun save-recording (recorder &key remuxed)
  "Put the final file in place - the remux already wrote FINAL-PATH
when REMUXED (the tmp only needs deleting), otherwise the fragmented
tmp is renamed onto it - and hand it to ON-KEEP."
  (handler-case
      (progn
        (if remuxed
            (ignore-errors
              (backend-delete-file (recorder-backend recorder)
                                   (recorder-tmp-path recorder)))
            (backend-rename-file (recorder-backend recorder)
                                 (recorder-tmp-path recorder)
                                 (recorder-final-path recorder)))
        (when (recorder-on-keep recorder)
          (ignore-errors
            (funcall (recorder-on-keep recorder)
                     (recorder-final-path recorder)
                     (recorder-pending-run recorder)))))
    (error (condition)
      (setf (recorder-last-error recorder)
            (format nil "could not save recording: ~a" condition))))
  (reset-recorder recorder))

(defun abort-capture (recorder message)
  "ffmpeg died on its own mid-capture: clean up and surface the error."
  (let ((backend (recorder-backend recorder)))
    (ignore-errors (backend-close-capture backend (recorder-capture recorder)))
    (ignore-errors (backend-delete-file backend (recorder-tmp-path recorder))))
  (reset-recorder recorder)
  (setf (recorder-last-error recorder) message))

(defun recorder-step (recorder detector-state completed-runs window-title)
  "Feed one poll frame. COMPLETED-RUNS is DETECTOR-STEP's return value.
Runs are accumulated BEFORE the stop check because the detector flips
to :idle on the very frame the full clear completes."
  (when (and completed-runs
             (member (recorder-state recorder) '(:recording :stopping)))
    ;; The poll loop enqueues these same plists right after this call,
    ;; so the offsets stamped here travel with the submission.
    (annotate-video-offsets recorder completed-runs)
    (setf (recorder-session-runs recorder)
          (append (recorder-session-runs recorder) completed-runs)))
  (ecase (recorder-state recorder)
    (:idle
     ;; Edge-triggered: only a fresh :idle -> :in-quest transition starts
     ;; a capture, so a failed start or a mid-run toggle never produces a
     ;; partial video of a valid run.
     (when (and (eq detector-state :in-quest)
                (not (eq (recorder-last-detector-state recorder) :in-quest))
                (config-value :record-enabled)
                window-title)
       (start-recording recorder window-title)))
    (:recording
     (cond
       ((not (backend-capture-alive-p (recorder-backend recorder)
                                      (recorder-capture recorder)))
        (abort-capture recorder "ffmpeg exited unexpectedly"))
       ((eq detector-state :idle)
        (begin-stop recorder))))
    (:stopping
     (cond
       ((not (backend-capture-alive-p (recorder-backend recorder)
                                      (recorder-capture recorder)))
        (finalize-capture recorder))
       ((and (recorder-stop-deadline recorder)
             (>= (get-internal-real-time) (recorder-stop-deadline recorder)))
        ;; "q" did not work; the fragmented MP4 survives the kill.
        (ignore-errors (backend-kill-capture (recorder-backend recorder)
                                             (recorder-capture recorder)))
        (setf (recorder-stop-deadline recorder) nil))))
    (:remuxing
     (cond
       ((not (backend-capture-alive-p (recorder-backend recorder)
                                      (recorder-remux-capture recorder)))
        (finish-remux recorder))
       ((and (recorder-remux-deadline recorder)
             (>= (get-internal-real-time) (recorder-remux-deadline recorder)))
        ;; Runaway remux: kill it; the next frame falls back to the
        ;; fragmented original via FINISH-REMUX.
        (ignore-errors (backend-kill-capture (recorder-backend recorder)
                                             (recorder-remux-capture recorder)))
        (setf (recorder-remux-deadline recorder) nil)))))
  (setf (recorder-last-detector-state recorder) detector-state)
  recorder)

(defun wait-for-capture (backend capture timeout)
  "Poll CAPTURE until it exits or TIMEOUT seconds pass; kill it when
the deadline hits."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop :while (and (backend-capture-alive-p backend capture)
                      (< (get-internal-real-time) deadline))
          :do (sleep 0.05))
    (when (backend-capture-alive-p backend capture)
      (ignore-errors (backend-kill-capture backend capture)))))

(defun recorder-shutdown (recorder &key (timeout +stop-grace-seconds+))
  "Client is exiting: finish any capture in progress, waiting up to
TIMEOUT seconds for a graceful stop before killing ffmpeg, then up to
TIMEOUT more for the remux of a kept file."
  (when (eq (recorder-state recorder) :recording)
    (begin-stop recorder))
  (when (eq (recorder-state recorder) :stopping)
    (wait-for-capture (recorder-backend recorder)
                      (recorder-capture recorder) timeout)
    (finalize-capture recorder))
  (when (eq (recorder-state recorder) :remuxing)
    (wait-for-capture (recorder-backend recorder)
                      (recorder-remux-capture recorder) timeout)
    (finish-remux recorder)))

(defun cleanup-stale-recordings (recorder)
  "Delete rec-tmp-*.mp4 left behind by a crash of a previous session."
  (let ((backend (recorder-backend recorder)))
    (dolist (path (ignore-errors
                    (backend-list-stale-files backend (resolve-record-dir))))
      (ignore-errors (backend-delete-file backend path)))))

;;; Local storage budget. Recordings pile up - a reset-farm can add a
;;; hundred quest videos in a day at ~21 MB/minute - and nothing ever
;;; reclaimed them, so the folder grew without bound (see the config
;;; note on :RECORD-MAX-TOTAL-GB). This mirrors the server's hosted
;;; retention on the client: once the folder crosses the cap, delete the
;;; oldest videos until it is back under, uploaded ones first (a copy
;;; reached the site) and never a file the game is still writing or one
;;; that has not made it to the server yet.

(defun recordings-to-evict (files cap-bytes &key protected uploaded)
  "Which of FILES to delete so their total drops to CAP-BYTES. FILES is
a list of (namestring size-bytes write-date). PROTECTED namestrings are
never returned (the live capture, and files still awaiting upload - the
only copy the leaderboard submit can use). UPLOADED namestrings are
evicted before anything else, since the site already holds a copy;
within each tier the oldest (smallest write-date) go first. Pure, so
the tests pin it. Returns the namestrings to delete, in deletion order;
NIL when the cap is unset or the folder is already under it."
  (when (and cap-bytes (plusp cap-bytes))
    (let ((total (reduce #'+ files :key #'second :initial-value 0)))
      (when (> total cap-bytes)
        (let* ((candidates (remove-if (lambda (file)
                                        (member (first file) protected
                                                :test #'equal))
                                      files))
               (by-age (sort (copy-list candidates) #'< :key #'third))
               ;; Uploaded files ahead of the rest, ages preserved within
               ;; each tier (BY-AGE is already oldest-first, STABLE-SORT
               ;; keeps that order among equals).
               (ordered (stable-sort
                         (copy-list by-age)
                         (lambda (a b)
                           (and (member (first a) uploaded :test #'equal)
                                (not (member (first b) uploaded
                                             :test #'equal)))))))
          (loop :for file :in ordered
                :while (> total cap-bytes)
                :collect (first file)
                :do (decf total (second file))))))))

(defun record-max-total-bytes ()
  "The recordings-folder cap in bytes from :RECORD-MAX-TOTAL-GB, or NIL
when it is unset/zero (unlimited)."
  (let ((gb (config-value :record-max-total-gb)))
    (when (and (numberp gb) (plusp gb))
      (round (* gb 1024 1024 1024)))))

;; APPLY-RECORDING-RETENTION drives this from the run queue and lives in
;; store.lisp (the queue owner, loaded after this file).
