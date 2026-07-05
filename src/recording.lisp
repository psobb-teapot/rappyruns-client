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
;;; remuxed (stream copy) into a regular MP4 with the moov up front:
;;; fragmented MP4 carries no duration/seek index, so browsers playing
;;; the hosted video would show a seekbar that grows as it loads. The
;;; fragmented original is kept as-is when the remux fails.

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

(defgeneric backend-list-stale-files (backend dir)
  (:documentation "Leftover rec-tmp-*.mp4 files from a previous session."))

;;; Encoder settings. Not user-facing config: veryfast/23 costs a few
;;; percent CPU at PSOBB resolutions and the resulting quality is fine
;;; for run verification.

(defparameter +record-framerate+ 30)
(defparameter +record-preset+ "veryfast")
(defparameter +record-crf+ 23)

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

(defparameter +remux-grace-seconds+ 60
  "How long the post-capture remux may run before it is killed and the
fragmented original is kept instead. Stream copy is disk-bound, so even
an hour-long recording finishes in seconds; the margin covers a slow
HDD under load.")

;;; Paths and filenames

(defun resolve-record-dir ()
  "Config :record-dir, or <user home>/Videos/EphineaTA/ when blank."
  (let ((configured (string-trim " " (or (config-value :record-dir) ""))))
    (if (plusp (length configured))
        (uiop:ensure-directory-pathname configured)
        (let ((home (or (uiop:getenv "USERPROFILE")
                        (namestring (user-homedir-pathname)))))
          (merge-pathnames "Videos/EphineaTA/"
                           (uiop:ensure-directory-pathname home))))))

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

(defun recording-tmp-path (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (namestring
     (merge-pathnames
      (format nil "rec-tmp-~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d.mp4"
              year month day hour min sec)
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
  "The run that names the video: longest time-ms, i.e. the full clear
when segments completed alongside it."
  (first (sort (copy-list runs) #'>
               :key (lambda (run) (getf run :time-ms 0)))))

(defun build-ffmpeg-args (&key window-title output-path audio-pipe
                               (framerate +record-framerate+))
  "ffmpeg argv (without the program itself). Fragmented MP4 keeps the
file playable even when ffmpeg is killed instead of quitting on \"q\".
With AUDIO-PIPE, raw 16-bit 48 kHz stereo game audio arrives on that
named pipe as a second input and is encoded as AAC."
  (append
   (list "-y" "-loglevel" "error"
         "-f" "gdigrab"
         "-framerate" (princ-to-string framerate)
         "-draw_mouse" "0"
         "-i" (format nil "title=~a" window-title))
   (when audio-pipe
     (list "-f" "s16le" "-ar" "48000" "-ac" "2"
           "-thread_queue_size" "1024"
           "-i" audio-pipe))
   (list "-c:v" "libx264"
         "-preset" +record-preset+
         "-crf" (princ-to-string +record-crf+)
         "-pix_fmt" "yuv420p")
   (when audio-pipe
     ;; Loopback capture is post-mixer: a low per-app volume slider
     ;; (observed at 5% in the field) makes the raw capture inaudible.
     ;; Loudness normalization brings every recording to a consistent,
     ;; audible level regardless of the player's mixer settings. The
     ;; mix is float, so boosting quiet captures loses nothing.
     ;; (loudnorm outputs 192 kHz internally; resample back down.)
     (list "-af" "loudnorm=I=-16:TP=-1.5:LRA=11,aresample=48000"
           "-c:a" "aac" "-b:a" "160k"))
   (list "-movflags" "+frag_keyframe+empty_moov"
         output-path)))

(defun build-remux-args (input-path output-path)
  "ffmpeg argv rewriting the fragmented recording as a regular MP4 with
the moov (duration + seek index) at the front. Stream copy - no
re-encode - so this takes seconds even for long captures."
  (list "-y" "-loglevel" "error"
        "-i" input-path
        "-c" "copy"
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
   (list "-af" "loudnorm=I=-16:TP=-1.5:LRA=11,aresample=48000"
         "-c:a" "aac" "-b:a" "160k")))

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

(defun start-recording (recorder window-title)
  (let* ((ffmpeg (resolve-ffmpeg-path))
         (output (recording-tmp-path))
         (audio-pid (and (config-value :record-audio) *audio-target-pid*))
         (audio-pipe (and audio-pid (audio-pipe-name)))
         (args (build-ffmpeg-args :window-title window-title
                                  :output-path output
                                  :audio-pipe audio-pipe)))
    (multiple-value-bind (capture error)
        (backend-start-capture (recorder-backend recorder) ffmpeg args output
                               :audio-pipe audio-pipe :audio-pid audio-pid)
      (if capture
          (setf (recorder-capture recorder) capture
                (recorder-tmp-path recorder) output
                (recorder-session-runs recorder) '()
                (recorder-last-error recorder) nil
                (recorder-state recorder) :recording)
          ;; Stay :idle; the edge trigger retries on the next quest.
          (setf (recorder-last-error recorder)
                (or error "could not start ffmpeg"))))))

(defun begin-stop (recorder)
  "Ask ffmpeg to finish and decide the file's fate: keep it under the
best completed run's name, or delete it when nothing completed."
  (let ((best (best-session-run (recorder-session-runs recorder))))
    (setf (recorder-pending-keep-p recorder) (and best t)
          (recorder-pending-run recorder) best
          (recorder-final-path recorder)
          (and best (namestring (merge-pathnames (run-video-filename best)
                                                 (resolve-record-dir))))
          (recorder-stop-deadline recorder)
          (+ (get-internal-real-time)
             (* +stop-grace-seconds+ internal-time-units-per-second))
          (recorder-state recorder) :stopping)
    (ignore-errors (backend-request-stop (recorder-backend recorder)
                                         (recorder-capture recorder)))))

(defun reset-recorder (recorder)
  "Back to :idle with every per-capture field cleared."
  (setf (recorder-capture recorder) nil
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
