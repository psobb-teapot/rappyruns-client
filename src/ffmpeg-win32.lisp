(in-package :ephinea-ta-client)

;;; LispWorks-only: the live capture backend. Spawns ffmpeg.exe with
;;; CreateProcessW, holding a pipe to its stdin so a single "q" stops it
;;; gracefully (flushing the final MP4 fragment); TerminateProcess is
;;; the fallback. kernel32 is registered in win32.lisp.

(fli:define-c-struct security-attributes
  (nlength (:unsigned :long))
  (security-descriptor :pointer)
  (inherit-handle (:boolean :int)))

(fli:define-c-struct startupinfo-w
  (cb (:unsigned :long))
  (reserved :pointer)
  (desktop :pointer)
  (title :pointer)
  (x (:unsigned :long))
  (y (:unsigned :long))
  (x-size (:unsigned :long))
  (y-size (:unsigned :long))
  (x-count-chars (:unsigned :long))
  (y-count-chars (:unsigned :long))
  (fill-attribute (:unsigned :long))
  (flags (:unsigned :long))
  (show-window (:unsigned :short))
  (cb-reserved2 (:unsigned :short))
  (reserved2 :pointer)
  (std-input :pointer)
  (std-output :pointer)
  (std-error :pointer))

(fli:define-c-struct process-information
  (process :pointer)
  (thread :pointer)
  (pid (:unsigned :long))
  (tid (:unsigned :long)))

(fli:define-foreign-function (%create-pipe "CreatePipe")
    ((read-handle (:reference-return :pointer))
     (write-handle (:reference-return :pointer))
     (pipe-attributes (:pointer (:struct security-attributes)))
     (size (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%set-handle-information "SetHandleInformation")
    ((handle :pointer)
     (mask (:unsigned :long))
     (flags (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%create-process "CreateProcessW")
    ((application-name :pointer)
     (command-line :pointer)      ; writable WCHAR buffer, per the API
     (process-attributes :pointer)
     (thread-attributes :pointer)
     (inherit-handles (:boolean :int))
     (creation-flags (:unsigned :long))
     (environment :pointer)
     (current-directory :pointer)
     (startup-info (:pointer (:struct startupinfo-w)))
     (process-information (:pointer (:struct process-information))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

;; %write-file lives in win32.lisp (shared with the audio pipe).

(fli:define-foreign-function (%terminate-process "TerminateProcess")
    ((process :pointer)
     (exit-code (:unsigned :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defparameter +audio-drain-seconds+ 3
  "After the audio pipe EOF, how long ffmpeg gets to drain the buffered
audio tail before \"q\" stops it reading.")

(defconstant +startf-usestdhandles+ #x100)
(defconstant +create-no-window+ #x08000000)
(defconstant +below-normal-priority-class+ #x4000)
(defconstant +handle-flag-inherit+ 1)

;;; Windows command lines are one string; arguments must be quoted per
;;; the CommandLineToArgvW rules (the gdigrab title= argument contains
;;; spaces, and paths may contain spaces and quotes).

(defun quote-windows-arg (arg)
  (if (and (plusp (length arg))
           (notany (lambda (char) (find char '(#\Space #\Tab #\"))) arg))
      arg
      (with-output-to-string (out)
        (write-char #\" out)
        (let ((backslashes 0))
          (loop :for char :across arg
                :do (case char
                      (#\\ (incf backslashes))
                      (#\"
                       ;; Backslashes before a quote must be doubled,
                       ;; plus one to escape the quote itself.
                       (dotimes (i (1+ (* 2 backslashes)))
                         (write-char #\\ out))
                       (setf backslashes 0)
                       (write-char #\" out))
                      (t
                       (dotimes (i backslashes) (write-char #\\ out))
                       (setf backslashes 0)
                       (write-char char out))))
          ;; Trailing backslashes must not escape the closing quote.
          (dotimes (i (* 2 backslashes)) (write-char #\\ out)))
        (write-char #\" out))))

(defun argv->command-line (program args)
  (format nil "~{~a~^ ~}" (mapcar #'quote-windows-arg (cons program args))))

;;; Spawning

(defstruct ffmpeg-capture
  process-handle
  thread-handle
  stdin-write   ; our end of the child's stdin pipe
  pid
  audio)        ; audio-session (audio-win32.lisp) or NIL

(defun create-stdin-pipe ()
  "An anonymous pipe whose read end the child may inherit as stdin.
Returns (values read-end write-end)."
  (fli:with-dynamic-foreign-objects ((attributes (:struct security-attributes)))
    (setf (fli:foreign-slot-value attributes 'nlength)
          (fli:size-of '(:struct security-attributes))
          (fli:foreign-slot-value attributes 'security-descriptor)
          fli:*null-pointer*
          (fli:foreign-slot-value attributes 'inherit-handle) t)
    (multiple-value-bind (ok read-end write-end)
        (%create-pipe 0 0 attributes 0)
      (unless ok
        (error "CreatePipe failed (Windows error ~d)" (%get-last-error)))
      ;; CreatePipe made both ends inheritable; only the child's may be.
      (%set-handle-information write-end +handle-flag-inherit+ 0)
      (values read-end write-end))))

(defun zero-startupinfo (startup)
  (setf (fli:foreign-slot-value startup 'cb)
        (fli:size-of '(:struct startupinfo-w)))
  (dolist (pointer-slot '(reserved desktop title reserved2
                          std-input std-output std-error))
    (setf (fli:foreign-slot-value startup pointer-slot) fli:*null-pointer*))
  (dolist (dword-slot '(x y x-size y-size x-count-chars y-count-chars
                        fill-attribute flags show-window cb-reserved2))
    (setf (fli:foreign-slot-value startup dword-slot) 0)))

(defun spawn-process (program args)
  "CreateProcessW PROGRAM with ARGS, stdin wired to a pipe we keep.
Returns an FFMPEG-CAPTURE; signals an error on failure. A bare PROGRAM
name (e.g. \"ffmpeg.exe\") is searched on PATH by the API.
The child runs at below-normal priority: every process spawned here is
an ffmpeg (capture, remux, -version probe) sharing the machine with a
live game, and x264 at normal priority visibly stole frame time from
PSOBB. Windows still gives ffmpeg the leftover cores, so a capture
keeps up whenever the machine has headroom at all."
  (multiple-value-bind (stdin-read stdin-write) (create-stdin-pipe)
    (handler-case
        (fli:with-dynamic-foreign-objects
            ((startup (:struct startupinfo-w))
             (process-info (:struct process-information)))
          (zero-startupinfo startup)
          (setf (fli:foreign-slot-value startup 'flags) +startf-usestdhandles+
                (fli:foreign-slot-value startup 'std-input) stdin-read)
          (let ((ok (fli:with-foreign-string (command element-count byte-count
                                              :external-format :unicode)
                        (argv->command-line program args)
                      (declare (ignore element-count byte-count))
                      (%create-process fli:*null-pointer* command
                                       fli:*null-pointer* fli:*null-pointer*
                                       t (logior +create-no-window+
                                                 +below-normal-priority-class+)
                                       fli:*null-pointer* fli:*null-pointer*
                                       startup process-info))))
            (unless ok
              (error "could not start ~a (Windows error ~d)"
                     program (%get-last-error)))
            ;; The child inherited its own stdin-read; drop our copy.
            (%close-handle stdin-read)
            (make-ffmpeg-capture
             :process-handle (fli:foreign-slot-value process-info 'process)
             :thread-handle (fli:foreign-slot-value process-info 'thread)
             :stdin-write stdin-write
             :pid (fli:foreign-slot-value process-info 'pid))))
      (error (condition)
        (%close-handle stdin-read)
        (%close-handle stdin-write)
        (error condition)))))

(defun close-capture-handles (capture)
  (%close-handle (ffmpeg-capture-stdin-write capture))
  (%close-handle (ffmpeg-capture-thread-handle capture))
  (%close-handle (ffmpeg-capture-process-handle capture)))

(defun capture-alive-p (capture)
  (multiple-value-bind (ok code)
      (%get-exit-code-process (ffmpeg-capture-process-handle capture) 0)
    (and ok (= code +still-active+))))

(defun ffmpeg-available-p ()
  "T when RESOLVE-FFMPEG-PATH points at something that starts. Used by
the GUI to validate the recording checkbox; -version exits on its own."
  (handler-case
      (let ((capture (spawn-process (resolve-ffmpeg-path) (list "-version"))))
        (close-capture-handles capture)
        t)
    (error () nil)))

;;; Hardware-encoder probe. Runs once, in the background, at startup;
;;; the winner lands in *HW-VIDEO-ENCODER* (recording.lisp) and every
;;; later capture uses it. Probing at capture start instead would
;;; delay the recording past the run's start trigger.

(defparameter +hw-probe-timeout-seconds+ 15
  "Per-encoder cap on the probe encode. A usable encoder finishes the
8 black frames in well under a second; an unusable one fails to open
even faster - the margin only covers a cold ffmpeg start under load.")

(defun probe-hw-encoder ()
  "First +HW-ENCODER-CANDIDATES+ entry this machine's ffmpeg can
actually encode with, or NIL when only libx264 remains."
  (let ((ffmpeg (resolve-ffmpeg-path))
        (backend (make-instance 'win32-ffmpeg-backend)))
    (dolist (encoder +hw-encoder-candidates+)
      (handler-case
          (let ((capture (spawn-process ffmpeg
                                        (hw-encoder-probe-args encoder))))
            (unwind-protect
                 (progn
                   (wait-for-capture backend capture
                                     +hw-probe-timeout-seconds+)
                   (when (backend-capture-succeeded-p backend capture)
                     (return encoder)))
              (backend-close-capture backend capture)))
        (error (condition)
          (recording-log "hw encoder probe ~a failed to spawn: ~a"
                         encoder condition)
          nil)))))

(defun probe-hw-gpu-chain ()
  "T when the zero-copy fullscreen capture chain works on this machine
\(see HW-GPU-CHAIN-PROBE-ARGS). Only worth asking once the encoder
probe picked h264_qsv; the probe grabs a few frames of the primary
desktop, which every interactive session allows."
  (let ((backend (make-instance 'win32-ffmpeg-backend)))
    (handler-case
        (let ((capture (spawn-process (resolve-ffmpeg-path)
                                      (hw-gpu-chain-probe-args))))
          (unwind-protect
               (progn
                 (wait-for-capture backend capture
                                   +hw-probe-timeout-seconds+)
                 (backend-capture-succeeded-p backend capture))
            (backend-close-capture backend capture)))
      (error (condition)
        (recording-log "gpu chain probe failed to spawn: ~a" condition)
        nil))))

(defun start-hw-encoder-probe ()
  "Probe in a worker thread and publish the result. Called once from
MAIN; until it lands, captures fall back to libx264 (see
*HW-VIDEO-ENCODER*). A QSV winner is probed further for the zero-copy
fullscreen chain (*HW-FULLSCREEN-GPU-CHAIN*)."
  (mp:process-run-function
   "eta-hw-encoder-probe" '()
   (lambda ()
     (let ((encoder (ignore-errors (probe-hw-encoder))))
       (setf *hw-video-encoder* encoder)
       (recording-log "hw encoder probe: using ~a"
                      (or encoder "libx264 (no hardware encoder)"))
       (when (equal encoder "h264_qsv")
         (setf *hw-fullscreen-gpu-chain* (probe-hw-gpu-chain))
         (recording-log "gpu chain probe: fullscreen captures ~:[keep the hwdownload fallback~;stay on the GPU (hwmap -> scale_qsv)~]"
                        *hw-fullscreen-gpu-chain*))))))

;;; Fullscreen detection. An exclusive-fullscreen PSOBB renders past
;;; GDI, so the gdigrab capture records black frames (field-observed on
;;; a Boot Camp machine, 2026-07-07); BACKEND-FULLSCREEN-MONITOR makes
;;; BUILD-FFMPEG-ARGS switch to ddagrab. "Fullscreen" here is simply
;;; "the window covers its whole monitor" (RECT-COVERS-P): a borderless
;;; window matches too, and ddagrab records it just as well.

(fli:define-c-struct win-rect
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

;; MONITORINFOEXW: MONITORINFO plus the GDI device name
;; ("\\.\DISPLAYn"), which maps the monitor to a ddagrab output index
;; (DISPLAY-DEVICE-OUTPUT-INDEX).
(fli:define-c-struct monitorinfoex
  (cb-size (:unsigned :long))
  (rc-monitor (:struct win-rect))
  (rc-work (:struct win-rect))
  (flags (:unsigned :long))
  (device (:c-array (:unsigned :short) 32)))

(fli:define-foreign-function (%get-window-rect "GetWindowRect")
    ((hwnd :pointer)
     (rect (:pointer (:struct win-rect))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%monitor-from-window "MonitorFromWindow")
    ((hwnd :pointer)
     (flags (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-monitor-info "GetMonitorInfoW")
    ((monitor :pointer)
     (info (:pointer (:struct monitorinfoex))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(defconstant +monitor-defaulttonearest+ 2)

(defun rect-list (rect)
  "(left top right bottom) from a WIN-RECT pointer."
  (list (fli:foreign-slot-value rect 'left)
        (fli:foreign-slot-value rect 'top)
        (fli:foreign-slot-value rect 'right)
        (fli:foreign-slot-value rect 'bottom)))

(defun monitor-device-name (info)
  "The szDevice field of a MONITORINFOEX pointer as a string."
  (let ((device (fli:foreign-slot-pointer info 'device)))
    (with-output-to-string (out)
      (loop :for i :from 0 :below 32
            :for code := (fli:foreign-aref device i)
            :until (zerop code)
            :do (write-char (code-char code) out)))))

(defun psobb-fullscreen-monitor ()
  "Plist (:output-idx :width :height) of the monitor the PSOBB window
covers entirely, or NIL when the window is absent or windowed. The
dimensions are the monitor rect's - the fullscreen window spans it
exactly, so they are also the capture size (sizes the GPU-side scale)."
  (let ((hwnd (find-psobb-window)))
    (when hwnd
      (fli:with-dynamic-foreign-objects ((rect win-rect)
                                         (info monitorinfoex))
        (setf (fli:foreign-slot-value info 'cb-size)
              (fli:size-of '(:struct monitorinfoex)))
        (let ((monitor (%monitor-from-window hwnd
                                             +monitor-defaulttonearest+)))
          (when (and (not (fli:null-pointer-p monitor))
                     (%get-window-rect hwnd rect)
                     (%get-monitor-info monitor info))
            (let ((monitor-rect (rect-list (fli:foreign-slot-pointer
                                            info 'rc-monitor))))
              (when (rect-covers-p (rect-list rect) monitor-rect)
                (let ((index (display-device-output-index
                              (monitor-device-name info))))
                  (when index
                    (destructuring-bind (left top right bottom) monitor-rect
                      (list :output-idx index
                            :width (- right left)
                            :height (- bottom top)))))))))))))

;;; The live backend

(defclass win32-ffmpeg-backend () ())

(defun recording-log (fmt &rest args)
  "Append a timestamped line to %TEMP%\\ephinea-ta-recording.log. The
GUI can only show a one-line error label; capture-start failures in
the field (e.g. \"pointer out of memory bounds\" right after a game
crash, 2026-07-06) need the full story to be diagnosable after the
fact. Never signals."
  (ignore-errors
    (with-open-file (s (merge-pathnames "ephinea-ta-recording.log"
                                        (uiop:temporary-directory))
                       :direction :output :if-exists :append
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (multiple-value-bind (sec min hour day month)
          (decode-universal-time (get-universal-time))
        (format s "~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d "
                month day hour min sec))
      (apply #'format s fmt args)
      (terpri s))))

(defmethod backend-fullscreen-monitor ((backend win32-ffmpeg-backend))
  (let ((monitor (ignore-errors (psobb-fullscreen-monitor))))
    (when monitor
      (recording-log "fullscreen window: capturing via ddagrab output_idx=~d (~dx~d)"
                     (getf monitor :output-idx)
                     (getf monitor :width) (getf monitor :height)))
    monitor))

(defmethod backend-start-capture ((backend win32-ffmpeg-backend)
                                  ffmpeg-path args output-path
                                  &key audio-pipe audio-pid)
  (handler-case
      (progn
        (ensure-directories-exist output-path)
        ;; The pipe server end must exist before ffmpeg opens its
        ;; inputs. Any audio-side failure - returning NIL or signaling
        ;; (a COM/FLI error right after a game crash was seen in the
        ;; field) - drops the audio arguments and records video-only
        ;; rather than fail the capture; otherwise point ffmpeg at the
        ;; session's actual capture format.
        (let ((audio (and audio-pipe
                          (handler-case
                              (start-audio-session audio-pipe audio-pid)
                            (error (condition)
                              (recording-log
                               "audio session failed (video-only fallback), pid=~a: ~a"
                               audio-pid condition)
                              nil)))))
          (setf args
                (if audio
                    (retarget-audio-args
                     args
                     :sample-format (audio-session-sample-format audio)
                     :rate (audio-session-rate audio)
                     :channels (audio-session-channels audio))
                    (strip-audio-args args audio-pipe)))
          (handler-case
              (let ((capture (spawn-process ffmpeg-path args)))
                (setf (ffmpeg-capture-audio capture) audio)
                ;; The rate tokens say which profile ran (the low-memory
                ;; machines this diagnoses can only be read after the fact).
                (recording-log "capture started: audio=~a ffmpeg=~a~@[ b:v=~a~]~@[ crf=~a~]"
                               (and audio (audio-session-scope audio))
                               ffmpeg-path
                               (let ((p (position "-b:v" args :test #'equal)))
                                 (and p (nth (1+ p) args)))
                               (let ((p (position "-crf" args :test #'equal)))
                                 (and p (nth (1+ p) args))))
                capture)
            (error (condition)
              (when audio (stop-audio-session audio))
              (error condition)))))
    (error (condition)
      (recording-log "capture start FAILED: ~a~%  ffmpeg=~a output=~a pid=~a"
                     condition ffmpeg-path output-path audio-pid)
      (values nil (format nil "~a" condition)))))

(defmethod backend-capture-alive-p ((backend win32-ffmpeg-backend) capture)
  (capture-alive-p capture))

(defmethod backend-start-remux ((backend win32-ffmpeg-backend)
                                ffmpeg-path args)
  ;; The remux ffmpeg reads no stdin, but SPAWN-PROCESS's pipe is
  ;; harmless and keeps the capture token uniform.
  (handler-case (spawn-process ffmpeg-path args)
    (error (condition)
      (recording-log "remux start FAILED: ~a" condition)
      (values nil (format nil "~a" condition)))))

(defmethod backend-capture-succeeded-p ((backend win32-ffmpeg-backend) capture)
  (multiple-value-bind (ok code)
      (%get-exit-code-process (ffmpeg-capture-process-handle capture) 0)
    (and ok (zerop code))))

(defun write-quit (capture)
  ;; A lone "q" on stdin makes ffmpeg finish the output cleanly. Two
  ;; bytes always fit the pipe buffer, so this never blocks.
  (fli:with-dynamic-foreign-objects ()
    (let ((buffer (fli:allocate-dynamic-foreign-object
                   :type '(:unsigned :byte) :nelems 2)))
      (setf (fli:dereference buffer :index 0) (char-code #\q)
            (fli:dereference buffer :index 1) (char-code #\Newline))
      (%write-file (ffmpeg-capture-stdin-write capture) buffer 2 0
                   fli:*null-pointer*))))

(defmethod backend-request-stop ((backend win32-ffmpeg-backend) capture)
  ;; End the audio stream first: closing the pipe is the audio EOF
  ;; ffmpeg needs. ffmpeg reads the piped audio a couple of seconds
  ;; behind real time, and "q" makes it stop reading at once - so wait
  ;; (off-thread; the poll loop must not block) for the buffered tail
  ;; to drain before quitting, or the last seconds of audio are lost.
  (let ((audio (ffmpeg-capture-audio capture)))
    (cond (audio
           (stop-audio-session audio)
           (mp:process-run-function
            "eta-ffmpeg-stop" '()
            (lambda ()
              (sleep +audio-drain-seconds+)
              (ignore-errors (write-quit capture)))))
          (t (write-quit capture)))))

(defmethod backend-kill-capture ((backend win32-ffmpeg-backend) capture)
  (when (ffmpeg-capture-audio capture)
    (stop-audio-session (ffmpeg-capture-audio capture)))
  (%terminate-process (ffmpeg-capture-process-handle capture) 1))

(defmethod backend-close-capture ((backend win32-ffmpeg-backend) capture)
  ;; Idempotent; also reached when ffmpeg died on its own, where the
  ;; capture thread must not be left serving a dead pipe.
  (when (ffmpeg-capture-audio capture)
    (stop-audio-session (ffmpeg-capture-audio capture)))
  (close-capture-handles capture))

(defmethod backend-rename-file ((backend win32-ffmpeg-backend) from to)
  (uiop:rename-file-overwriting-target from to))

(defmethod backend-delete-file ((backend win32-ffmpeg-backend) path)
  (uiop:delete-file-if-exists path))

(defmethod backend-list-stale-files ((backend win32-ffmpeg-backend) dir)
  (when (probe-file dir)
    (mapcar #'namestring (uiop:directory-files dir "rec-tmp-*.mp4"))))

(defmethod backend-list-recordings ((backend win32-ffmpeg-backend) dir)
  (when (probe-file dir)
    (loop :for path :in (uiop:directory-files dir "*.mp4")
          ;; Skip the in-progress work files; only the kept, run-named
          ;; recordings count against the budget.
          :unless (uiop:string-prefix-p "rec-tmp-" (pathname-name path))
            :collect (list (namestring path)
                           (or (ignore-errors
                                 (with-open-file (s path
                                                    :element-type '(unsigned-byte 8))
                                   (file-length s)))
                               0)
                           (or (ignore-errors (file-write-date path)) 0)))))
