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
name (e.g. \"ffmpeg.exe\") is searched on PATH by the API."
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
                                       t +create-no-window+
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

;;; The live backend

(defclass win32-ffmpeg-backend () ())

(defmethod backend-start-capture ((backend win32-ffmpeg-backend)
                                  ffmpeg-path args output-path
                                  &key audio-pipe audio-pid)
  (handler-case
      (progn
        (ensure-directories-exist output-path)
        ;; The pipe server end must exist before ffmpeg opens its
        ;; inputs. If the session cannot start at all, drop the audio
        ;; arguments and record video-only rather than fail the run;
        ;; otherwise point ffmpeg at the session's actual capture format.
        (let ((audio (and audio-pipe
                          (start-audio-session audio-pipe audio-pid))))
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
                capture)
            (error (condition)
              (when audio (stop-audio-session audio))
              (error condition)))))
    (error (condition)
      (values nil (format nil "~a" condition)))))

(defmethod backend-capture-alive-p ((backend win32-ffmpeg-backend) capture)
  (capture-alive-p capture))

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
