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
;;; TerminateProcess leaves a playable file.

;;; Capture-backend protocol

(defgeneric backend-start-capture (backend ffmpeg-path args output-path)
  (:documentation
   "Spawn ffmpeg with ARGS (a list of argv strings; OUTPUT-PATH is the
last of them, passed separately so the backend can create the
directory). Returns a capture token, or (values nil error-string)."))

(defgeneric backend-capture-alive-p (backend capture))

(defgeneric backend-request-stop (backend capture)
  (:documentation "Ask ffmpeg to finish gracefully (\"q\" on stdin)."))

(defgeneric backend-kill-capture (backend capture)
  (:documentation "Force-terminate ffmpeg (TerminateProcess)."))

(defgeneric backend-close-capture (backend capture)
  (:documentation "Release process/pipe handles once the process is dead."))

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
(defparameter +stop-grace-seconds+ 5
  "How long to wait for ffmpeg to exit after \"q\" before killing it.")

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
  "\"<slug>_<9m59.123>_<2026-07-04_2130>.mp4\" from a completed run plist."
  (multiple-value-bind (total-seconds msec) (floor (getf run :time-ms 0) 1000)
    (multiple-value-bind (minutes seconds) (floor total-seconds 60)
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (or (getf run :finished-at)
                                     (get-universal-time)))
        (declare (ignore sec))
        (sanitize-filename
         (format nil "~a_~dm~2,'0d.~3,'0d_~4,'0d-~2,'0d-~2,'0d_~2,'0d~2,'0d.mp4"
                 (or (getf run :quest-slug) "run")
                 minutes seconds msec year month day hour min))))))

(defun best-session-run (runs)
  "The run that names the video: longest time-ms, i.e. the full clear
when segments completed alongside it."
  (first (sort (copy-list runs) #'>
               :key (lambda (run) (getf run :time-ms 0)))))

(defun build-ffmpeg-args (&key window-title output-path
                               (framerate +record-framerate+))
  "ffmpeg argv (without the program itself). Fragmented MP4 keeps the
file playable even when ffmpeg is killed instead of quitting on \"q\"."
  (list "-y" "-loglevel" "error"
        "-f" "gdigrab"
        "-framerate" (princ-to-string framerate)
        "-draw_mouse" "0"
        "-i" (format nil "title=~a" window-title)
        "-c:v" "libx264"
        "-preset" +record-preset+
        "-crf" (princ-to-string +record-crf+)
        "-pix_fmt" "yuv420p"
        "-movflags" "+frag_keyframe+empty_moov"
        output-path))

;;; The recorder state machine

(defstruct recorder
  backend              ; capture-backend protocol object
  (state :idle)        ; :idle | :recording | :stopping
  capture              ; backend token while :recording / :stopping
  tmp-path             ; file ffmpeg is writing (namestring)
  session-runs         ; runs completed during this capture
  (last-detector-state :idle)
  stop-deadline        ; internal real time to give up on "q" and kill
  pending-keep-p       ; decided when the stop begins
  final-path           ; rename target when keeping
  last-error)          ; string for the GUI, or NIL

(defun start-recording (recorder window-title)
  (let* ((ffmpeg (resolve-ffmpeg-path))
         (output (recording-tmp-path))
         (args (build-ffmpeg-args :window-title window-title
                                  :output-path output)))
    (multiple-value-bind (capture error)
        (backend-start-capture (recorder-backend recorder) ffmpeg args output)
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
          (recorder-final-path recorder)
          (and best (namestring (merge-pathnames (run-video-filename best)
                                                 (resolve-record-dir))))
          (recorder-stop-deadline recorder)
          (+ (get-internal-real-time)
             (* +stop-grace-seconds+ internal-time-units-per-second))
          (recorder-state recorder) :stopping)
    (ignore-errors (backend-request-stop (recorder-backend recorder)
                                         (recorder-capture recorder)))))

(defun finalize-capture (recorder)
  "The process is dead: release handles, then rename or delete the file."
  (let ((backend (recorder-backend recorder)))
    (ignore-errors (backend-close-capture backend (recorder-capture recorder)))
    (if (and (recorder-pending-keep-p recorder) (recorder-final-path recorder))
        (handler-case
            (backend-rename-file backend (recorder-tmp-path recorder)
                                 (recorder-final-path recorder))
          (error (condition)
            (setf (recorder-last-error recorder)
                  (format nil "could not save recording: ~a" condition))))
        (ignore-errors
          (backend-delete-file backend (recorder-tmp-path recorder)))))
  (setf (recorder-capture recorder) nil
        (recorder-tmp-path recorder) nil
        (recorder-session-runs recorder) '()
        (recorder-pending-keep-p recorder) nil
        (recorder-final-path recorder) nil
        (recorder-stop-deadline recorder) nil
        (recorder-state recorder) :idle))

(defun abort-capture (recorder message)
  "ffmpeg died on its own mid-capture: clean up and surface the error."
  (let ((backend (recorder-backend recorder)))
    (ignore-errors (backend-close-capture backend (recorder-capture recorder)))
    (ignore-errors (backend-delete-file backend (recorder-tmp-path recorder))))
  (setf (recorder-capture recorder) nil
        (recorder-tmp-path recorder) nil
        (recorder-session-runs recorder) '()
        (recorder-pending-keep-p recorder) nil
        (recorder-final-path recorder) nil
        (recorder-stop-deadline recorder) nil
        (recorder-state recorder) :idle
        (recorder-last-error recorder) message))

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
        (setf (recorder-stop-deadline recorder) nil)))))
  (setf (recorder-last-detector-state recorder) detector-state)
  recorder)

(defun recorder-shutdown (recorder &key (timeout +stop-grace-seconds+))
  "Client is exiting: finish any capture in progress, waiting up to
TIMEOUT seconds for a graceful stop before killing ffmpeg."
  (when (eq (recorder-state recorder) :recording)
    (begin-stop recorder))
  (when (eq (recorder-state recorder) :stopping)
    (let ((backend (recorder-backend recorder))
          (capture (recorder-capture recorder))
          (deadline (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second))))
      (loop :while (and (backend-capture-alive-p backend capture)
                        (< (get-internal-real-time) deadline))
            :do (sleep 0.05))
      (when (backend-capture-alive-p backend capture)
        (ignore-errors (backend-kill-capture backend capture)))
      (finalize-capture recorder))))

(defun cleanup-stale-recordings (recorder)
  "Delete rec-tmp-*.mp4 left behind by a crash of a previous session."
  (let ((backend (recorder-backend recorder)))
    (dolist (path (ignore-errors
                    (backend-list-stale-files backend (resolve-record-dir))))
      (ignore-errors (backend-delete-file backend path)))))
