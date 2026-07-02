(in-package :ephinea-ta-client)

;;; Trigger discovery mode: when enabled, diff quest registers and floor
;;; switches between consecutive frames and append every change to
;;; %APPDATA%/ephinea-ta-client/trigger-log.txt. Used to find the end
;;; trigger for a new category (e.g. "kill everything up to room 2"):
;;; play the segment with logging on and look at which floor switch (or
;;; register) fires the moment the last enemy of the room dies.

(defvar *trigger-log-stream* nil)

;; The GUI thread (toggle -> header) and the poll thread (per-frame diffs)
;; both write here; serialize file access.
(defvar *trigger-log-lock*
  #+lispworks (mp:make-lock :name "eta-client-trigger-log")
  #-lispworks nil)

(defmacro with-trigger-log-lock (&body body)
  #+lispworks `(mp:with-lock (*trigger-log-lock*) ,@body)
  #-lispworks `(progn ,@body))

(defun trigger-log-path ()
  (merge-pathnames "trigger-log.txt" (config-dir)))

(defun trigger-log-stream ()
  (or *trigger-log-stream*
      (setf *trigger-log-stream*
            (progn
              (ensure-directories-exist (trigger-log-path))
              (open (trigger-log-path) :direction :output
                    :if-exists :append :if-does-not-exist :create
                    :external-format :utf-8)))))

(defun close-trigger-log ()
  (with-trigger-log-lock
    (when *trigger-log-stream*
      (ignore-errors (close *trigger-log-stream*))
      (setf *trigger-log-stream* nil))))

(defun start-trigger-log ()
  "Open (creating) the log and write a session header, so the file exists
the moment logging is enabled - before any trigger has changed. Returns
the log path."
  (with-trigger-log-lock
    (let ((stream (trigger-log-stream)))
      (format stream "~&=== trigger logging started ~a ===~%" (time-of-day))
      (format stream "Play the segment; each register / floor-switch change is listed below.~%")
      (finish-output stream)))
  (trigger-log-path))

(defun time-of-day ()
  (multiple-value-bind (second minute hour) (decode-universal-time
                                             (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun log-trigger-changes (previous snapshot)
  "Append register / floor-switch diffs between two consecutive
snapshots of the same loaded quest."
  (when (and previous snapshot
             (getf previous :quest-name)
             (getf snapshot :quest-name)
             (eql (getf previous :quest-ptr) (getf snapshot :quest-ptr)))
    (with-trigger-log-lock
    (let ((stream (trigger-log-stream))
          (stamp (time-of-day))
          (quest (getf snapshot :quest-name))
          (changes 0))
      (let ((old (getf previous :registers))
            (new (getf snapshot :registers)))
        (when (and old new)
          (dotimes (id +register-count+)
            (let ((old-value (bytes-u16 old (* 4 id)))
                  (new-value (bytes-u16 new (* 4 id))))
              (unless (= old-value new-value)
                (incf changes)
                (format stream "~a ~s register ~d: ~d -> ~d~%"
                        stamp quest id old-value new-value))))))
      (let ((old (getf previous :floor-switches))
            (new (getf snapshot :floor-switches)))
        (when (and old new)
          (dotimes (i (min (length old) (length new)))
            (let ((old-byte (aref old i))
                  (new-byte (aref new i)))
              (unless (= old-byte new-byte)
                (dotimes (bit 8)
                  (let ((mask (ash #x80 (- bit))))
                    (unless (= (logand old-byte mask) (logand new-byte mask))
                      (incf changes)
                      (format stream "~a ~s floor ~d switch ~d: ~:[off~;on~] -> ~:[off~;on~]~%"
                              stamp quest
                              (floor i 32)
                              (+ (* 8 (mod i 32)) bit)
                              (plusp (logand old-byte mask))
                              (plusp (logand new-byte mask)))))))))))
      (when (plusp changes)
        (force-output stream))
      changes))))
