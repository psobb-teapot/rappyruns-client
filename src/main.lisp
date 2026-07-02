(in-package :ephinea-ta-client)

;;; Entry point and the polling loop: attach to PSOBB, read a snapshot
;;; ~30x per second, feed the detector, queue and submit completed runs.

(defparameter +poll-interval+ (/ 1 30))
(defparameter +search-interval+ 1)
(defparameter +gui-update-interval+ 1/4)

(defvar *stop-requested* nil)

(defun handle-completed-run (run)
  (enqueue-run! run)
  (when (config-value :auto-submit)
    (submit-queued!)))

(defun poll-step (reader detector)
  "One frame. Returns the run plist when a quest just completed."
  (let ((snapshot (ignore-errors (read-snapshot reader))))
    (detector-step detector snapshot)))

#+lispworks
(defun poll-loop (interface)
  (let ((reader nil)
        (detector (make-detector))
        (last-gui-update 0))
    (unwind-protect
         (loop :until *stop-requested*
               :do (cond
                     ;; Not attached: look for the game once per second.
                     ((null reader)
                      (setf reader (open-psobb-reader))
                      (if reader
                          (detector-step detector nil) ; fresh attach: disarm
                          (progn
                            (update-game-status interface nil detector nil)
                            (mp:process-wait-with-timeout
                             "waiting for game" +search-interval+
                             (lambda () *stop-requested*)))))
                     ;; Attached but the process died: detach.
                     ((not (reader-alive-p reader))
                      (close-reader reader)
                      (setf reader nil)
                      (detector-step detector nil))
                     (t
                      (let* ((snapshot (ignore-errors (read-snapshot reader)))
                             (run (detector-step detector snapshot)))
                        (when run
                          (handle-completed-run run)
                          (refresh-runs-list interface))
                        (when *retry-requested*
                          (setf *retry-requested* nil)
                          (submit-queued!)
                          (refresh-runs-list interface))
                        (let ((now (get-internal-real-time)))
                          (when (> (- now last-gui-update)
                                   (* +gui-update-interval+
                                      internal-time-units-per-second))
                            (setf last-gui-update now)
                            (update-game-status interface (and reader t)
                                                detector snapshot)
                            (when (eq (detector-state detector) :in-quest)
                              (refresh-runs-list interface)))))
                      (mp:process-wait-with-timeout
                       "poll interval" +poll-interval+
                       (lambda () *stop-requested*)))))
      (when reader (close-reader reader)))))

#+lispworks
(defun main ()
  (setf *stop-requested* nil)
  (load-config!)
  (load-queue!)
  (load-quest-defs)
  (let ((interface (make-instance 'client-window)))
    (capi:display interface)
    (refresh-runs-list interface)
    (check-server interface)
    (mp:process-run-function "eta-client-poll" '()
                             (lambda () (poll-loop interface)))
    interface))

#-lispworks
(defun main ()
  (error "The GUI requires LispWorks; use RUN-HEADLESS for testing."))

(defun run-headless (&key snapshots on-run)
  "Drive the detector over a pre-built list of SNAPSHOTS (demo/testing).
Calls ON-RUN with each completed run; returns all completed runs.
Uses whatever config is already loaded - it does not reload it."
  (let ((detector (make-detector))
        (completed '()))
    (dolist (snapshot snapshots (nreverse completed))
      (let ((run (detector-step detector snapshot)))
        (when run
          (push run completed)
          (when on-run (funcall on-run run)))))))
