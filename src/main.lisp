(in-package :ephinea-ta-client)

;;; Entry point and the polling loop: attach to PSOBB, read a snapshot
;;; ~30x per second, feed the detector, queue and submit completed runs.

(defparameter +poll-interval+ (/ 1 30))
(defparameter +search-interval+ 1)
(defparameter +gui-update-interval+ 1/4)

(defvar *stop-requested* nil)

(defvar *last-heavy-sample* 0
  "Internal real time of the last inventory/monster sample.")

(defun augment-snapshot (reader snapshot)
  "Attach :inventory and :monsters to SNAPSHOT about once per second
while a quest is loaded. These need many small process reads, so they
are sampled instead of read at the 30/s poll rate; the telemetry module
uses them whenever present."
  (when (and snapshot
             (getf snapshot :quest-ptr)
             (plusp (getf snapshot :quest-ptr)))
    (let ((now (get-internal-real-time)))
      (when (>= (- now *last-heavy-sample*) internal-time-units-per-second)
        (setf *last-heavy-sample* now)
        (setf snapshot
              (append snapshot
                      (list :inventory (ignore-errors
                                         (read-inventory
                                          reader (getf snapshot :my-index)))
                            :monsters (ignore-errors
                                        (read-monsters reader))))))))
  snapshot)

(defun handle-completed-runs (runs)
  (dolist (run runs)
    (enqueue-run! run))
  (when (and runs (config-value :auto-submit))
    (submit-queued!)))

#+lispworks
(defun poll-loop (interface)
  (let ((reader nil)
        (detector (make-detector))
        (previous-snapshot nil)
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
                      (setf previous-snapshot nil)
                      (detector-step detector nil))
                     (t
                      (let* ((snapshot (augment-snapshot
                                        reader
                                        (ignore-errors (read-snapshot reader))))
                             (runs (detector-step detector snapshot)))
                        (when runs
                          (handle-completed-runs runs)
                          (refresh-runs-list interface))
                        (when (config-value :trigger-log)
                          (ignore-errors
                            (log-trigger-changes previous-snapshot snapshot)))
                        (setf previous-snapshot snapshot)
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
      (close-trigger-log)
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
      (dolist (run (detector-step detector snapshot))
        (push run completed)
        (when on-run (funcall on-run run))))))
