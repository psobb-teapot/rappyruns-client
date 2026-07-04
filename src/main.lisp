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
  "Queue RUNS and auto-submit; returns the submission results (updated
entries) so the caller can react to failures, or NIL when not submitting."
  (dolist (run runs)
    (enqueue-run! run))
  (when (and runs (config-value :auto-submit))
    (submit-queued!)))

#+lispworks
(defun run-completion-sounds (runs)
  "Completion beep the moment RUNS finish (before the network round
trip), then the error sound if auto-submission left any of them
failed/rejected. Two sounds correctly say: timed OK, upload did not."
  (when (config-value :completion-sound)
    (%message-beep +mb-iconasterisk+))
  (let ((results (handle-completed-runs runs)))
    (when (and (config-value :completion-sound)
               (find-if (lambda (entry)
                          (member (getf entry :status) '(:failed :rejected)))
                        results))
      (%message-beep +mb-iconhand+))))

(defvar *clipboard-seen-seq* nil
  "Last clipboard sequence number examined, so the poll loop only reads
the clipboard when its contents actually changed.")

(defvar *clipboard-last-offered* nil
  "Last URL the attach prompt was shown for; never offered twice.")

#+lispworks
(defun check-clipboard (interface)
  "Offer to attach a freshly copied YouTube URL to a draft. Cheap when
nothing changed: just a sequence-number read, no clipboard open."
  (let ((seq (clipboard-sequence-number)))
    (unless (eql seq *clipboard-seen-seq*)
      (setf *clipboard-seen-seq* seq)
      (let ((url (youtube-video-url (clipboard-text))))
        (when (and url
                   (not (equal url *clipboard-last-offered*))
                   (video-candidates))
          (setf *clipboard-last-offered* url)
          (offer-clipboard-url interface url))))))

#+lispworks
(defun poll-loop (interface)
  (let ((reader nil)
        (detector (make-detector))
        (recorder (make-recorder
                   :backend (make-instance 'win32-ffmpeg-backend)
                   ;; The kept file is tied to its queue entry so the
                   ;; Upload button and clipboard attach can find it.
                   :on-keep (lambda (path run)
                              (link-video-file! run path)
                              (refresh-runs-list interface))))
        (previous-snapshot nil)
        (last-gui-update 0))
    (ignore-errors (cleanup-stale-recordings recorder))
    (unwind-protect
         (loop :until *stop-requested*
               :do (cond
                     ;; Not attached: look for the game once per second.
                     ((null reader)
                      (setf reader (open-psobb-reader))
                      (setf *audio-target-pid*
                            (and reader (live-reader-pid reader)))
                      (if reader
                          (detector-step detector nil) ; fresh attach: disarm
                          (progn
                            ;; Keep any in-flight stop moving (deletes
                            ;; the abandoned file once ffmpeg exits).
                            (ignore-errors
                              (recorder-step recorder
                                             (detector-state detector)
                                             '() nil))
                            ;; "Submit pending runs" must work without
                            ;; the game running too.
                            (when *retry-requested*
                              (setf *retry-requested* nil)
                              (submit-queued!)
                              (refresh-runs-list interface))
                            ;; Uploads happen while the game is closed
                            ;; too, so watch the clipboard here as well.
                            (ignore-errors (check-clipboard interface))
                            (update-game-status interface nil detector nil
                                                recorder)
                            (mp:process-wait-with-timeout
                             "waiting for game" +search-interval+
                             (lambda () *stop-requested*)))))
                     ;; Attached but the process died: detach.
                     ((not (reader-alive-p reader))
                      (close-reader reader)
                      (setf reader nil)
                      (setf *audio-target-pid* nil)
                      (setf previous-snapshot nil)
                      (detector-step detector nil)
                      (ignore-errors
                        (recorder-step recorder (detector-state detector)
                                       '() nil)))
                     (t
                      (let* ((snapshot (augment-snapshot
                                        reader
                                        (ignore-errors (read-snapshot reader))))
                             (runs (detector-step detector snapshot)))
                        ;; Recording never interferes with detection or
                        ;; submission; errors only reach the GUI label.
                        (ignore-errors
                          (recorder-step recorder (detector-state detector)
                                         runs (reader-window-title reader)))
                        (when runs
                          (run-completion-sounds runs)
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
                            (ignore-errors (check-clipboard interface))
                            (update-game-status interface (and reader t)
                                                detector snapshot recorder)
                            (when (eq (detector-state detector) :in-quest)
                              (refresh-runs-list interface)))))
                      (mp:process-wait-with-timeout
                       "poll interval" +poll-interval+
                       (lambda () *stop-requested*)))))
      (ignore-errors (recorder-shutdown recorder))
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
    (maybe-prompt-for-token interface)
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
