(in-package :ephinea-ta-client)

;;; Entry point and the polling loop: attach to PSOBB, read a snapshot
;;; ~30x per second, feed the detector, queue and submit completed runs.

(defparameter +poll-interval+ (/ 1 30))
(defparameter +search-interval+ 1)
(defparameter +gui-update-interval+ 1/4)

;; *STOP-REQUESTED* is defvar'd in updater.lisp (which loads first and
;; sets it during a self-update handover).

(defvar *last-heavy-sample* 0
  "Internal real time of the last inventory sample.")

(defparameter +retention-interval-seconds+ 120
  "How often the local recordings folder is swept against its size
budget. Listing the folder and stat-ing every file is cheap but not
free, and the folder only grows a file per finished quest, so a couple
of minutes between sweeps is ample.")

(defvar *last-retention-sweep* nil
  "Internal real time of the last recordings-folder budget sweep, or NIL
before the first one.")

#+lispworks
(defun maybe-sweep-recordings (recorder)
  "Reap old recordings past the size budget, throttled so the folder is
not restat-ed on every idle poll. Runs only while idle (the encoder
keeps the disk during a capture); errors stay off the poll loop."
  (let ((now (get-internal-real-time)))
    (when (or (null *last-retention-sweep*)
              (>= (- now *last-retention-sweep*)
                  (* +retention-interval-seconds+
                     internal-time-units-per-second)))
      (setf *last-retention-sweep* now)
      (ignore-errors (apply-recording-retention recorder)))))

(defvar *psobb-rejection* nil
  "Rejection plist (:pid :path :status :signer) for a running PSOBB
whose exe failed Authenticode verification. The poll loop refuses to
attach to that pid - so nothing is detected, recorded or submitted -
and the GUI shows the reason.")

#+lispworks
(defun psobb-trust-rejection (reader)
  "NIL when READER's exe is the signed official Ephinea client, else
the rejection plist. The previous rejection is reused for the same
pid, so the once-per-second search loop does not re-hash the exe."
  (let ((pid (live-reader-pid reader))
        (cached *psobb-rejection*))
    (if (and cached (eql (getf cached :pid) pid))
        cached
        (let ((path (process-image-path reader)))
          (multiple-value-bind (status signer)
              (if path
                  (authenticode-verify path)
                  ;; No path means no verdict; fail closed.
                  (values :invalid nil))
            (unless (psobb-signature-trusted-p status signer)
              (list :pid pid :path path :status status :signer signer)))))))

(defun augment-snapshot (reader snapshot)
  "Attach :monsters (every poll: kill attribution and frame-1 detection
need psostats' full sample rate; READ-MONSTERS is one block read per
monster) and :inventory (about once per second: it is many small
process reads) to SNAPSHOT while a quest is loaded."
  (when (and snapshot
             (getf snapshot :quest-ptr)
             (plusp (getf snapshot :quest-ptr)))
    (setf snapshot
          (append snapshot
                  (list :monsters (ignore-errors (read-monsters reader)))))
    (let ((now (get-internal-real-time)))
      (when (>= (- now *last-heavy-sample*) internal-time-units-per-second)
        (setf *last-heavy-sample* now)
        (setf snapshot
              (append snapshot
                      (list :inventory (ignore-errors
                                         (read-inventory
                                          reader (getf snapshot :my-index)))))))))
  snapshot)

(defun handle-completed-runs (runs)
  "Queue RUNS and auto-submit; returns the submission results (updated
entries) so the caller can react to failures, or NIL when not submitting.
Aborted (mid-quest quit) runs are dropped when :submit-aborted is off."
  (let ((runs (if (config-value :submit-aborted)
                  runs
                  (remove-if (lambda (run) (getf run :aborted)) runs))))
    (dolist (run runs)
      (enqueue-run! run))
    (when (and runs (config-value :auto-submit))
      (submit-queued!))))

#+lispworks
(defun run-completion-sounds (runs)
  "Completion beep the moment RUNS finish (before the network round
trip), then the error sound if auto-submission left any of them
failed/rejected. Two sounds correctly say: timed OK, upload did not.
Aborted runs are silent - quitting a quest is not an event."
  (when (and (config-value :completion-sound)
             (find-if-not (lambda (run) (getf run :aborted)) runs))
    (%message-beep +mb-iconasterisk+))
  (let ((results (handle-completed-runs runs)))
    (when (and (config-value :completion-sound)
               (find-if (lambda (entry)
                          (member (getf entry :status) '(:failed :rejected)))
                        results))
      (%message-beep +mb-iconhand+))))

#+lispworks
(defvar *upload-process* nil
  "The in-flight video upload worker, or NIL. One at a time.")

#+lispworks
(defun maybe-start-upload (recorder)
  "Kick off the oldest pending recording upload in a worker thread.
Skipped while recording - the encoder gets the disk and the uplink to
itself - and while a previous upload is still running; failures back
off inside UPLOAD-ENTRY-VIDEO! and a later call picks up the retry."
  (when (and (config-value :video-upload)
             (not (eq (recorder-state recorder) :recording))
             (or (null *upload-process*)
                 (not (mp:process-alive-p *upload-process*))))
    (let ((entry (upload-candidate)))
      (when entry
        (setf *upload-process*
              (mp:process-run-function
               "eta-client-video-upload" '()
               (lambda ()
                 (let ((last-percent -1))
                   (upload-entry-video!
                    entry
                    :on-progress
                    (lambda (done total)
                      (setf *upload-progress*
                            (list (getf entry :server-id) done total))
                      ;; Redraw only when the whole percent moves, not
                      ;; on every megabyte.
                      (let ((percent (if (plusp total)
                                         (floor (* 100 done) total)
                                         0)))
                        (when (/= percent last-percent)
                          (setf last-percent percent)
                          (refresh-runs-list *interface*))))))
                 (setf *upload-progress* nil)
                 (refresh-runs-list *interface*))))))))

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
(defun note-poll-activity (interface detector recorder)
  "Track whether a run or recording is in flight (the updater never
swaps the exe mid-run) and, on return to idle, apply the deferred
update exactly once."
  (setf *poll-busy-p* (or (eq (detector-state detector) :in-quest)
                          (eq (recorder-state recorder) :recording)))
  (unless *poll-busy-p*
    (let ((ready *update-ready-zip*))
      (when ready
        (setf *update-ready-zip* nil)
        (apply-update-restart interface (car ready) (cdr ready))))))

#+lispworks
(defun poll-loop ()
  ;; INTERFACE is re-read from *INTERFACE* every iteration: the language
  ;; toggle replaces the window (REBUILD-INTERFACE), and updates must
  ;; land on the current one.
  (let ((reader nil)
        (detector (make-detector))
        (recorder (make-recorder
                   :backend (make-instance 'win32-ffmpeg-backend)
                   ;; The kept file is tied to its queue entry so the
                   ;; Upload button and clipboard attach can find it.
                   :on-keep (lambda (path run)
                              (link-video-file! run path)
                              (refresh-runs-list *interface*))))
        (previous-snapshot nil)
        (last-gui-update 0))
    (ignore-errors (cleanup-stale-recordings recorder))
    (unwind-protect
         (loop :for interface := *interface*
               :until *stop-requested*
               :do (note-poll-activity interface detector recorder)
                   (cond
                     ;; Not attached: look for the game once per second.
                     ((null reader)
                      (setf reader (open-psobb-reader))
                      (cond (reader
                             ;; Refuse anything but the signed official
                             ;; client - no attach, no recording.
                             (let ((rejection (psobb-trust-rejection reader)))
                               (setf *psobb-rejection* rejection)
                               (when rejection
                                 (close-reader reader)
                                 (setf reader nil))))
                            ;; The rejected process went away: back to
                            ;; plain searching.
                            ((and *psobb-rejection* (not (find-psobb-window)))
                             (setf *psobb-rejection* nil)))
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
                            (ignore-errors (maybe-start-upload recorder))
                            (ignore-errors (maybe-sweep-recordings recorder))
                            (update-game-status interface nil detector nil
                                                recorder *psobb-rejection*)
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
                        ;; Track the last kill regardless of the log toggle:
                        ;; the rule-registration dialog reads it any time.
                        (ignore-errors
                          (update-last-kill previous-snapshot snapshot))
                        ;; Accumulate this run's kills / switch flips by
                        ;; room, for the post-run room/enemy rule picker.
                        (ignore-errors
                          (update-run-logs previous-snapshot snapshot))
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
                            (ignore-errors (maybe-start-upload recorder))
                            (ignore-errors (maybe-sweep-recordings recorder))
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
(defun quit-app ()
  "Fully quit the app (tray Quit, or the x button when close-to-tray is
off). Stop the poll loop first so the recorder unwinds cleanly, drop the
tray icon so no ghost is left, then terminate unconditionally with
ExitProcess. We do NOT rely on LW:QUIT here: called from the tray thread
(inside its message loop) it did not reliably end the image, leaving the
process alive - so Quit appeared to do nothing. Config and the run queue
are persisted incrementally, so an abrupt exit loses nothing."
  (setf *really-quitting* t
        *stop-requested* t)
  (let ((poll *poll-process*))
    (when poll (ignore-errors (mp:process-join poll :timeout 10))))
  (ignore-errors (tray-remove-icon-now))
  (exit-process-now))

#+lispworks
(defun main ()
  ;; Single-instance guard, before any window or thread exists: a second
  ;; launch (manual, or the logon autostart entry) would otherwise start
  ;; yet another resident copy that hides to the tray, piling up unbounded.
  ;; Instead, raise the running instance's window and exit immediately.
  (when (already-running-p)
    (signal-existing-instance)
    (exit-process-now))
  (setf *stop-requested* nil
        *really-quitting* nil)
  (load-config!)
  (setf *language* (valid-language (config-value :language))
        ;; Seed from the last verified role so a moderator's Rooms tab and
        ;; rule button are present on the first frame; CHECK-TOKEN re-verifies
        ;; against /api/me and rebuilds the window if it changed.
        *moderator-p* (and (config-value :moderator) t))
  (cleanup-old-update-files)
  ;; Self-update BEFORE the main window exists, so an outdated build
  ;; never flashes at the user just to quit and relaunch. Does not
  ;; return when an update applies (helper handover + LW:QUIT).
  (when (and (config-value :auto-update) *client-version*)
    (startup-auto-update))
  (load-queue!)
  (load-quest-defs)
  (let ((interface (make-instance 'client-window)))
    (setf *interface* interface)
    (capi:display interface)
    (refresh-runs-list interface)
    (check-server interface)
    ;; A revoked token heals itself when a login.txt sits next to the
    ;; exe: the file login just issues a fresh one.
    (check-token interface
                 :on-invalid (lambda ()
                               (when (credentials-present-p)
                                 (start-file-login-flow interface))))
    (prompt-for-token-setup interface)
    (report-startup-update interface)
    ;; Resident-app tray icon: keeps running when the window is closed
    ;; (CLIENT-CONFIRM-DESTROY hides to the tray) and offers Show / Quit.
    (start-tray!)
    ;; Launch straight to the tray (autostart --minimized, or the config
    ;; toggle): the window is realized above, then hidden. A brief flash
    ;; is possible but keeping one display path is simpler than a
    ;; never-shown window, and the token setup prompt (first run only)
    ;; still needs a visible window.
    (when (startup-minimized-p)
      (setf (capi:top-level-interface-display-state interface) :hidden))
    (setf *poll-process*
          (mp:process-run-function "eta-client-poll" '() 'poll-loop))
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
