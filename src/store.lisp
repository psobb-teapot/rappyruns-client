(in-package :ephinea-ta-client)

;;; Completed-run queue: holds detector output until it is submitted,
;;; persists unsubmitted runs across restarts, and retries failures.
;;; Server-side duplicate detection (submitter x quest x time_ms) makes
;;; resubmission idempotent.

(defvar *runs* '()
  "Newest first. Each entry is the detector run plist plus
:status (:queued | :submitted | :duplicate | :rejected | :failed)
and, once submitted, :url and :reason on rejection.")

(defvar *runs-lock*
  #+lispworks (mp:make-lock :name "eta-client-runs")
  #-lispworks nil)

(defmacro with-runs-lock (&body body)
  #+lispworks `(mp:with-lock (*runs-lock*) ,@body)
  #-lispworks `(progn ,@body))

(defparameter +max-finished-runs+ 50
  "How many finished entries (submitted/duplicate/rejected) to keep in
the list; active ones (see ENTRY-ACTIVE-P) are never dropped.")

(defun entry-active-p (entry)
  "Unfinished business that must survive trimming and restarts: entries
awaiting (re)submission, and entries whose saved video has not been
attached to their server draft yet (unless the upload was permanently
rejected - those are as finished as a rejected run). Aborted runs never
upload their recording (reset-farming would flood hosted storage with
worthless footage), so a pending video does not keep them active."
  (or (member (getf entry :status) '(:queued :failed))
      (and (getf entry :video-path)
           (getf entry :server-id)
           (not (getf entry :aborted))
           (not (getf entry :video-attached))
           (not (getf entry :upload-given-up)))))

(defun trim-finished-runs (runs limit)
  "RUNS is newest first: keep every active entry and only the newest
LIMIT finished ones, preserving order."
  (let ((finished 0))
    (remove-if (lambda (entry)
                 (and (not (entry-active-p entry))
                      (> (incf finished) limit)))
               runs)))

;;; Display helpers for run entries. They live here rather than in the
;;; (LispWorks-only) GUI so the SBCL tests can cover them.

(defun format-run-time (ms)
  (multiple-value-bind (total-seconds msec) (floor ms 1000)
    (multiple-value-bind (minutes seconds) (floor total-seconds 60)
      (format nil "~d:~2,'0d.~3,'0d" minutes seconds msec))))

(defun run-status-label (entry)
  (if (getf entry :video-attached)
      (cond
        ;; An aborted run's link never enters review, so it must not claim
        ;; to be "awaiting review" like an ordinary attached draft does.
        ((getf entry :aborted)
         (tr :status-aborted-video-attached))
        ;; A hosted recording streamed from the client lands 'held': it
        ;; is on the server but the player must publish it in the browser
        ;; (issue 105), so point them there rather than promise a review.
        ((getf entry :held)
         (tr :status-video-held))
        ;; A :duplicate reply may report the run already approved (issue
        ;; 100), so it must not read "awaiting review".
        ((getf entry :approved)
         (tr :status-video-approved))
        (t
         (tr :status-video-attached)))
      (case (getf entry :status)
        (:queued (tr :status-queued))
        (:submitted (cond ((getf entry :aborted)
                           (tr :status-draft-aborted))
                          ((not (getf entry :video-path))
                           (tr :status-draft-add))
                          ((and (config-value :video-upload)
                                (not (getf entry :upload-given-up)))
                           (tr :status-draft-auto-upload))
                          (t (tr :status-draft-upload))))
        (:duplicate (tr :status-duplicate))
        (:rejected (tr :status-rejected (or (getf entry :reason) "?")))
        (:failed (tr :status-failed (or (getf entry :reason) "?")))
        (t "?"))))

(defun run-video-label (entry)
  "The Video column: the recording's journey from disk to the site."
  (cond ((getf entry :video-uploaded) (tr :video-uploaded))
        ((getf entry :video-attached) (tr :video-attached))
        ((upload-progress-percent entry) (tr :video-uploading
                                             (upload-progress-percent entry)))
        ((getf entry :upload-given-up) (tr :video-upload-failed))
        ((getf entry :video-path) (tr :video-saved))
        (t "")))

(defvar *queue-path* nil
  "Override for tests; NIL means the real %APPDATA% location.")

(defun queue-path ()
  (or *queue-path* (merge-pathnames "queue.sexp" (config-dir))))

(defun persistable-entry (entry)
  "ENTRY as written to disk. Entries kept only for their pending video
link were already submitted, so the (large) telemetry payload is dropped."
  (if (member (getf entry :status) '(:queued :failed))
      entry
      (let ((copy (copy-list entry)))
        (remf copy :telemetry)
        copy)))

(defun save-queue! ()
  (write-sexp-file (queue-path)
                   (mapcar #'persistable-entry
                           (with-runs-lock
                             (remove-if-not #'entry-active-p *runs*)))))

(defun load-queue! ()
  (let ((saved (read-sexp-file (queue-path))))
    (when (listp saved)
      (with-runs-lock
        (setf *runs* (append *runs* saved))))))

(defun queued-runs ()
  (with-runs-lock (copy-list *runs*)))

(defun entry-unsent-p (entry)
  "Entries that exist nowhere but this client: clearing one loses the
run for good, unlike drafts (on the server) and recordings (on disk)."
  (member (getf entry :status) '(:queued :failed)))

(defun clear-runs! ()
  "Drop every entry except unsent ones (see ENTRY-UNSENT-P). Cleared
drafts stay on the server and their recordings stay on disk; only this
client's link between the two is forgotten, so a video can still be
added on the site. Returns the number of entries removed."
  (let ((removed (with-runs-lock
                   (let ((kept (remove-if-not #'entry-unsent-p *runs*)))
                     (prog1 (- (length *runs*) (length kept))
                       (setf *runs* kept))))))
    (save-queue!)
    removed))

(defun enqueue-run! (run)
  (let ((entry (list* :status :queued run)))
    (with-runs-lock
      (push entry *runs*)
      (setf *runs* (trim-finished-runs *runs* +max-finished-runs+)))
    (save-queue!)
    entry))

(defun update-run! (entry &rest updates)
  "Replace ENTRY in *RUNS* with a copy carrying UPDATES; returns the copy.
\(A plain (setf getf) on the shared plist would silently drop keys that
are not already present.)"
  (let ((new (copy-list entry)))
    (loop :for (key value) :on updates :by #'cddr
          :do (setf (getf new key) value))
    (with-runs-lock
      (setf *runs* (trim-finished-runs
                    (substitute new entry *runs* :test #'eq)
                    +max-finished-runs+)))
    (save-queue!)
    new))

(defun submission-updates (outcome payload)
  "SUBMIT-RUN's result -> the plist of updates for the queue entry. The
server id is kept so a video can later be attached over the API."
  (let ((server-id (and (hash-table-p payload) (gethash "id" payload)))
        (url (and (hash-table-p payload) (gethash "url" payload)))
        (errors (and (hash-table-p payload) (gethash "errors" payload)))
        (message (and (hash-table-p payload) (gethash "message" payload))))
    (ecase outcome
      (:created (list :status :submitted :url url :server-id server-id))
      (:duplicate (list :status :duplicate :url url :server-id server-id))
      (:rejected (list :status :rejected
                       :reason (format nil "~@[~a ~]~@[~{~a~^; ~}~]"
                                       message
                                       (and errors (coerce errors 'list))))))))

(defun submit-entry! (entry)
  "Submit one queue entry. Returns the updated entry."
  (handler-case
      (multiple-value-bind (outcome payload) (submit-run entry)
        (apply #'update-run! entry (submission-updates outcome payload)))
    (api-error (condition)
      (update-run! entry :status :failed
                   :reason (api-error-message condition)))))

(defun submit-queued! ()
  "Try to submit every :queued or :failed run. Returns the UPDATED
entries (the pre-submission plists would show stale statuses)."
  (let ((pending (with-runs-lock
                   (remove-if-not (lambda (entry)
                                    (member (getf entry :status) '(:queued :failed)))
                                  *runs*))))
    (mapcar #'submit-entry! pending)))

;;; Linking recordings to queue entries and attaching their YouTube URL.
;;; UPDATE-RUN! replaces entries with copies, so identity across updates
;;; is the run's natural key rather than EQ.

(defun same-run-p (a b)
  (and (equal (getf a :quest-slug) (getf b :quest-slug))
       (eql (getf a :time-ms) (getf b :time-ms))
       (eql (getf a :finished-at) (getf b :finished-at))))

(defun link-video-file! (run video-path)
  "Record VIDEO-PATH on the queue entry for RUN. Returns the updated
entry, or NIL when the run is no longer in the list."
  (let ((entry (find run (queued-runs) :test #'same-run-p)))
    (when entry
      (update-run! entry :video-path (namestring video-path)))))

(defun hosted-video-replaceable-p (entry)
  "True when ENTRY's video on the server is the auto-uploaded hosted
copy and no external URL has been attached. Until a moderator approves
the run, the player may swap that copy for their own YouTube/Twitch
link (hosted videos expire under retention; their own channel's do
not)."
  (and (getf entry :video-uploaded)
       (not (getf entry :video-url))))

(defun video-candidates ()
  "Entries a copied video URL could belong to: on the server (they have
an id) and either no video attached yet, or only the auto-uploaded
hosted copy, which the player may still replace with an external link."
  (remove-if-not (lambda (entry)
                   (and (getf entry :server-id)
                        (or (not (getf entry :video-attached))
                            (hosted-video-replaceable-p entry))))
                 (queued-runs)))

(defun resolve-video-target (candidates preferred)
  "Which of CANDIDATES a copied video URL should go to. Taken
automatically only for PREFERRED - the run whose Upload to YouTube
button the player just pressed (so the copied link is the one they were
sent to fetch). A copied URL is otherwise never silently pinned to a
run, not even a lone candidate: a link copied for some other purpose (a
party member's video, a different run submitted on the site) must not
land on an unrelated draft, so the player picks explicitly (:choose).
NIL when there are no candidates."
  (cond
    ((null candidates) nil)
    ((and preferred (find preferred candidates :test #'same-run-p)))
    (t :choose)))

;;; Automatic upload of saved recordings to the server (which relays
;;; them into hosted storage). One upload at a time; failures back off
;;; and are retried by the poll loop, permanent rejections give up but
;;; leave the entry eligible for the manual YouTube flow.

(defvar *upload-progress* nil
  "(server-id bytes-so-far total) while an upload is in flight, else
NIL. A special variable rather than an entry key: progress ticks every
megabyte and UPDATE-RUN! writes the queue file on every change.")

(defun upload-progress-percent (entry)
  "Whole percent of ENTRY's in-flight upload, or NIL when it has none."
  (let ((progress *upload-progress*))
    (when (and progress
               (getf entry :server-id)
               (eql (first progress) (getf entry :server-id)))
      (let ((done (second progress))
            (total (third progress)))
        (if (plusp total) (floor (* 100 done) total) 0)))))

(defparameter +upload-retry-seconds+ 300
  "Backoff after a transport failure.")
(defparameter +upload-limit-retry-seconds+ 3600
  "Backoff when the server's pending-review limit is full; only a
moderator decision frees a slot, so probing more often is pointless.")

(defun upload-candidate (&key (now (get-universal-time)))
  "The oldest entry whose saved recording still needs uploading and is
not backing off. Aborted runs are skipped: their recording stays on
disk for the player, but a cancelled attempt has no review value and
heavy reset-farming (100 laps a day happen) would swamp hosted storage.
An entry whose file vanished from disk (the user deleted it) gives up
on the spot and the scan moves on."
  (dolist (entry (reverse (queued-runs)))
    (when (and (getf entry :video-path)
               (getf entry :server-id)
               (not (getf entry :aborted))
               (not (getf entry :video-attached))
               (not (getf entry :upload-given-up))
               (let ((next (getf entry :next-upload-at)))
                 (or (null next) (<= next now))))
      (if (probe-file (getf entry :video-path))
          (return entry)
          (update-run! entry :upload-given-up t)))))

(defun video-path-retention-sets ()
  "Two lists of recording namestrings drawn from the run queue, for the
local storage sweep (APPLY-RECORDING-RETENTION): PROTECTED, files still
awaiting their upload - the only copy the leaderboard submit can use, so
retention must never take them - and UPLOADED, files the site already
holds, which are the first to reclaim. Returns (values protected
uploaded). A file on disk matching no queue entry (an old, trimmed run)
lands in neither and is reaped in the sweep's middle tier. The
protected predicate mirrors UPLOAD-CANDIDATE's eligibility: aborted and
given-up entries never upload, so their videos are not protected -
reset-farm footage is exactly what the budget is meant to reclaim."
  (let ((protected '())
        (uploaded '()))
    (dolist (entry (queued-runs))
      (let ((path (getf entry :video-path)))
        (when path
          (cond
            ((getf entry :video-attached) (push path uploaded))
            ((and (getf entry :server-id)
                  (not (getf entry :aborted))
                  (not (getf entry :upload-given-up)))
             (push path protected))))))
    (values protected uploaded)))

(defun apply-recording-retention (recorder)
  "Enforce the local recordings size budget, reaping the oldest kept
videos once the folder exceeds :RECORD-MAX-TOTAL-GB. Consults the run
queue (VIDEO-PATH-RETENTION-SETS) so a file still awaiting its upload is
never taken, and so already-uploaded files go first. A no-op with no cap
set, and only while the recorder is idle - the encoder gets the disk to
itself and no in-flight tmp/remux file can be caught mid-write."
  (let ((cap (record-max-total-bytes)))
    (when (and cap (eq (recorder-state recorder) :idle))
      (let ((backend (recorder-backend recorder)))
        (multiple-value-bind (protected uploaded) (video-path-retention-sets)
          (dolist (path (recordings-to-evict
                         (ignore-errors
                           (backend-list-recordings backend
                                                    (resolve-record-dir)))
                         cap :protected protected :uploaded uploaded))
            (ignore-errors (backend-delete-file backend path))))))))

(defun upload-entry-video! (entry &key on-progress)
  "Upload ENTRY's recording to its server draft. Success marks the
entry attached (same flag as the manual URL flow, so every consumer
agrees the video is on the server); a :duplicate reply means a video
was already on file, which is just as done. Returns the updated entry."
  (handler-case
      (multiple-value-bind (outcome payload)
          (upload-run-video (getf entry :server-id)
                            (getf entry :video-path)
                            :offset-ms (getf entry :video-offset-ms)
                            :on-progress on-progress)
        (ecase outcome
          ((:attached :duplicate)
           ;; The site has it now, so the local copy is optional. Delete
           ;; it immediately when the player opted in; otherwise it stays
           ;; until the folder budget reaps it (APPLY-RECORDING-RETENTION).
           (when (config-value :delete-after-upload)
             (ignore-errors (uiop:delete-file-if-exists (getf entry :video-path))))
           ;; A fresh hosted upload lands 'held' server-side: it is on
           ;; the server but not public until the player publishes it in
           ;; the browser (issue 105), so the label must point them there
           ;; rather than promise a review. A :duplicate reply carries the
           ;; run's real status too - it may already be approved.
           (let ((status (and (hash-table-p payload)
                              (gethash "status" payload))))
             (update-run! entry :video-attached t :video-uploaded t
                          :held (equal status "held")
                          :approved (equal status "approved"))))
          (:rejected
           (let ((code (and (hash-table-p payload) (gethash "error" payload)))
                 (message (and (hash-table-p payload)
                               (gethash "message" payload))))
             (if (equal code "pending-limit")
                 (update-run! entry :next-upload-at
                              (+ (get-universal-time)
                                 +upload-limit-retry-seconds+))
                 (update-run! entry :upload-given-up t
                              :upload-error (or message code "rejected")))))))
    (api-error (condition)
      (update-run! entry :next-upload-at
                   (+ (get-universal-time) +upload-retry-seconds+)
                   :upload-error (api-error-message condition)))))

(defun attach-video-url! (entry video-url)
  "Attach VIDEO-URL to ENTRY's server run: a draft is promoted to
pending review, an auto-uploaded run has its hosted copy replaced.
Returns (values updated-entry error-message already-submitted-p);
ERROR-MESSAGE is NIL on success. ALREADY-SUBMITTED-P is true when the
server reports the run already carries a video it will not swap (a
duplicate reply - typically the run was approved in the meantime); the
entry is marked attached but keeps its existing video state."
  (handler-case
      (multiple-value-bind (outcome payload)
          (attach-run-video (getf entry :server-id) video-url)
        (ecase outcome
          (:attached
           (if (and (hash-table-p payload) (gethash "duplicate" payload))
               (values (update-run! entry :video-attached t) nil t)
               ;; A successful replace deletes the hosted copy server
               ;; side, so the entry's video is now the external URL,
               ;; not an upload.
               (values (update-run! entry :video-attached t
                                    :video-url video-url
                                    :video-uploaded nil)
                       nil)))
          (:rejected
           (values nil
                   (or (and (hash-table-p payload) (gethash "message" payload))
                       (and (hash-table-p payload) (gethash "error" payload))
                       "the server rejected the video URL")))))
    (api-error (condition)
      (values nil (api-error-message condition)))))
