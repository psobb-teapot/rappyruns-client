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
the list; unfinished ones (:queued/:failed) are never dropped.")

(defun trim-finished-runs (runs limit)
  "RUNS is newest first: keep every :queued/:failed entry and only the
newest LIMIT finished ones, preserving order."
  (let ((finished 0))
    (remove-if (lambda (entry)
                 (and (not (member (getf entry :status) '(:queued :failed)))
                      (> (incf finished) limit)))
               runs)))

;;; Display helpers for run entries. They live here rather than in the
;;; (LispWorks-only) GUI so the SBCL tests can cover them.

(defun format-run-time (ms)
  (multiple-value-bind (total-seconds msec) (floor ms 1000)
    (multiple-value-bind (minutes seconds) (floor total-seconds 60)
      (format nil "~d:~2,'0d.~3,'0d" minutes seconds msec))))

(defun run-status-label (entry)
  (case (getf entry :status)
    (:queued "queued")
    (:submitted "draft - double-click to add video")
    (:duplicate "duplicate (already on server)")
    (:rejected (format nil "rejected: ~a" (or (getf entry :reason) "?")))
    (:failed (format nil "failed: ~a" (or (getf entry :reason) "?")))
    (t "?")))

(defun queue-path ()
  (merge-pathnames "queue.sexp" (config-dir)))

(defun persistable (run)
  ;; Only unfinished business needs to survive a restart.
  (member (getf run :status) '(:queued :failed)))

(defun save-queue! ()
  (write-sexp-file (queue-path)
                   (with-runs-lock (remove-if-not #'persistable *runs*))))

(defun load-queue! ()
  (let ((saved (read-sexp-file (queue-path))))
    (when (listp saved)
      (with-runs-lock
        (setf *runs* (append *runs* saved))))))

(defun queued-runs ()
  (with-runs-lock (copy-list *runs*)))

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

(defun submit-entry! (entry)
  "Submit one queue entry. Returns the updated entry."
  (handler-case
      (multiple-value-bind (outcome payload) (submit-run entry)
        (let ((url (and (hash-table-p payload) (gethash "url" payload)))
              (errors (and (hash-table-p payload) (gethash "errors" payload)))
              (message (and (hash-table-p payload) (gethash "message" payload))))
          (ecase outcome
            (:created (update-run! entry :status :submitted :url url))
            (:duplicate (update-run! entry :status :duplicate :url url))
            (:rejected
             (update-run! entry :status :rejected
                          :reason (format nil "~@[~a ~]~@[~{~a~^; ~}~]"
                                          message
                                          (and errors (coerce errors 'list))))))))
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
