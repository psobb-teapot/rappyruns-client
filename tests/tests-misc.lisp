(in-package :ephinea-ta-client-tests)

;;; ------------------------------------------------------------------
;;; Authenticode trust policy (the Win32 verification itself is
;;; LispWorks-only; the decision and the GUI label are pure)
;;; ------------------------------------------------------------------

(defun run-signature-policy-tests ()
  (format t "~&--- signature policy ---~%")
  (check "the official signer is accepted"
         (psobb-signature-trusted-p :valid "Terry Chatman"))
  (check "a valid signature from an unknown signer is refused"
         (not (psobb-signature-trusted-p :valid "Mallory")))
  (check "an unsigned exe is refused"
         (not (psobb-signature-trusted-p :unsigned nil)))
  (check "a broken signature is refused even with the right name"
         (not (psobb-signature-trusted-p :invalid "Terry Chatman")))
  (check "a missing signer name is refused"
         (not (psobb-signature-trusted-p :valid nil)))
  (check "the rejection label names an untrusted signer"
         (let ((ephinea-ta-client::*language* :en))
           (search "Mallory"
                   (ephinea-ta-client::signature-status-label
                    '(:pid 1 :status :valid :signer "Mallory")))))
  (check "the rejection label explains an unsigned exe"
         (let ((ephinea-ta-client::*language* :en))
           (string= "no signature"
                    (ephinea-ta-client::signature-status-label
                     '(:pid 1 :status :unsigned)))))
  (check "any other status reads as unverifiable"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "署名を検証できません"
                    (ephinea-ta-client::signature-status-label
                     '(:pid 1 :status :invalid))))))

;;; ------------------------------------------------------------------
;;; i18n: the UI string table and TR
;;; ------------------------------------------------------------------

(defun run-i18n-tests ()
  (format t "~&--- i18n ---~%")
  (check "every key has an English and a Japanese string"
         (loop :for (key entry) :on ephinea-ta-client::*strings* :by #'cddr
               :always (and (keywordp key)
                            (= 2 (length entry))
                            (stringp (first entry))
                            (stringp (second entry)))))
  (check "default language is English"
         (string= "Settings" (ephinea-ta-client::tr :tab-settings)))
  (check "tr switches to Japanese"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "設定" (ephinea-ta-client::tr :tab-settings))))
  (check "tr formats arguments"
         (search "teapot" (ephinea-ta-client::tr :token-ok "teapot")))
  (check "labels follow the language too"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "保存済み" (ephinea-ta-client::run-video-label
                                (list :video-path "v.mp4")))))
  (check "an invalid configured language falls back to English"
         (and (eq :en (ephinea-ta-client::valid-language "nonsense"))
              (eq :ja (ephinea-ta-client::valid-language :ja))))
  ;; Directive mismatches between the two columns would error at
  ;; runtime in one language only; format every entry in both.
  (check "every entry formats cleanly in both languages"
         (loop :for language :in ephinea-ta-client::*languages*
               :always (let ((ephinea-ta-client::*language* language))
                         (loop :for (key entry) :on ephinea-ta-client::*strings*
                               :by #'cddr
                               :always (stringp
                                        (ignore-errors
                                          (ephinea-ta-client::tr key 1 2 3))))))))

;;; ------------------------------------------------------------------
;;; Automatic upload queue: candidate selection, backoff and labels
;;; ------------------------------------------------------------------

(defun run-upload-queue-tests ()
  (format t "~&--- automatic upload queue ---~%")
  (let ((video (merge-pathnames (format nil "eta-test-video-~d.mp4"
                                        (get-internal-real-time))
                                (uiop:temporary-directory)))
        (now (get-universal-time)))
    (with-open-file (out video :direction :output :if-exists :supersede)
      (write-string "not really mp4" out))
    (unwind-protect
         (let ((path (namestring video)))
           ;; *RUNS* is newest first; the candidate scan wants the oldest.
           (with-test-store ((list :status :submitted :server-id 3
                                   :video-path path)
                             (list :status :submitted :server-id 2
                                   :video-path path :video-attached t)
                             (list :status :submitted :server-id 1
                                   :video-path path))
             (check "the oldest unattached recording uploads first"
                    (eql 1 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path path
                                   :next-upload-at (+ now 900)))
             (check "a backing-off entry is skipped"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id)))
             (check "the backoff expires with time"
                    (eql 1 (getf (ephinea-ta-client::upload-candidate
                                  :now (+ now 1000))
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 1
                                   :video-path path :upload-given-up t))
             (check "a given-up entry is never a candidate"
                    (null (ephinea-ta-client::upload-candidate :now now))))
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path "C:/nowhere/gone.mp4"))
             (check "a vanished recording gives up and the scan moves on"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id)))
             (check "the vanished entry is marked given up"
                    (getf (find 1 (queued-runs)
                                :key (lambda (entry) (getf entry :server-id)))
                          :upload-given-up)))
           (with-test-store ((list :status :queued :video-path path))
             (check "entries without a server draft cannot upload yet"
                    (null (ephinea-ta-client::upload-candidate :now now))))
           ;; Aborted runs keep their recording locally but never
           ;; auto-upload it (reset-farming would flood hosted storage).
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path path :aborted t))
             (check "an aborted run's recording never uploads"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 1
                                   :video-path path :aborted t))
             (check "an aborted-only queue has no upload candidate"
                    (null (ephinea-ta-client::upload-candidate :now now))))
           ;; A recording can coexist with :unranked when tracking-only
           ;; mode was switched on mid-quest; the server refuses its
           ;; upload, so it must never become a candidate.
           (with-test-store ((list :status :submitted :server-id 1
                                   :video-path path :unranked t))
             (check "an unranked run's recording never uploads"
                    (null (ephinea-ta-client::upload-candidate :now now)))))
      (ignore-errors (delete-file video))))
  ;; The upload URL carries the recorder's video offset when known.
  (check "video-file url carries the offset when known"
         (equal "/api/runs/7/video-file?offset_ms=1234"
                (ephinea-ta-client::video-file-path 7 1234)))
  (check "video-file url omits the offset when unknown"
         (equal "/api/runs/7/video-file"
                (ephinea-ta-client::video-file-path 7 nil)))
  ;; Given-up entries are finished: trimmed, not persisted.
  (check "a given-up upload is no longer active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 1 :video-path "v.mp4"
                     :upload-given-up t))))
  (check "an aborted run with a pending video is not active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 1 :video-path "v.mp4"
                     :aborted t))))
  (check "an unranked run with a pending video is not active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 1 :video-path "v.mp4"
                     :unranked t))))
  (check "status label: aborted drafts say the recording stays local"
         (search "aborted" (ephinea-ta-client::run-status-label
                            (list :status :submitted :aborted t
                                  :video-path "v.mp4"))))
  (check "status label: unranked drafts read record only"
         (search "record only" (ephinea-ta-client::run-status-label
                                (list :status :submitted :unranked t))))
  ;; Tracking-only mode stamps runs at enqueue time (APPLY-TRACKING-MODE).
  (let ((run (make-test-run)))
    (check "tracking mode off leaves the run untouched"
           (eq run (ephinea-ta-client::apply-tracking-mode
                    run :tracking-only nil :tracking-private nil)))
    (let ((stamped (ephinea-ta-client::apply-tracking-mode
                    run :tracking-only t :tracking-private nil)))
      (check "tracking mode stamps :unranked"
             (and (getf stamped :unranked)
                  (not (getf stamped :run-private)))))
    (let ((stamped (ephinea-ta-client::apply-tracking-mode
                    run :tracking-only t :tracking-private t)))
      (check "the private sub-setting stamps :run-private too"
             (and (getf stamped :unranked) (getf stamped :run-private)))))
  (let ((aborted (make-test-run :aborted t)))
    (check "aborted runs pass through tracking mode untouched"
           (eq aborted (ephinea-ta-client::apply-tracking-mode
                        aborted :tracking-only t :tracking-private t))))
  ;; Labels around the upload lifecycle.
  (let ((ephinea-ta-client::*upload-progress* (list 7 50 200)))
    (check "video label: in-flight upload shows its percent"
           (search "25%" (ephinea-ta-client::run-video-label
                          (list :server-id 7 :video-path "v.mp4"))))
    (check "video label: other entries are not uploading"
           (equal "saved" (ephinea-ta-client::run-video-label
                           (list :server-id 8 :video-path "v.mp4")))))
  (check "video label: uploaded"
         (equal "uploaded" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t
                                  :video-uploaded t))))
  (check "video label: manual attach still reads attached"
         (equal "attached" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t))))
  (check "video label: given up"
         (equal "upload failed" (ephinea-ta-client::run-video-label
                                 (list :video-path "v.mp4"
                                       :upload-given-up t)))))

;;; ------------------------------------------------------------------
;;; local recordings storage budget (recording.lisp + store.lisp)
;;; ------------------------------------------------------------------

(defun run-retention-tests ()
  (format t "~&--- local storage budget ---~%")
  (flet ((evict (files cap &rest kw)
           (apply #'ephinea-ta-client::recordings-to-evict files cap kw)))
    ;; (namestring size-bytes write-date)
    (let ((files '(("a.mp4" 500 100) ("b.mp4" 500 200) ("c.mp4" 500 300))))
      (check "no cap set: nothing is evicted"
             (null (evict files nil)))
      (check "under the cap: nothing is evicted"
             (null (evict files 2000)))
      (check "over the cap: the oldest go until back under"
             (equal '("a.mp4") (evict files 1200)))
      (check "eviction keeps going until the total fits"
             (equal '("a.mp4" "b.mp4") (evict files 600)))
      (check "protected files are never evicted"
             (equal '("b.mp4" "c.mp4")
                    (evict files 100 :protected '("a.mp4"))))
      (check "when only protected files remain, eviction stops short"
             (equal '("c.mp4")
                    (evict files 100 :protected '("a.mp4" "b.mp4"))))
      (check "uploaded files are reclaimed before the rest"
             ;; c is newest but uploaded, so it goes before older a/b.
             (equal '("c.mp4") (evict files 1200 :uploaded '("c.mp4"))))
      (check "uploaded first, then oldest-first among the rest"
             (equal '("c.mp4" "a.mp4")
                    (evict files 600 :uploaded '("c.mp4"))))))
  ;; The queue drives which on-disk files are protected vs reclaimable.
  (with-test-store ((list :status :submitted :server-id 4
                          :video-path "up.mp4" :video-attached t)
                    (list :status :submitted :server-id 3
                          :video-path "pending.mp4")
                    (list :status :submitted :server-id 2
                          :video-path "aborted.mp4" :aborted t)
                    (list :status :submitted :server-id 1
                          :video-path "unranked.mp4" :unranked t))
    (multiple-value-bind (protected uploaded)
        (ephinea-ta-client::video-path-retention-sets)
      (check "a file awaiting upload is protected"
             (member "pending.mp4" protected :test #'equal))
      (check "an uploaded file is reclaimable first"
             (member "up.mp4" uploaded :test #'equal))
      (check "an aborted run's video is neither protected nor uploaded"
             (and (not (member "aborted.mp4" protected :test #'equal))
                  (not (member "aborted.mp4" uploaded :test #'equal))))
      (check "an unranked run's video is not protected either"
             (and (not (member "unranked.mp4" protected :test #'equal))
                  (not (member "unranked.mp4" uploaded :test #'equal))))))
  ;; End to end: the sweep reaps the uploaded file first, spares the
  ;; pending one, and reaches the unmatched file only when still over.
  (let* ((backend (make-instance 'mock-backend))
         (recorder (make-recorder :backend backend))
         (byte-cap (lambda (bytes) (/ bytes (* 1024 1024 1024)))))
    (setf (mock-recordings backend)
          '(("up.mp4" 500 100) ("pending.mp4" 500 200) ("orphan.mp4" 500 300)))
    (with-recording-config (:record-max-total-gb (funcall byte-cap 400))
      (with-test-store ((list :status :submitted :server-id 2
                              :video-path "up.mp4" :video-attached t)
                        (list :status :submitted :server-id 1
                              :video-path "pending.mp4"))
        (ephinea-ta-client::apply-recording-retention recorder)
        (let ((deleted (mapcar #'second (events-of backend :delete))))
          (check "uploaded and orphan files are reaped, pending is spared"
                 (equal '("up.mp4" "orphan.mp4") deleted))))))
  ;; A capture in progress means the disk is busy; the sweep waits.
  (let* ((backend (make-instance 'mock-backend))
         (recorder (make-recorder :backend backend :state :recording)))
    (setf (mock-recordings backend) '(("a.mp4" 500 100)))
    (with-recording-config (:record-max-total-gb (/ 1 (* 1024 1024 1024)))
      (with-test-store ()
        (ephinea-ta-client::apply-recording-retention recorder)
        (check "no sweep runs while a capture is in progress"
               (null (events-of backend :delete)))))))

;;; ------------------------------------------------------------------
;;; login.txt credentials parsing
;;; ------------------------------------------------------------------

(defun run-credentials-tests ()
  (format t "~&--- login.txt credentials ---~%")
  (multiple-value-bind (username password)
      (parse-credentials (format nil "username=Teapot~%password=secret123~%"))
    (check "plain username= and password= lines parse"
           (and (equal "Teapot" username) (equal "secret123" password))))
  (multiple-value-bind (username password)
      (parse-credentials
       (format nil "~a# comment~a~%  username = Teapot ~a~%password=a=b=c~a~%"
               (code-char #xFEFF) #\Return #\Return #\Return))
    (check "BOM, CRLF, comments and spaces around keys are tolerated"
           (and (equal "Teapot" username) (equal "a=b=c" password))))
  (check "a missing password yields NIL NIL"
         (null (parse-credentials (format nil "username=Teapot~%"))))
  (check "empty values yield NIL NIL"
         (null (parse-credentials
                (format nil "username=Teapot~%password=~%"))))
  (check "empty text yields NIL NIL"
         (null (parse-credentials "")))
  (check "NIL text yields NIL NIL"
         (null (parse-credentials nil)))
  (let ((path (merge-pathnames (format nil "eta-test-login-~d.txt"
                                       (get-internal-real-time))
                               (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out "username=Teapot~%password=secret123~%"))
    (unwind-protect
         (multiple-value-bind (username password) (read-credentials path)
           (check "read-credentials reads a real file"
                  (and (equal "Teapot" username)
                       (equal "secret123" password))))
      (ignore-errors (delete-file path)))
    (check "read-credentials on a missing file yields NIL NIL"
           (null (read-credentials path)))))

;;; ------------------------------------------------------------------
;;; Capture diagnostics (recording.lisp): the log tail that follows a
;;; video upload to the server
;;; ------------------------------------------------------------------

(defun run-diagnostics-tests ()
  (let ((path (merge-pathnames (format nil "eta-test-diag-~d.log"
                                       (get-internal-real-time))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
             (format out "line one~%line two 黒画面~%"))
           (check "file-tail returns a short file whole"
                  (let ((tail (ephinea-ta-client::file-tail path 1000)))
                    (and tail (search "line one" tail)
                         (search "黒画面" tail))))
           (check "file-tail keeps the newest end of a long file"
                  (let ((tail (ephinea-ta-client::file-tail path 12)))
                    (and (eql 12 (length tail))
                         (search "黒画面" tail)
                         (not (search "line one" tail))))))
      (ignore-errors (delete-file path)))
    (check "file-tail on a missing file is NIL, not an error"
           (null (ephinea-ta-client::file-tail path 100))))
  ;; The report itself must never signal, whatever the machine lacks
  ;; (SBCL here has no RAM reading and usually no recording log).
  (let ((report (ephinea-ta-client::diagnostics-report)))
    (check "diagnostics-report assembles a header and a log section"
           (and (stringp report)
                (search "client " report)
                (search "recording log tail" report)))))

