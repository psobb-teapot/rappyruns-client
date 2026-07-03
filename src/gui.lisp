(in-package :ephinea-ta-client)

;;; LispWorks CAPI GUI. All pane mutation goes through
;;; CAPI:EXECUTE-WITH-INTERFACE because the poll loop runs on its own
;;; mp:process.

(defparameter *ui-font*
  (gp:make-font-description :family "Segoe UI" :size 12)
  "Font for all panes; the CAPI default on Windows is small and hard to read.")

(defun run-status-label (entry)
  (case (getf entry :status)
    (:queued "queued")
    (:submitted (format nil "draft ~@[#~a~]" (getf entry :url)))
    (:duplicate "duplicate")
    (:rejected (format nil "rejected: ~a" (or (getf entry :reason) "?")))
    (:failed (format nil "failed: ~a" (or (getf entry :reason) "?")))
    (t "?")))

(defun format-run-time (ms)
  (multiple-value-bind (total-seconds msec) (floor ms 1000)
    (multiple-value-bind (minutes seconds) (floor total-seconds 60)
      (format nil "~d:~2,'0d.~3,'0d" minutes seconds msec))))

(capi:define-interface client-window ()
  ()
  (:panes
   (game-status capi:title-pane
                :text "Game: searching..."
                :font *ui-font*
                :accessor game-status-pane)
   (server-status capi:title-pane
                  :text "Server: not checked"
                  :font *ui-font*
                  :accessor server-status-pane)
   (quest-status capi:title-pane
                 :text "No active quest"
                 :font *ui-font*
                 :accessor quest-status-pane)
   (runs-list capi:multi-column-list-panel
              :font *ui-font*
              :header-args (list :font *ui-font*)
              :columns '((:title "Quest" :width (:character 34))
                         (:title "Time" :width (:character 12))
                         (:title "Party" :width (:character 6))
                         (:title "Status" :width (:character 40)))
              :items '()
              :column-function
              (lambda (entry)
                (list (or (getf entry :quest-name) (getf entry :quest-slug))
                      (format-run-time (getf entry :time-ms))
                      (format nil "~dP~:[~;/PB~]"
                              (getf entry :party-size) (getf entry :pb))
                      (run-status-label entry)))
              :accessor runs-list-pane
              :visible-min-height '(:character 8))
   (server-url-input capi:text-input-pane
                     :title "Server URL"
                     :text (config-value :server-url)
                     :font *ui-font*
                     :title-font *ui-font*
                     :accessor server-url-input)
   (api-token-input capi:password-pane
                    :title "API token"
                    :text (config-value :api-token)
                    :font *ui-font*
                    :title-font *ui-font*
                    :accessor api-token-input)
   (auto-submit-check capi:check-button
                      :text "Submit automatically on quest completion"
                      :selected (config-value :auto-submit)
                      :font *ui-font*
                      :accessor auto-submit-check)
   (trigger-log-check capi:check-button
                      :text "Log trigger changes (for finding switch IDs of new categories)"
                      :selected (config-value :trigger-log)
                      :selection-callback 'toggle-trigger-log-callback
                      :retract-callback 'toggle-trigger-log-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor trigger-log-check)
   (record-check capi:check-button
                 :text "Record quest videos automatically (needs ffmpeg)"
                 :selected (config-value :record-enabled)
                 :selection-callback 'toggle-record-callback
                 :retract-callback 'toggle-record-callback
                 :callback-type :interface
                 :font *ui-font*
                 :accessor record-check)
   (ffmpeg-path-input capi:text-input-pane
                      :title "ffmpeg path (blank = bundled/PATH)"
                      :text (config-value :ffmpeg-path)
                      :font *ui-font*
                      :title-font *ui-font*
                      :accessor ffmpeg-path-input)
   (record-dir-input capi:text-input-pane
                     :title "Recordings folder (blank = Videos\\EphineaTA)"
                     :text (config-value :record-dir)
                     :font *ui-font*
                     :title-font *ui-font*
                     :accessor record-dir-input)
   (save-button capi:push-button
                :text "Save settings"
                :callback 'save-settings-callback
                :callback-type :interface
                :font *ui-font*)
   (retry-button capi:push-button
                 :text "Submit pending runs"
                 :callback 'retry-callback
                 :callback-type :interface
                 :font *ui-font*))
  (:layouts
   (status-row capi:row-layout '(game-status server-status))
   (settings-row capi:row-layout '(server-url-input api-token-input))
   (recording-row capi:row-layout '(ffmpeg-path-input record-dir-input))
   (buttons-row capi:row-layout '(save-button retry-button))
   (main-layout capi:column-layout
                '(status-row quest-status runs-list
                  settings-row auto-submit-check trigger-log-check
                  record-check recording-row buttons-row)
                :adjust :left))
  (:default-initargs
   :title "Ephinea TA Client"
   :layout 'main-layout
   :visible-min-width '(:character 100)))

(defun save-settings-callback (interface)
  (setf (config-value :server-url)
        (string-right-trim "/ " (capi:text-input-pane-text
                                 (server-url-input interface)))
        (config-value :api-token)
        (string-trim " " (capi:text-input-pane-text
                          (api-token-input interface)))
        (config-value :auto-submit)
        (capi:button-selected (auto-submit-check interface))
        (config-value :trigger-log)
        (capi:button-selected (trigger-log-check interface))
        (config-value :record-enabled)
        (capi:button-selected (record-check interface))
        (config-value :ffmpeg-path)
        (string-trim " " (capi:text-input-pane-text
                          (ffmpeg-path-input interface)))
        (config-value :record-dir)
        (string-trim " " (capi:text-input-pane-text
                          (record-dir-input interface))))
  (save-config!)
  (check-server interface))

(defun retry-callback (interface)
  (declare (ignore interface))
  ;; The poll loop owns submission; just flag the queue for a retry pass.
  (setf *retry-requested* t))

(defvar *retry-requested* nil)

(defun toggle-trigger-log-callback (interface)
  "Apply the logging toggle immediately (no Save needed) and, when turned
on, start the log file right away so the user can see it is working."
  (let ((on (capi:button-selected (trigger-log-check interface))))
    (setf (config-value :trigger-log) on)
    (save-config!)
    (if on
        (let ((path (start-trigger-log)))
          (capi:display-message
           "Trigger logging is on. Play the segment, then open:~%~%~a~%~%The floor switch (or register) that flips when the room is cleared is your end trigger."
           (namestring path)))
        (close-trigger-log))))

(defun toggle-record-callback (interface)
  "Apply the recording toggle immediately (no Save needed). Turning it
on first adopts the path fields as typed, then verifies that ffmpeg can
actually be started; otherwise the box snaps back off with instructions."
  (let ((on (capi:button-selected (record-check interface))))
    (when on
      (setf (config-value :ffmpeg-path)
            (string-trim " " (capi:text-input-pane-text
                              (ffmpeg-path-input interface)))
            (config-value :record-dir)
            (string-trim " " (capi:text-input-pane-text
                              (record-dir-input interface))))
      (unless (ffmpeg-available-p)
        (setf (capi:button-selected (record-check interface)) nil
              on nil)
        (capi:display-message
         "ffmpeg was not found, so recording stays off.~%~%Either use the client zip that bundles it (ffmpeg\\ffmpeg.exe next to the exe), or install ffmpeg yourself and put its full path into \"ffmpeg path\".")))
    (setf (config-value :record-enabled) on)
    (save-config!)))

(defun set-pane-text (interface accessor text)
  (capi:execute-with-interface
   interface
   (lambda ()
     (setf (capi:title-pane-text (funcall accessor interface)) text))))

(defun refresh-runs-list (interface)
  (let ((runs (queued-runs)))
    (capi:execute-with-interface
     interface
     (lambda ()
       (setf (capi:collection-items (runs-list-pane interface))
             (coerce runs 'vector))))))

(defun check-server (interface)
  "Fetch quests, load any moderator-defined detection categories, and
cross-check the builtin trigger slugs against the server."
  (mp:process-run-function
   "eta-client-server-check" '()
   (lambda ()
     (handler-case
         (let* ((quests (fetch-quests))
                (server-defs (set-server-quest-defs quests))
                (slugs (loop :for quest :across quests
                             :collect (gethash "slug" quest)))
                (unknown (unknown-slugs slugs *builtin-quest-defs*)))
           (set-pane-text interface #'server-status-pane
                          (format nil "Server: OK (~d quests, ~d timed category~:p~@[; ~d local trigger~:p unknown~])"
                                  (length quests) server-defs
                                  (and (plusp (length unknown)) (length unknown)))))
       (error (condition)
         (set-pane-text interface #'server-status-pane
                        (format nil "Server: ~a" condition)))))))

(defun update-game-status (interface connected-p detector snapshot
                           &optional recorder)
  (set-pane-text interface #'game-status-pane
                 (format nil "~a~@[ / recording error: ~a~]"
                         (if connected-p "Game: attached" "Game: searching...")
                         (and recorder (recorder-last-error recorder))))
  (set-pane-text
   interface #'quest-status-pane
   (let ((recording-p (and recorder
                           (eq (recorder-state recorder) :recording))))
     (cond
       ((eq (detector-state detector) :in-quest)
        (let ((extra (1- (detector-active-count detector))))
          (format nil "~a~@[ (+~d)~] - ~a~:[~; [REC]~]"
                  (quest-def-slug (detector-active-def detector))
                  (and (plusp extra) extra)
                  (format-run-time (detector-elapsed-ms detector))
                  recording-p)))
       ((and snapshot (getf snapshot :quest-name))
        (format nil "~a (waiting for start)" (getf snapshot :quest-name)))
       (t "No active quest")))))
