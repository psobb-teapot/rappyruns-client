(in-package :ephinea-ta-client)

;;; LispWorks CAPI GUI. All pane mutation goes through
;;; CAPI:EXECUTE-WITH-INTERFACE because the poll loop runs on its own
;;; mp:process.

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
                :accessor game-status-pane)
   (server-status capi:title-pane
                  :text "Server: not checked"
                  :accessor server-status-pane)
   (quest-status capi:title-pane
                 :text "No active quest"
                 :accessor quest-status-pane)
   (runs-list capi:multi-column-list-panel
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
                     :accessor server-url-input)
   (api-token-input capi:password-pane
                    :title "API token"
                    :text (config-value :api-token)
                    :accessor api-token-input)
   (auto-submit-check capi:check-button
                      :text "Submit automatically on quest completion"
                      :selected (config-value :auto-submit)
                      :accessor auto-submit-check)
   (save-button capi:push-button
                :text "Save settings"
                :callback 'save-settings-callback
                :callback-type :interface)
   (retry-button capi:push-button
                 :text "Submit pending runs"
                 :callback 'retry-callback
                 :callback-type :interface))
  (:layouts
   (status-row capi:row-layout '(game-status server-status))
   (settings-row capi:row-layout '(server-url-input api-token-input))
   (buttons-row capi:row-layout '(save-button retry-button))
   (main-layout capi:column-layout
                '(status-row quest-status runs-list
                  settings-row auto-submit-check buttons-row)
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
        (capi:button-selected (auto-submit-check interface)))
  (save-config!)
  (check-server interface))

(defun retry-callback (interface)
  (declare (ignore interface))
  ;; The poll loop owns submission; just flag the queue for a retry pass.
  (setf *retry-requested* t))

(defvar *retry-requested* nil)

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
  "Verify connectivity and cross-check trigger slugs against the server."
  (mp:process-run-function
   "eta-client-server-check" '()
   (lambda ()
     (handler-case
         (let* ((quests (fetch-quests))
                (slugs (loop :for quest :across quests
                             :collect (gethash "slug" quest)))
                (unknown (unknown-slugs slugs)))
           (set-pane-text interface #'server-status-pane
                          (if unknown
                              (format nil "Server: OK (~d quests; ~d local trigger~:p unknown to server)"
                                      (length quests) (length unknown))
                              (format nil "Server: OK (~d quests)" (length quests)))))
       (error (condition)
         (set-pane-text interface #'server-status-pane
                        (format nil "Server: ~a" condition)))))))

(defun update-game-status (interface connected-p detector snapshot)
  (set-pane-text interface #'game-status-pane
                 (if connected-p "Game: attached" "Game: searching..."))
  (set-pane-text
   interface #'quest-status-pane
   (cond
     ((eq (detector-state detector) :in-quest)
      (format nil "~a - ~a"
              (first (quest-def-names (detector-quest-def detector)))
              (format-run-time (detector-elapsed-ms detector))))
     ((and snapshot (getf snapshot :quest-name))
      (format nil "~a (waiting for start)" (getf snapshot :quest-name)))
     (t "No active quest"))))
