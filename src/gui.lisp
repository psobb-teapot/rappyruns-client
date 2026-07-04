(in-package :ephinea-ta-client)

;;; LispWorks CAPI GUI. All pane mutation goes through
;;; CAPI:EXECUTE-WITH-INTERFACE because the poll loop runs on its own
;;; mp:process.

(defparameter *ui-font*
  (gp:make-font-description :family "Segoe UI" :size 12)
  "Font for all panes; the CAPI default on Windows is small and hard to read.")

;; RUN-STATUS-LABEL and FORMAT-RUN-TIME live in store.lisp (pure CL,
;; covered by the SBCL tests).

(defun open-in-browser (url)
  "Open URL in the default browser. Only http(s) URLs; never signals."
  (and (valid-http-url-p url)
       (> (fli:pointer-address
           (%shell-execute fli:*null-pointer* "open" url
                           fli:*null-pointer* fli:*null-pointer*
                           +sw-shownormal+))
          32)))

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
              :action-callback 'runs-list-action-callback
              :callback-type :data-interface
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
   (completion-sound-check capi:check-button
                           :text "Play a sound when a run completes"
                           :selected (config-value :completion-sound)
                           :font *ui-font*
                           :accessor completion-sound-check)
   (trigger-log-check capi:check-button
                      :text "Log trigger changes (for finding switch IDs of new categories)"
                      :selected (config-value :trigger-log)
                      :selection-callback 'toggle-trigger-log-callback
                      :retract-callback 'toggle-trigger-log-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor trigger-log-check)
   (record-check capi:check-button
                 :text "Record quest videos automatically"
                 :selected (config-value :record-enabled)
                 :selection-callback 'toggle-record-callback
                 :retract-callback 'toggle-record-callback
                 :callback-type :interface
                 :font *ui-font*
                 :accessor record-check)
   (record-audio-check capi:check-button
                       :text "Record game audio (only the game is heard, not Discord etc.)"
                       :selected (config-value :record-audio)
                       :font *ui-font*
                       :accessor record-audio-check)
   ;; ffmpeg itself is not a setting: the release bundles it next to the
   ;; exe (an override still exists as :ffmpeg-path in config.sexp).
   (record-dir-display capi:title-pane
                       :text (record-dir-label)
                       :font *ui-font*
                       :accessor record-dir-display)
   (record-dir-button capi:push-button
                      :text "Change folder..."
                      :callback 'choose-record-dir-callback
                      :callback-type :interface
                      :font *ui-font*)
   (save-button capi:push-button
                :text "Save settings"
                :callback 'save-settings-callback
                :callback-type :interface
                :font *ui-font*)
   (recordings-folder-button capi:push-button
                             :text "Open recordings folder"
                             :callback 'open-recordings-folder-callback
                             :callback-type :interface
                             :font *ui-font*)
   (my-runs-button capi:push-button
                   :text "Open My Runs (add videos)"
                   :callback 'open-my-runs-callback
                   :callback-type :interface
                   :font *ui-font*)
   (retry-button capi:push-button
                 :text "Submit pending runs"
                 :callback 'retry-callback
                 :callback-type :interface
                 :font *ui-font*))
  ;; Two tabs mirror how the app is used: Settings once up front, then
  ;; the Runs tab for the daily play -> check video -> submit flow.
  (:layouts
   (status-row capi:row-layout '(game-status server-status))
   ;; Flow order: grab the video, attach it on the site, resubmit stragglers.
   (actions-row capi:row-layout
                '(recordings-folder-button my-runs-button retry-button))
   (runs-tab capi:column-layout
             '(status-row quest-status runs-list actions-row)
             :adjust :left)
   (settings-row capi:row-layout '(server-url-input api-token-input))
   (recording-row capi:row-layout '(record-dir-display record-dir-button)
                  :adjust :center)
   (settings-tab capi:column-layout
                 '(settings-row auto-submit-check completion-sound-check
                   record-check record-audio-check recording-row
                   trigger-log-check save-button)
                 :adjust :left)
   (main-tabs capi:tab-layout ()
              :items '(("Runs" runs-tab) ("Settings" settings-tab))
              :print-function 'first
              :visible-child-function 'second
              ;; First launch without a token lands on Settings (step 1
              ;; of the flow); otherwise straight to the Runs tab.
              :selection (if (string= (config-value :api-token) "") 1 0)))
  (:default-initargs
   :title "Ephinea TA Client"
   :layout 'main-tabs
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
        (config-value :completion-sound)
        (capi:button-selected (completion-sound-check interface))
        (config-value :trigger-log)
        (capi:button-selected (trigger-log-check interface))
        (config-value :record-enabled)
        (capi:button-selected (record-check interface))
        (config-value :record-audio)
        (capi:button-selected (record-audio-check interface)))
  (save-config!)
  (check-server interface))

(defun record-dir-label ()
  (format nil "Recordings folder: ~a" (namestring (resolve-record-dir))))

(defun choose-record-dir-callback (interface)
  "Pick the recordings folder with the system directory dialog; applied
and saved immediately (no Save settings needed)."
  (let ((dir (capi:prompt-for-directory "Choose the recordings folder"
                                        :pathname (resolve-record-dir))))
    (when dir
      (setf (config-value :record-dir) (namestring dir))
      (save-config!)
      (setf (capi:title-pane-text (record-dir-display interface))
            (record-dir-label)))))

(defun retry-callback (interface)
  (declare (ignore interface))
  ;; The poll loop owns submission; just flag the queue for a retry pass.
  (setf *retry-requested* t))

(defun runs-list-action-callback (entry interface)
  "Double-click / Enter on a run: open it on the site (drafts get their
video attached there). Rows that were never submitted have no URL and
do nothing."
  (declare (ignore interface))
  (let ((url (getf entry :url)))
    (when url
      (open-in-browser url))))

(defun open-my-runs-callback (interface)
  (declare (ignore interface))
  (open-in-browser (api-url (config-value :server-url) "/my/runs")))

(defun open-recordings-folder-callback (interface)
  "Open the recordings folder in Explorer (created on demand so the
button works before the first recording exists)."
  (declare (ignore interface))
  (let ((dir (resolve-record-dir)))
    (ignore-errors (ensure-directories-exist dir))
    (%shell-execute fli:*null-pointer* "open" (namestring dir)
                    fli:*null-pointer* fli:*null-pointer* +sw-shownormal+)))

(defun maybe-prompt-for-token (interface)
  "One-time nudge when no API token is configured: without it runs are
timed but can never be uploaded. Never shown again once answered."
  (when (and (string= (config-value :api-token) "")
             (not (config-value :token-prompt-shown)))
    (setf (config-value :token-prompt-shown) t)
    (save-config!)
    (capi:execute-with-interface
     interface
     (lambda ()
       (let ((url (api-url (config-value :server-url) "/my/tokens")))
         (when (capi:confirm-yes-or-no
                "No API token is set yet.~%~%Runs are still timed and listed below, but they can only be uploaded to the site once a token is pasted into \"API token\" (then press Save settings).~%~%Open the token page in your browser now?~%(~a - requires Discord login)"
                url)
           (open-in-browser url)))))))

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
on verifies that ffmpeg can actually be started; otherwise the box
snaps back off with instructions."
  (let ((on (capi:button-selected (record-check interface))))
    (when (and on (not (ffmpeg-available-p)))
      (setf (capi:button-selected (record-check interface)) nil
            on nil)
      (capi:display-message
       "ffmpeg was not found, so recording stays off.~%~%Use the client zip that bundles it (ffmpeg\\ffmpeg.exe next to the exe), or install ffmpeg so it is on PATH."))
    (setf (config-value :record-enabled) on)
    (save-config!)))

(defun set-pane-text (interface accessor text &optional foreground)
  "Update a title pane's text and foreground color (NIL = default) from
any thread. Errors get :red so they stand out from routine status."
  (capi:execute-with-interface
   interface
   (lambda ()
     (let ((pane (funcall accessor interface)))
       (setf (capi:title-pane-text pane) text)
       (setf (capi:simple-pane-foreground pane) foreground)))))

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
                        (server-status-error-text condition) :red))))))

(defvar *last-window-title* nil
  "Cache so the 4x-per-second GUI tick only calls SetWindowText on change.")

(defun set-window-title (interface title)
  (unless (equal title *last-window-title*)
    (setf *last-window-title* title)
    (capi:execute-with-interface
     interface
     (lambda ()
       (setf (capi:interface-title interface) title)))))

(defun update-game-status (interface connected-p detector snapshot
                           &optional recorder)
  (let ((recording-error (and recorder (recorder-last-error recorder)))
        (recording-p (and recorder
                          (eq (recorder-state recorder) :recording)))
        (in-quest-p (eq (detector-state detector) :in-quest)))
    (set-pane-text interface #'game-status-pane
                   (format nil "~a~@[ / recording error: ~a~]"
                           (if connected-p "Game: attached" "Game: searching...")
                           recording-error)
                   (and recording-error :red))
    (set-pane-text
     interface #'quest-status-pane
     (cond
       (in-quest-p
        (let ((extra (1- (detector-active-count detector))))
          (format nil "~a~@[ (+~d)~] - ~a~:[~; [REC]~]"
                  (quest-def-slug (detector-active-def detector))
                  (and (plusp extra) extra)
                  (format-run-time (detector-elapsed-ms detector))
                  recording-p)))
       ((and snapshot (getf snapshot :quest-name))
        (format nil "~a (waiting for start)" (getf snapshot :quest-name)))
       (t "No active quest")))
    ;; The taskbar truncates from the right, so the time goes first.
    (set-window-title
     interface
     (if in-quest-p
         (format nil "~a~:[~; [REC]~] - Ephinea TA Client"
                 (format-run-time (detector-elapsed-ms detector))
                 recording-p)
         "Ephinea TA Client"))))
