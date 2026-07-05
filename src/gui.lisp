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
   (token-status capi:title-pane
                 :text "Token: not checked"
                 :font *ui-font*
                 :accessor token-status-pane)
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
                         (:title "Video" :width (:character 10))
                         (:title "Status" :width (:character 40)))
              :items '()
              :column-function
              (lambda (entry)
                (list (or (getf entry :quest-name) (getf entry :quest-slug))
                      (format-run-time (getf entry :time-ms))
                      (format nil "~dP~:[~;/PB~]"
                              (getf entry :party-size) (getf entry :pb))
                      (run-video-label entry)
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
                      :selection-callback 'toggle-auto-submit-callback
                      :retract-callback 'toggle-auto-submit-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor auto-submit-check)
   (completion-sound-check capi:check-button
                           :text "Play a sound when a run completes"
                           :selected (config-value :completion-sound)
                           :selection-callback 'toggle-completion-sound-callback
                           :retract-callback 'toggle-completion-sound-callback
                           :callback-type :interface
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
                       :selection-callback 'toggle-record-audio-callback
                       :retract-callback 'toggle-record-audio-callback
                       :callback-type :interface
                       :font *ui-font*
                       :accessor record-audio-check)
   ;; ffmpeg itself is not a setting: the release bundles it next to the
   ;; exe (an override still exists as :ffmpeg-path in config.sexp).
   (update-status-pane capi:title-pane
                       :text (format nil "Version: ~a" (client-version))
                       :font *ui-font*
                       :accessor update-status-pane)
   (auto-update-check capi:check-button
                      :text "Check for updates at startup"
                      :selected (config-value :auto-update)
                      :selection-callback 'toggle-auto-update-callback
                      :retract-callback 'toggle-auto-update-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor auto-update-check)
   (check-updates-button capi:push-button
                         :text "Check for updates now"
                         :callback 'check-updates-callback
                         :callback-type :interface
                         :font *ui-font*)
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
                :text "Save & verify"
                :callback 'save-settings-callback
                :callback-type :interface
                :font *ui-font*)
   (upload-button capi:push-button
                  :text "Upload to YouTube"
                  :callback 'upload-video-callback
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
   (status-row capi:row-layout '(game-status server-status token-status))
   ;; Flow order: upload the video (the copied URL is then attached
   ;; right here), with the folder / site / resubmit as fallbacks.
   (actions-row capi:row-layout
                '(upload-button recordings-folder-button my-runs-button
                  retry-button))
   (runs-tab capi:column-layout
             '(status-row quest-status runs-list actions-row)
             :adjust :left)
   ;; Settings are grouped by how they behave: the Connection fields
   ;; need Save & verify, every checkbox applies immediately.
   (connection-group capi:column-layout
                     '(server-url-input api-token-input save-button)
                     :title "Connection" :title-position :frame
                     :title-font *ui-font* :adjust :left)
   (completion-group capi:column-layout
                     '(auto-submit-check completion-sound-check)
                     :title "When a run completes" :title-position :frame
                     :title-font *ui-font* :adjust :left)
   (recording-row capi:row-layout '(record-dir-display record-dir-button)
                  :adjust :center)
   (recording-group capi:column-layout
                    '(record-check record-audio-check recording-row)
                    :title "Recording" :title-position :frame
                    :title-font *ui-font* :adjust :left)
   (updates-group capi:column-layout
                  '(update-status-pane auto-update-check
                    check-updates-button)
                  :title "Updates" :title-position :frame
                  :title-font *ui-font* :adjust :left)
   (advanced-group capi:column-layout '(trigger-log-check)
                   :title "Advanced" :title-position :frame
                   :title-font *ui-font* :adjust :left)
   (settings-tab capi:column-layout
                 '(connection-group completion-group recording-group
                   updates-group advanced-group)
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
  "Save the Connection fields and verify both against the server.
The checkboxes are not read here: each one applies and saves itself
when toggled."
  (setf (config-value :server-url)
        (string-right-trim "/ " (capi:text-input-pane-text
                                 (server-url-input interface)))
        (config-value :api-token)
        (normalize-token (capi:text-input-pane-text
                          (api-token-input interface))))
  (save-config!)
  (check-server interface)
  (check-token interface :notify t))

(defun toggle-auto-submit-callback (interface)
  "Apply the auto-submit toggle immediately (no Save needed)."
  (setf (config-value :auto-submit)
        (capi:button-selected (auto-submit-check interface)))
  (save-config!))

(defun toggle-completion-sound-callback (interface)
  "Apply the completion-sound toggle immediately (no Save needed)."
  (setf (config-value :completion-sound)
        (capi:button-selected (completion-sound-check interface)))
  (save-config!))

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

;;; The upload flow: one click opens YouTube plus an Explorer window
;;; with the recording selected; when the uploaded video's URL is copied
;;; the poll loop notices (CHECK-CLIPBOARD in main.lisp) and offers to
;;; attach it to the draft, so the site never has to be opened.

(defvar *last-upload-run* nil
  "The entry whose Upload to YouTube button was pressed last: the
preferred target when a copied URL could belong to several runs.")

(defun open-file-in-explorer (path)
  "Open an Explorer window with PATH's file already selected. Explorer
only accepts backslashes and a quoted path after /select,."
  (let ((windows-path (substitute #\\ #\/ (namestring path))))
    (> (fli:pointer-address
        (%shell-execute-args fli:*null-pointer* "open" "explorer.exe"
                             (format nil "/select,\"~a\"" windows-path)
                             fli:*null-pointer* +sw-shownormal+))
       32)))

(defun upload-video-callback (interface)
  "Open the YouTube upload page and an Explorer window with the run's
recording selected. Uses the selected row; with no selection, the newest
run whose video is still unattached (the just-finished-playing case)."
  (let* ((selected (capi:choice-selected-item (runs-list-pane interface)))
         (entry (cond ((and selected (getf selected :video-path)) selected)
                      ((null selected)
                       (find-if (lambda (e)
                                  (and (getf e :video-path)
                                       (not (getf e :video-attached))))
                                (queued-runs))))))
    (cond
      ((and selected (not (getf selected :video-path)))
       (capi:display-message "This run has no saved recording.~%~%Videos are only saved when recording is enabled while the quest is played."))
      ((null entry)
       (capi:display-message "No saved recordings to upload yet.~%~%Videos are saved automatically when a recorded quest completes."))
      ((not (ignore-errors (probe-file (getf entry :video-path))))
       (capi:display-message "The recording file is missing:~%~%~a"
                             (getf entry :video-path)))
      (t
       (setf *last-upload-run* entry)
       (open-file-in-explorer (getf entry :video-path))
       (open-in-browser "https://www.youtube.com/upload")))))

(defun run-choice-label (entry)
  (format nil "~a  ~a"
          (or (getf entry :quest-name) (getf entry :quest-slug))
          (format-run-time (getf entry :time-ms))))

(defun attach-video-in-background (interface entry url)
  "Network round trip off the GUI thread; result lands back on it."
  (mp:process-run-function
   "eta-client-attach-video" '()
   (lambda ()
     (multiple-value-bind (updated error) (attach-video-url! entry url)
       (declare (ignore updated))
       (refresh-runs-list interface)
       (when error
         (capi:execute-with-interface
          interface
          (lambda ()
            (capi:display-message "Could not attach the video:~%~%~a"
                                  error))))))))

(defun offer-clipboard-url (interface url)
  "Confirm (on the GUI thread) which run the copied URL belongs to,
then attach it in a worker process."
  (capi:execute-with-interface
   interface
   (lambda ()
     (let ((target (resolve-video-target (video-candidates)
                                         *last-upload-run*)))
       (cond
         ((null target))
         ((eq target :choose)
          (multiple-value-bind (entry okp)
              (capi:prompt-with-list
               (video-candidates)
               (format nil "A YouTube link was copied:~%~a~%~%Attach it to which run?"
                       url)
               :print-function 'run-choice-label)
            (when (and okp entry)
              (attach-video-in-background interface entry url))))
         (t
          (when (capi:confirm-yes-or-no
                 "Attach the copied YouTube link to this run?~%~%~a~%~a"
                 (run-choice-label target) url)
            (attach-video-in-background interface target url))))))))

(defun open-recordings-folder-callback (interface)
  "Open the recordings folder in Explorer (created on demand so the
button works before the first recording exists)."
  (declare (ignore interface))
  (let ((dir (resolve-record-dir)))
    (ignore-errors (ensure-directories-exist dir))
    (%shell-execute fli:*null-pointer* "open" (namestring dir)
                    fli:*null-pointer* fli:*null-pointer* +sw-shownormal+)))

(defun prompt-for-token-setup (interface)
  "First-run setup: while no API token is configured, offer to open the
token page, take the pasted token right here, save it and verify it
against /api/me. Shown again on every launch until a token is set;
Cancel (or an empty paste) skips it for this launch."
  (when (string= (normalize-token (config-value :api-token)) "")
    (capi:execute-with-interface
     interface
     (lambda ()
       (let ((url (api-url (config-value :server-url) "/my/tokens")))
         (when (capi:confirm-yes-or-no
                "No API token is set yet.~%~%Runs are still timed and listed below, but they can only be uploaded to the site with a token.~%~%Open the token page in your browser now?~%(~a - requires Discord login)"
                url)
           (open-in-browser url))
         (labels ((ask ()
                    (multiple-value-bind (text okp)
                        (capi:prompt-for-string
                         "Paste your API token here (Cancel to skip - you can also set it later in Settings):")
                      (let ((token (and okp (normalize-token text))))
                        (if (or (null token) (string= token ""))
                            (set-pane-text interface #'token-status-pane
                                           "Token: not set")
                            (progn
                              (setf (config-value :api-token) token)
                              (save-config!)
                              (setf (capi:text-input-pane-text
                                     (api-token-input interface))
                                    token)
                              (check-token
                               interface
                               :on-invalid
                               (lambda ()
                                 (capi:execute-with-interface
                                  interface
                                  (lambda ()
                                    (when (capi:confirm-yes-or-no
                                           "The server rejected that token (unauthorized).~%~%Paste it again?")
                                      (ask))))))))))))
           (ask)))))))

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

(defun toggle-record-audio-callback (interface)
  "Apply the audio toggle immediately (no Save needed). Read at
recording start, so it takes effect from the next quest."
  (setf (config-value :record-audio)
        (capi:button-selected (record-audio-check interface)))
  (save-config!))

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

(defun check-token (interface &key on-invalid notify)
  "Verify the configured API token against /api/me on a background
thread and reflect the outcome in the token-status pane. ON-INVALID
runs only on a definite 401 - not on transport errors, where the token
itself may well be fine. NOTIFY also pops the outcome as a dialog, for
the Save settings flow."
  (let ((token (normalize-token (config-value :api-token))))
    (if (string= token "")
        (set-pane-text interface #'token-status-pane "Token: not set")
        (progn
          (set-pane-text interface #'token-status-pane "Token: checking...")
          (mp:process-run-function
           "eta-client-token-check" '()
           (lambda ()
             (handler-case
                 (multiple-value-bind (outcome user) (fetch-me :token token)
                   (ecase outcome
                     (:ok
                      (let ((name (gethash "username" user)))
                        (set-pane-text interface #'token-status-pane
                                       (format nil "Token: OK (~a)" name))
                        (when notify
                          (capi:execute-with-interface
                           interface
                           (lambda ()
                             (capi:display-message
                              "Token OK - authenticated as ~a." name))))))
                     (:unauthorized
                      (set-pane-text interface #'token-status-pane
                                     "Token: invalid or revoked" :red)
                      (when notify
                        (capi:execute-with-interface
                         interface
                         (lambda ()
                           (capi:display-message
                            "The server rejected the API token (unauthorized).~%~%Paste a fresh one from the site's token page."))))
                      (when on-invalid (funcall on-invalid)))))
               (error (condition)
                 (set-pane-text interface #'token-status-pane
                                (token-status-error-text condition) :red)))))))))

;;; Self-update flow. The mechanics (release fetch, download, helper
;;; script handover) live in updater.lisp; this is the GUI choreography:
;;; a startup check fails silently, the Settings button reports every
;;; outcome, and nothing is ever applied while a run or recording is in
;;; flight (note-poll-activity in main.lisp re-offers once idle).

(defun toggle-auto-update-callback (interface)
  "Apply the auto-update toggle immediately (no Save needed)."
  (setf (config-value :auto-update)
        (capi:button-selected (auto-update-check interface)))
  (save-config!))

(defun check-updates-callback (interface)
  (check-for-updates interface :manual t))

(defun set-version-status (interface &optional note color)
  (set-pane-text interface #'update-status-pane
                 (format nil "Version: ~a~@[ - ~a~]" (client-version) note)
                 color))

(defun update-note (interface format-string &rest args)
  "Pop an informational dialog from any thread."
  (let ((text (apply #'format nil format-string args)))
    (capi:execute-with-interface
     interface
     (lambda () (capi:display-message "~a" text)))))

(defun check-for-updates (interface &key manual)
  "Look for a newer release on a background thread and walk the user
through downloading and installing it. MANUAL marks the Settings
button, which reports every outcome; the startup check only ever
surfaces an actual update."
  (cond
    ((and (null *client-version*) (not manual))
     nil)                               ; dev image: never self-checks
    ((and (null *client-version*) manual)
     (update-note interface
                  "This is a dev build (no version baked in), so there is nothing to compare against.~%~%Releases live at:~%~a"
                  (update-release-page-url)))
    (t
     (set-version-status interface "checking for updates...")
     (mp:process-run-function
      "eta-client-update-check" '()
      (lambda ()
        (let ((release (fetch-latest-release)))
          (cond
            ((null release)
             (set-version-status interface (and manual "update check failed"))
             (when manual
               (update-note interface
                            "Could not check for updates - network trouble, GitHub rate limiting, or no release published yet.")))
            ((not (update-available-p *client-version* (getf release :tag)))
             (set-version-status interface "up to date")
             (when manual
               (update-note interface "You are on the latest version (~a)."
                            (client-version))))
            (t
             (offer-update-download interface release)))))))))

(defun offer-update-download (interface release)
  (capi:execute-with-interface
   interface
   (lambda ()
     (if (capi:confirm-yes-or-no
          "Version ~a is available (you have ~a).~%~%Download and install it now?"
          (getf release :tag) (client-version))
         (mp:process-run-function
          "eta-client-update-download" '()
          (lambda () (run-update-download interface release)))
         (set-version-status interface
                             (format nil "~a available (skipped)"
                                     (getf release :tag)))))))

(defun run-update-download (interface release)
  (cond
    ((not (install-dir-writable-p))
     (set-version-status interface "install folder not writable" :red)
     (capi:execute-with-interface
      interface
      (lambda ()
        (when (capi:confirm-yes-or-no
               "The client's folder is not writable, so the update cannot be applied automatically.~%~%Open the download page to update by hand?")
          (open-in-browser (update-release-page-url))))))
    (t
     (let* ((tag (getf release :tag))
            (last-shown -1)
            (zip (download-update!
                  release (update-zip-path)
                  :on-progress
                  (lambda (done total)
                    ;; Once per MB: this runs per 64KB chunk, and every
                    ;; update is a message to the GUI thread.
                    (let ((mb (floor done 1048576)))
                      (when (> mb last-shown)
                        (setf last-shown mb)
                        (set-version-status
                         interface
                         (format nil "downloading ~a... ~d~@[ / ~d~] MB"
                                 tag mb (and total
                                             (ceiling total 1048576))))))))))
       (cond
         ((null zip)
          (set-version-status interface "download failed" :red)
          (update-note interface
                       "The update download failed or did not verify. Nothing was changed; try again later."))
         (*poll-busy-p*
          ;; Never swap the exe mid-run; note-poll-activity re-offers
          ;; the moment the run and its recording are over.
          (setf *update-ready-zip* (cons zip tag))
          (set-version-status interface
                              (format nil "~a downloaded - installs after this run"
                                      tag)))
         (t
          (offer-update-restart interface zip tag)))))))

(defun offer-update-restart (interface zip tag)
  "Final confirmation; on yes the app exits and the helper script swaps
the exe and restarts. Called from the download thread and from the poll
loop (for updates deferred past a run)."
  (set-version-status interface (format nil "~a downloaded" tag))
  (capi:execute-with-interface
   interface
   (lambda ()
     (if (capi:confirm-yes-or-no
          "Update ~a is downloaded and verified.~%~%Restart now to finish installing?"
          tag)
         (launch-updater-and-quit interface zip)
         (set-version-status
          interface
          (format nil "~a downloaded - restart skipped; it will be offered again next launch"
                  tag))))))

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
