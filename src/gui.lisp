(in-package :ephinea-ta-client)

;;; LispWorks CAPI GUI. All pane mutation goes through
;;; CAPI:EXECUTE-WITH-INTERFACE because the poll loop runs on its own
;;; mp:process.

(defparameter *ui-font*
  (gp:make-font-description :family "Segoe UI" :size 12)
  "Font for all panes; the CAPI default on Windows is small and hard to read.")

(defvar *interface* nil
  "The live CLIENT-WINDOW. A global rather than a closure argument so
the language toggle can replace the window (see REBUILD-INTERFACE); the
poll loop re-reads it every iteration.")

(defvar *last-window-title* nil
  "Cache so the 4x-per-second GUI tick only calls SetWindowText on change.")

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
                :text (tr :game-searching)
                :font *ui-font*
                :accessor game-status-pane)
   (server-status capi:title-pane
                  :text (tr :server-not-checked)
                  :font *ui-font*
                  :accessor server-status-pane)
   (token-status capi:title-pane
                 :text (tr :token-not-checked)
                 :font *ui-font*
                 :accessor token-status-pane)
   (quest-status capi:title-pane
                 :text (tr :no-active-quest)
                 :font *ui-font*
                 :accessor quest-status-pane)
   (runs-list capi:multi-column-list-panel
              :font *ui-font*
              :header-args (list :font *ui-font*)
              :columns `((:title ,(tr :col-quest) :width (:character 34))
                         (:title ,(tr :col-time) :width (:character 12))
                         (:title ,(tr :col-party) :width (:character 6))
                         (:title ,(tr :col-video) :width (:character 10))
                         (:title ,(tr :col-status) :width (:character 40)))
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
   (language-radio capi:radio-button-panel
                   :items *languages*
                   :print-function 'language-label
                   :selected-item *language*
                   :selection-callback 'language-changed-callback
                   :callback-type :interface
                   :font *ui-font*
                   :accessor language-radio)
   (server-url-input capi:text-input-pane
                     :title (tr :server-url-label)
                     :text (config-value :server-url)
                     :font *ui-font*
                     :title-font *ui-font*
                     :accessor server-url-input)
   (api-token-input capi:password-pane
                    :title (tr :api-token-label)
                    :text (config-value :api-token)
                    :font *ui-font*
                    :title-font *ui-font*
                    :accessor api-token-input)
   (auto-submit-check capi:check-button
                      :text (tr :auto-submit-label)
                      :selected (config-value :auto-submit)
                      :selection-callback 'toggle-auto-submit-callback
                      :retract-callback 'toggle-auto-submit-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor auto-submit-check)
   (submit-aborted-check capi:check-button
                         :text (tr :submit-aborted-label)
                         :selected (config-value :submit-aborted)
                         :selection-callback 'toggle-submit-aborted-callback
                         :retract-callback 'toggle-submit-aborted-callback
                         :callback-type :interface
                         :font *ui-font*
                         :accessor submit-aborted-check)
   (completion-sound-check capi:check-button
                           :text (tr :completion-sound-label)
                           :selected (config-value :completion-sound)
                           :selection-callback 'toggle-completion-sound-callback
                           :retract-callback 'toggle-completion-sound-callback
                           :callback-type :interface
                           :font *ui-font*
                           :accessor completion-sound-check)
   (trigger-log-check capi:check-button
                      :text (tr :trigger-log-label)
                      :selected (config-value :trigger-log)
                      :selection-callback 'toggle-trigger-log-callback
                      :retract-callback 'toggle-trigger-log-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor trigger-log-check)
   (record-check capi:check-button
                 :text (tr :record-label)
                 :selected (config-value :record-enabled)
                 :selection-callback 'toggle-record-callback
                 :retract-callback 'toggle-record-callback
                 :callback-type :interface
                 :font *ui-font*
                 :accessor record-check)
   (record-audio-check capi:check-button
                       :text (tr :record-audio-label)
                       :selected (config-value :record-audio)
                       :selection-callback 'toggle-record-audio-callback
                       :retract-callback 'toggle-record-audio-callback
                       :callback-type :interface
                       :font *ui-font*
                       :accessor record-audio-check)
   (video-upload-check capi:check-button
                       :text (tr :video-upload-label)
                       :selected (config-value :video-upload)
                       :selection-callback 'toggle-video-upload-callback
                       :retract-callback 'toggle-video-upload-callback
                       :callback-type :interface
                       :font *ui-font*
                       :accessor video-upload-check)
   ;; The retention policy in one line, sitting under the upload
   ;; toggle it applies to. Uploading defaults to on, but the first
   ;; launch lands on this tab (no token yet), so it is seen before
   ;; anything is uploaded.
   (video-retention-note capi:title-pane
                         :text (tr :video-retention-note)
                         :font *ui-font*)
   ;; ffmpeg itself is not a setting: the release bundles it next to the
   ;; exe (an override still exists as :ffmpeg-path in config.sexp).
   (update-status-pane capi:title-pane
                       :text (tr :version-status (client-version) nil)
                       :font *ui-font*
                       :accessor update-status-pane)
   (auto-update-check capi:check-button
                      :text (tr :auto-update-label)
                      :selected (config-value :auto-update)
                      :selection-callback 'toggle-auto-update-callback
                      :retract-callback 'toggle-auto-update-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor auto-update-check)
   (check-updates-button capi:push-button
                         :text (tr :check-updates-button)
                         :callback 'check-updates-callback
                         :callback-type :interface
                         :font *ui-font*)
   (record-dir-display capi:title-pane
                       :text (record-dir-label)
                       :font *ui-font*
                       :accessor record-dir-display)
   (record-dir-button capi:push-button
                      :text (tr :change-folder-button)
                      :callback 'choose-record-dir-callback
                      :callback-type :interface
                      :font *ui-font*)
   (save-button capi:push-button
                :text (tr :save-button)
                :callback 'save-settings-callback
                :callback-type :interface
                :font *ui-font*)
   (upload-button capi:push-button
                  :text (tr :upload-button)
                  :callback 'upload-video-callback
                  :callback-type :interface
                  :font *ui-font*)
   (recordings-folder-button capi:push-button
                             :text (tr :recordings-folder-button)
                             :callback 'open-recordings-folder-callback
                             :callback-type :interface
                             :font *ui-font*)
   (my-runs-button capi:push-button
                   :text (tr :my-runs-button)
                   :callback 'open-my-runs-callback
                   :callback-type :interface
                   :font *ui-font*)
   (retry-button capi:push-button
                 :text (tr :retry-button)
                 :callback 'retry-callback
                 :callback-type :interface
                 :font *ui-font*)
   ;; A bare x, like the clear button of a search field: it sits at the
   ;; list's top right corner, so "clear the list" is read off the
   ;; position (plus tooltip); the confirm dialog spells out the rest.
   (clear-list-button capi:push-button
                      :text "×"
                      :callback 'clear-list-callback
                      :callback-type :interface
                      :help-key :clear-list
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
   ;; The x rides the quest-status row (NIL = stretchable gap), landing
   ;; in the list's top right corner away from the everyday buttons.
   (quest-row capi:row-layout '(quest-status nil clear-list-button)
              :adjust :center)
   (runs-tab capi:column-layout
             '(status-row quest-row runs-list actions-row)
             :adjust :left)
   ;; Settings are grouped by how they behave: the Connection fields
   ;; need Save & verify, every checkbox applies immediately.
   (language-group capi:column-layout '(language-radio)
                   :title (tr :group-language) :title-position :frame
                   :title-font *ui-font* :adjust :left)
   ;; The Server URL is a developer setting: pointing a normal user's
   ;; client anywhere else only breaks it. The pane always exists (so
   ;; SAVE-SETTINGS-CALLBACK and REBUILD-INTERFACE read it either way),
   ;; it just stays out of the layout without --debug / :debug t.
   (connection-group capi:column-layout
                     (if (debug-mode-p)
                         '(server-url-input api-token-input save-button)
                         '(api-token-input save-button))
                     :title (tr :group-connection) :title-position :frame
                     :title-font *ui-font* :adjust :left)
   (completion-group capi:column-layout
                     '(auto-submit-check submit-aborted-check
                       completion-sound-check)
                     :title (tr :group-completion) :title-position :frame
                     :title-font *ui-font* :adjust :left)
   (recording-row capi:row-layout '(record-dir-display record-dir-button)
                  :adjust :center)
   (recording-group capi:column-layout
                    '(record-check record-audio-check video-upload-check
                      video-retention-note recording-row)
                    :title (tr :group-recording) :title-position :frame
                    :title-font *ui-font* :adjust :left)
   (updates-group capi:column-layout
                  '(update-status-pane auto-update-check
                    check-updates-button)
                  :title (tr :group-updates) :title-position :frame
                  :title-font *ui-font* :adjust :left)
   (advanced-group capi:column-layout '(trigger-log-check)
                   :title (tr :group-advanced) :title-position :frame
                   :title-font *ui-font* :adjust :left)
   (settings-tab capi:column-layout
                 '(language-group connection-group completion-group
                   recording-group updates-group advanced-group)
                 :adjust :left)
   (main-tabs capi:tab-layout ()
              :items `((,(tr :tab-runs) runs-tab)
                       (,(tr :tab-settings) settings-tab))
              :font *ui-font*
              :print-function 'first
              :visible-child-function 'second
              ;; First launch without a token lands on Settings (step 1
              ;; of the flow); otherwise straight to the Runs tab.
              :selection (if (string= (config-value :api-token) "") 1 0)))
  (:default-initargs
   :title "RappyRuns Client"
   :layout 'main-tabs
   :help-callback 'client-help-callback
   :visible-min-width '(:character 100)))

(defun client-help-callback (interface pane type key)
  "Tooltips for panes whose looks alone do not say what they do (the
bare x button). CAPI calls this for every help interaction; anything
but a known tooltip key returns NIL (= no tooltip)."
  (declare (ignore interface pane))
  (when (eq type :tooltip)
    (case key
      (:clear-list (tr :clear-list-tooltip)))))

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

(defun toggle-submit-aborted-callback (interface)
  "Apply the submit-aborted toggle immediately (no Save needed)."
  (setf (config-value :submit-aborted)
        (capi:button-selected (submit-aborted-check interface)))
  (save-config!))

(defun toggle-completion-sound-callback (interface)
  "Apply the completion-sound toggle immediately (no Save needed)."
  (setf (config-value :completion-sound)
        (capi:button-selected (completion-sound-check interface)))
  (save-config!))

(defun language-changed-callback (interface)
  "Switch the UI language. CAPI fixes pane labels, list columns and tab
titles at creation time, so the reliable way to relabel everything is
to build a fresh window."
  (let ((language (capi:choice-selected-item (language-radio interface))))
    (unless (eq language *language*)
      (setf *language* language
            (config-value :language) language)
      (save-config!)
      (rebuild-interface interface))))

(defun rebuild-interface (old)
  "Replace OLD with a freshly built window at the same screen position,
carrying over unsaved Connection edits and the selected tab.
*INTERFACE* flips to the new window before OLD is destroyed, so the
poll loop never picks up a dead interface."
  (multiple-value-bind (x y) (capi:top-level-interface-geometry old)
    (let ((new (make-instance 'client-window :best-x x :best-y y)))
      (setf (capi:text-input-pane-text (server-url-input new))
            (capi:text-input-pane-text (server-url-input old))
            (capi:text-input-pane-text (api-token-input new))
            (capi:text-input-pane-text (api-token-input old)))
      (setf (capi:choice-selection (slot-value new 'main-tabs))
            (capi:choice-selection (slot-value old 'main-tabs)))
      (capi:display new)
      (setf *interface* new
            *last-window-title* nil)
      (refresh-runs-list new)
      (check-server new)
      (check-token new)
      (capi:destroy old))))

(defun record-dir-label ()
  (tr :record-dir-label (namestring (resolve-record-dir))))

(defun choose-record-dir-callback (interface)
  "Pick the recordings folder with the system directory dialog; applied
and saved immediately (no Save settings needed)."
  (let ((dir (capi:prompt-for-directory (tr :choose-record-dir)
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

(defun clear-list-callback (interface)
  "Clear the runs list after confirmation. Only unsent runs stay (see
CLEAR-RUNS!); drafts with a pending video go too - by the time someone
reaches for this button, those are recordings they never meant to
upload, and the site can still take a video for them."
  (when (capi:confirm-yes-or-no "~a" (tr :clear-list-confirm))
    (clear-runs!)
    (refresh-runs-list interface)))

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
run whose video could still take a YouTube link - unattached, or only
auto-uploaded (the just-finished-playing case)."
  (let* ((selected (capi:choice-selected-item (runs-list-pane interface)))
         (entry (cond ((and selected (getf selected :video-path)) selected)
                      ((null selected)
                       (find-if (lambda (e)
                                  (and (getf e :video-path)
                                       (or (not (getf e :video-attached))
                                           (hosted-video-replaceable-p e))))
                                (queued-runs))))))
    (cond
      ((and selected (not (getf selected :video-path)))
       (capi:display-message "~a" (tr :no-recording-for-run)))
      ((null entry)
       (capi:display-message "~a" (tr :no-recordings-yet)))
      ((not (ignore-errors (probe-file (getf entry :video-path))))
       (capi:display-message "~a" (tr :recording-file-missing
                                      (getf entry :video-path))))
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
     (multiple-value-bind (updated error already-submitted)
         (attach-video-url! entry url)
       (declare (ignore updated))
       (refresh-runs-list interface)
       (when (or error already-submitted)
         (capi:execute-with-interface-if-alive
          interface
          (lambda ()
            (capi:display-message
             "~a" (if error
                      (tr :attach-failed error)
                      (tr :attach-already-submitted))))))))))

(defun offer-clipboard-url (interface url)
  "Confirm (on the GUI thread) which run the copied URL belongs to,
then attach it in a worker process."
  (capi:execute-with-interface-if-alive
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
               (tr :attach-choose url)
               :print-function 'run-choice-label)
            (when (and okp entry)
              (attach-video-in-background interface entry url))))
         (t
          (when (capi:confirm-yes-or-no
                 "~a" (tr (if (hosted-video-replaceable-p target)
                              :attach-confirm-replace
                              :attach-confirm)
                          (run-choice-label target) url))
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
                "~a" (tr :token-setup-offer url))
           (open-in-browser url))
         (labels ((ask ()
                    (multiple-value-bind (text okp)
                        (capi:prompt-for-string (tr :token-paste-prompt))
                      (let ((token (and okp (normalize-token text))))
                        (if (or (null token) (string= token ""))
                            (set-pane-text interface #'token-status-pane
                                           (tr :token-not-set))
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
                                           "~a" (tr :token-retry))
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
           "~a" (tr :trigger-log-on (namestring path))))
        (close-trigger-log))))

(defun toggle-record-callback (interface)
  "Apply the recording toggle immediately (no Save needed). Turning it
on verifies that ffmpeg can actually be started; otherwise the box
snaps back off with instructions."
  (let ((on (capi:button-selected (record-check interface))))
    (when (and on (not (ffmpeg-available-p)))
      (setf (capi:button-selected (record-check interface)) nil
            on nil)
      (capi:display-message "~a" (tr :ffmpeg-missing)))
    (setf (config-value :record-enabled) on)
    (save-config!)))

(defun toggle-record-audio-callback (interface)
  "Apply the audio toggle immediately (no Save needed). Read at
recording start, so it takes effect from the next quest."
  (setf (config-value :record-audio)
        (capi:button-selected (record-audio-check interface)))
  (save-config!))

(defun toggle-video-upload-callback (interface)
  "Applies immediately: the poll loop reads it before each upload, so
unticking also stops the queue after the in-flight file (if any)."
  (setf (config-value :video-upload)
        (capi:button-selected (video-upload-check interface)))
  (save-config!))

;; The cross-thread update helpers use the -IF-ALIVE variant: the
;; language toggle destroys and replaces the window, and the poll loop
;; or a background check may still hold the old one for a moment.

(defun set-pane-text (interface accessor text &optional foreground)
  "Update a title pane's text and foreground color (NIL = default) from
any thread. Errors get :red so they stand out from routine status."
  (capi:execute-with-interface-if-alive
   interface
   (lambda ()
     (let ((pane (funcall accessor interface)))
       (setf (capi:title-pane-text pane) text)
       (setf (capi:simple-pane-foreground pane) foreground)))))

(defun refresh-runs-list (interface)
  (let ((runs (queued-runs)))
    (capi:execute-with-interface-if-alive
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
                          (tr :server-ok
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
        (set-pane-text interface #'token-status-pane (tr :token-not-set))
        (progn
          (set-pane-text interface #'token-status-pane (tr :token-checking))
          (mp:process-run-function
           "eta-client-token-check" '()
           (lambda ()
             (handler-case
                 (multiple-value-bind (outcome user) (fetch-me :token token)
                   (ecase outcome
                     (:ok
                      (let ((name (gethash "username" user)))
                        (set-pane-text interface #'token-status-pane
                                       (tr :token-ok name))
                        (when notify
                          (capi:execute-with-interface-if-alive
                           interface
                           (lambda ()
                             (capi:display-message
                              "~a" (tr :token-ok-dialog name)))))))
                     (:unauthorized
                      (set-pane-text interface #'token-status-pane
                                     (tr :token-invalid) :red)
                      (when notify
                        (capi:execute-with-interface-if-alive
                         interface
                         (lambda ()
                           (capi:display-message
                            "~a" (tr :token-rejected-dialog)))))
                      (when on-invalid (funcall on-invalid)))))
               (error (condition)
                 (set-pane-text interface #'token-status-pane
                                (token-status-error-text condition) :red)))))))))

;;; Self-update flow. The mechanics (release fetch, download, helper
;;; script handover) live in updater.lisp; this is the GUI choreography.
;;; The automatic leg runs BEFORE the main window exists
;;; (STARTUP-AUTO-UPDATE, called from MAIN): launching the outdated
;;; build just to watch it quit and relaunch reads as a glitch, so the
;;; update downloads behind a small progress splash and the client only
;;; ever comes up on the new build. The Settings button
;;; (CHECK-FOR-UPDATES) covers mid-session checks and reports every
;;; outcome; downloads and restarts run unattended - no confirmation
;;; dialogs. The only guard is that nothing is ever applied while a run
;;; or recording is in flight (note-poll-activity in main.lisp applies
;;; the deferred update once idle).

(defun toggle-auto-update-callback (interface)
  "Apply the auto-update toggle immediately (no Save needed)."
  (setf (config-value :auto-update)
        (capi:button-selected (auto-update-check interface)))
  (save-config!))

(defun check-updates-callback (interface)
  (check-for-updates interface))

(defun set-version-status (interface &optional note color)
  (set-pane-text interface #'update-status-pane
                 (tr :version-status (client-version) note)
                 color))

(defun update-note (interface text)
  "Pop an informational dialog from any thread."
  (capi:execute-with-interface-if-alive
   interface
   (lambda () (capi:display-message "~a" text))))

(defun check-for-updates (interface)
  "The Settings button: look for a newer release on a background thread,
report every outcome and install unattended when there is one. (The
automatic startup check runs before the main window exists -
STARTUP-AUTO-UPDATE.)"
  (cond
    ((null *client-version*)
     (update-note interface (tr :update-dev-build (update-release-page-url))))
    (t
     (set-version-status interface (tr :update-checking))
     (mp:process-run-function
      "eta-client-update-check" '()
      (lambda ()
        (let ((release (fetch-latest-release)))
          (cond
            ((null release)
             (set-version-status interface (tr :update-check-failed))
             (update-note interface (tr :update-check-failed-dialog)))
            ((not (update-available-p *client-version* (getf release :tag)))
             (set-version-status interface (tr :update-up-to-date))
             (update-note interface
                          (tr :update-latest-dialog (client-version))))
            (t
             ;; Already on the check's background thread; download
             ;; right away, no confirmation.
             (run-update-download interface release)))))))))

(defun offer-manual-download (interface)
  "The install dir refuses writes (e.g. Program Files without
elevation): flag it and offer the release page instead."
  (set-version-status interface (tr :update-not-writable) :red)
  (capi:execute-with-interface-if-alive
   interface
   (lambda ()
     (when (capi:confirm-yes-or-no
            "~a" (tr :update-not-writable-confirm))
       (open-in-browser (update-release-page-url))))))

(defun download-release-with-progress (release show)
  "DOWNLOAD-UPDATE! with the standard progress line handed to SHOW (a
function of one status string). Returns the verified zip path or NIL."
  (let ((tag (getf release :tag))
        (last-shown -1))
    (download-update!
     release (update-zip-path)
     :on-progress
     (lambda (done total)
       ;; Once per MB: this runs per 64KB chunk, and every line shown
       ;; is a message to the GUI thread.
       (let ((mb (floor done 1048576)))
         (when (> mb last-shown)
           (setf last-shown mb)
           (funcall show (tr :update-downloading
                             tag mb (and total (ceiling total 1048576))))))))))

(defun run-update-download (interface release)
  (cond
    ((not (install-dir-writable-p))
     (offer-manual-download interface))
    (t
     (let ((tag (getf release :tag))
           (zip (download-release-with-progress
                 release
                 (lambda (text) (set-version-status interface text)))))
       (cond
         ((null zip)
          (set-version-status interface (tr :update-download-failed) :red)
          (update-note interface (tr :update-download-failed-dialog)))
         (*poll-busy-p*
          ;; Never swap the exe mid-run; note-poll-activity re-offers
          ;; the moment the run and its recording are over.
          (setf *update-ready-zip* (cons zip tag))
          (set-version-status interface (tr :update-after-run tag)))
         (t
          (apply-update-restart interface zip tag)))))))

(defun apply-update-restart (interface zip tag)
  "Exit and let the helper script swap the exe and restart - no prompt;
auto-update means unattended. Called from the download thread and from
the poll loop (for updates deferred past a run)."
  (set-version-status interface (tr :update-restarting tag))
  (capi:execute-with-interface-if-alive
   interface
   (lambda ()
     ;; *INTERFACE*, not the captured window: a language switch may
     ;; have rebuilt it, and the teardown must destroy the live one.
     (launch-updater-and-quit *interface* zip))))

(capi:define-interface update-splash ()
  ()
  (:panes
   (message capi:title-pane
            :text ""
            :font *ui-font*
            :visible-min-width '(:character 44)
            :accessor splash-message-pane))
  (:layouts
   (main capi:column-layout '(message)
         :internal-border 24))
  (:default-initargs
   :title "RappyRuns Client"))

(defun startup-auto-update ()
  "The automatic update pass, run from MAIN before the main window is
created. When a newer release is out and the folder is writable, the
zip downloads behind a small splash and LAUNCH-UPDATER-AND-QUIT takes
over - this call never returns and the main window of the old build
never shows. In every other case it returns with the outcome in
*STARTUP-UPDATE-NOTE* for REPORT-STARTUP-UPDATE to surface."
  (let* ((release (fetch-latest-release))
         (decision (startup-update-decision
                    release *client-version* (install-dir-writable-p))))
    (setf *startup-update-note* decision)
    (when (eq decision :apply)
      (let ((tag (getf release :tag))
            (splash (capi:display (make-instance 'update-splash))))
        ;; Something to read while the connection is still coming up.
        (set-pane-text splash #'splash-message-pane
                       (tr :update-downloading tag 0 nil))
        (let ((zip (download-release-with-progress
                    release
                    (lambda (text)
                      (set-pane-text splash #'splash-message-pane text)))))
          (cond
            (zip
             (set-pane-text splash #'splash-message-pane
                            (tr :update-restarting tag))
             ;; Does not return: hands over to the helper and quits.
             (launch-updater-and-quit splash zip))
            (t
             (setf *startup-update-note* :download-failed)
             (capi:execute-with-interface-if-alive
              splash
              (lambda () (capi:destroy splash))))))))))

(defun report-startup-update (interface)
  "Reflect the pre-GUI update pass on the settings pane once the main
window is up. The two failures the user can act on also get the same
dialogs the Settings-button check shows; a failed release check stays
silent, exactly like the old silent startup check."
  (case *startup-update-note*
    (:up-to-date (set-version-status interface (tr :update-up-to-date)))
    (:not-writable (offer-manual-download interface))
    (:download-failed
     (set-version-status interface (tr :update-download-failed) :red)
     (update-note interface (tr :update-download-failed-dialog)))))

(defun set-window-title (interface title)
  (unless (equal title *last-window-title*)
    (setf *last-window-title* title)
    (capi:execute-with-interface-if-alive
     interface
     (lambda ()
       (setf (capi:interface-title interface) title)))))

(defun update-game-status (interface connected-p detector snapshot
                           &optional recorder rejection)
  (let ((recording-error (and recorder (recorder-last-error recorder)))
        (recording-p (and recorder
                          (eq (recorder-state recorder) :recording)))
        (in-quest-p (eq (detector-state detector) :in-quest)))
    (set-pane-text interface #'game-status-pane
                   (let ((base (cond
                                 (rejection
                                  (tr :game-signature-refused
                                      (signature-status-label rejection)))
                                 (connected-p (tr :game-attached))
                                 (t (tr :game-searching)))))
                     (if recording-error
                         (tr :game-status-with-error base recording-error)
                         base))
                   (and (or rejection recording-error) :red))
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
        (tr :quest-waiting (getf snapshot :quest-name)))
       (t (tr :no-active-quest))))
    ;; The taskbar truncates from the right, so the time goes first.
    (set-window-title
     interface
     (if in-quest-p
         (format nil "~a~:[~; [REC]~] - RappyRuns Client"
                 (format-run-time (detector-elapsed-ms detector))
                 recording-p)
         "RappyRuns Client"))))
