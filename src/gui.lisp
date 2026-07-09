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

(defvar *moderator-p* nil
  "Whether the verified account may author quest rules (role moderator or
admin). Drives the two moderator-only pieces of UI - the Rooms tab and
the Advanced 'register rule' button - which a normal user can't use (the
server rejects the POST), so hiding them keeps the window uncluttered.
Seeded from the cached :MODERATOR config in MAIN so a returning
moderator's window is right on the first frame, then re-verified from
/api/me by CHECK-TOKEN, which rebuilds the window if it changed.")

(defun client-tab-items ()
  "Main-window tab entries as (LABEL PANE-NAME) pairs. The Rooms tab
lists the live run's rooms and enemies solely to author quest-clear
rules, so it appears only for moderators (*MODERATOR-P*); normal users,
who cannot create rules, never see it."
  (append
   (list (list (tr :tab-runs) 'runs-tab))
   (when *moderator-p*
     (list (list (tr :tab-rooms) 'rooms-tab)))
   (list (list (tr :tab-settings) 'settings-tab))))

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
   ;; Live rooms/monsters of the current or most-recent run. Fills as you
   ;; play; double-click a row to register a rule from that condition.
   (rooms-hint capi:title-pane
               :text (tr :rooms-hint)
               :font *ui-font*)
   (rooms-list capi:multi-column-list-panel
               :font *ui-font*
               :header-args (list :font *ui-font*)
               :columns `((:title ,(tr :col-area) :width (:character 30))
                          (:title ,(tr :col-condition) :width (:character 20))
                          (:title ,(tr :col-trigger) :width (:character 22)))
               :items '()
               :column-function
               (lambda (row)
                 (list (getf row :area)
                       (rooms-row-condition row)
                       (rule-trigger-label (getf row :trigger))))
               :accessor rooms-list-pane
               :action-callback 'rooms-list-action-callback
               :callback-type :data-interface
               :visible-min-height '(:character 10))
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
   ;; Auto-submit, submit-aborted and completion-sound are fixed
   ;; behaviors now (see +FORCED-CONFIG-KEYS+); no controls for them.
   (trigger-log-check capi:check-button
                      :text (tr :trigger-log-label)
                      :selected (config-value :trigger-log)
                      :selection-callback 'toggle-trigger-log-callback
                      :retract-callback 'toggle-trigger-log-callback
                      :callback-type :interface
                      :font *ui-font*
                      :accessor trigger-log-check)
   (register-rule-button capi:push-button
                         :text (tr :register-rule-button)
                         :callback 'register-quest-rule-callback
                         :callback-type :interface
                         :font *ui-font*)
   ;; Recording and uploading are fixed behaviors now (see
   ;; +FORCED-CONFIG-KEYS+); only the game-audio toggle remains.
   (record-audio-check capi:check-button
                       :text (tr :record-audio-label)
                       :selected (config-value :record-audio)
                       :selection-callback 'toggle-record-audio-callback
                       :retract-callback 'toggle-record-audio-callback
                       :callback-type :interface
                       :font *ui-font*
                       :accessor record-audio-check)
   ;; The local storage budget in one line - what keeps the recordings
   ;; folder from growing without bound - alongside the hosted note.
   (record-storage-note capi:title-pane
                        :text (tr :record-storage-note
                                  (config-value :record-max-total-gb))
                        :font *ui-font*)
   ;; The retention policy in one line. Uploading is always on, so this
   ;; is what happens to the hosted videos over time.
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
   ;; Resident-app (tray) settings. Autostart is backed by the registry
   ;; (autostart-win32.lisp), so its checkbox reads AUTOSTART-ENABLED-P
   ;; rather than config; the other two are plain config toggles.
   (close-to-tray-check capi:check-button
                        :text (tr :close-to-tray-label)
                        :selected (config-value :close-to-tray)
                        :selection-callback 'toggle-close-to-tray-callback
                        :retract-callback 'toggle-close-to-tray-callback
                        :callback-type :interface
                        :font *ui-font*
                        :accessor close-to-tray-check)
   (autostart-check capi:check-button
                    :text (tr :autostart-label)
                    :selected (autostart-enabled-p)
                    :selection-callback 'toggle-autostart-callback
                    :retract-callback 'toggle-autostart-callback
                    :callback-type :interface
                    :font *ui-font*
                    :accessor autostart-check)
   (start-minimized-check capi:check-button
                          :text (tr :start-minimized-label)
                          :selected (config-value :start-minimized)
                          :selection-callback 'toggle-start-minimized-callback
                          :retract-callback 'toggle-start-minimized-callback
                          :callback-type :interface
                          :font *ui-font*
                          :accessor start-minimized-check)
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
   (recording-row capi:row-layout '(record-dir-display record-dir-button)
                  :adjust :center)
   (recording-group capi:column-layout
                    '(record-audio-check record-storage-note
                      video-retention-note recording-row)
                    :title (tr :group-recording) :title-position :frame
                    :title-font *ui-font* :adjust :left)
   (updates-group capi:column-layout
                  '(update-status-pane auto-update-check
                    check-updates-button)
                  :title (tr :group-updates) :title-position :frame
                  :title-font *ui-font* :adjust :left)
   (tray-group capi:column-layout
               '(close-to-tray-check autostart-check start-minimized-check)
               :title (tr :group-tray) :title-position :frame
               :title-font *ui-font* :adjust :left)
   ;; The 'register rule' button authors quest rules and is moderator-only
   ;; (the server rejects the POST otherwise), so it drops out of the
   ;; layout for normal users - just like the Rooms tab. The pane always
   ;; exists so its callback stays wired either way.
   (advanced-group capi:column-layout
                   (if *moderator-p*
                       '(trigger-log-check register-rule-button)
                       '(trigger-log-check))
                   :title (tr :group-advanced) :title-position :frame
                   :title-font *ui-font* :adjust :left)
   (settings-tab capi:column-layout
                 '(language-group connection-group
                   recording-group updates-group tray-group advanced-group)
                 :adjust :left)
   (rooms-tab capi:column-layout '(rooms-hint rooms-list) :adjust :left)
   (main-tabs capi:tab-layout ()
              :items (client-tab-items)
              :font *ui-font*
              :print-function 'first
              :visible-child-function 'second
              ;; First launch without a token lands on Settings (step 1
              ;; of the flow); otherwise straight to the Runs tab. The
              ;; Settings index shifts with the optional Rooms tab, so
              ;; find it rather than hard-coding a position.
              :selection (if (string= (config-value :api-token) "")
                             (position 'settings-tab (client-tab-items)
                                       :key 'second)
                             0)))
  (:default-initargs
   :title "Rappy Runs Client"
   :layout 'main-tabs
   :help-callback 'client-help-callback
   :confirm-destroy-function 'client-confirm-destroy
   :visible-min-width '(:character 100)))

(defun client-confirm-destroy (interface)
  "Called when the window is about to close (the x button / quit-interface;
NOT plain CAPI:DESTROY, so the language-toggle rebuild is unaffected).
Non-NIL lets the close proceed. When a real quit is already in progress
(*REALLY-QUITTING*) allow it; when close-to-tray is on, hide to the tray
and veto the destroy so the app keeps running; otherwise (close-to-tray
off) this is a genuine quit - QUIT-APP tears down the tray and poll
threads and terminates, which returning T alone would not do."
  (cond (*really-quitting* t)
        ((config-value :close-to-tray)
         (setf (capi:top-level-interface-display-state interface) :hidden)
         nil)
        (t (quit-app) t)))

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
      ;; Carry over the selected tab by identity, not index: a
      ;; moderator-status rebuild changes the tab set (the Rooms tab
      ;; appears or vanishes), so the old index can point at a different
      ;; tab or none at all. Match on the tab's pane symbol, falling back
      ;; to the first tab when the old one is gone.
      (let* ((old-item (capi:choice-selected-item (slot-value old 'main-tabs)))
             (pane (and (consp old-item) (second old-item)))
             (items (capi:collection-items (slot-value new 'main-tabs))))
        (setf (capi:choice-selection (slot-value new 'main-tabs))
              (or (position pane items :key 'second :test 'eq) 0)))
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
       ;; The Upload-to-YouTube intent is consumed once its link is on a
       ;; run: clear the preferred target so the next copied URL, which
       ;; may be for something else entirely, is not auto-aimed here.
       (unless error (setf *last-upload-run* nil))
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

(defvar *pairing-process* nil
  "The in-flight pairing worker, or NIL. One at a time.")

(defun pairing-label ()
  "API token label for a pairing from this machine, so the token list
on the site says which computer it belongs to."
  (let ((name (ignore-errors (machine-instance))))
    (if (and (stringp name) (string/= name ""))
        (format nil "Desktop client (~a)" name)
        "Desktop client")))

(defun prompt-for-token-setup (interface)
  "First-run setup: while no API token is configured, log in from the
login.txt next to the exe when there is one, otherwise pair with the
site - open /pair?code=... in the browser and poll until the user
approves the connection there; the token arrives over the API. Runs
again on every launch until a token is set; the Settings tab's manual
paste stays as the fallback."
  (when (string= (normalize-token (config-value :api-token)) "")
    (if (credentials-present-p)
        (start-file-login-flow interface)
        (start-pairing-flow interface))))

(defun start-pairing-flow (interface)
  (unless (and *pairing-process* (mp:process-alive-p *pairing-process*))
    (setf *pairing-process*
          (mp:process-run-function
           "eta-client-pairing" '()
           (lambda () (run-pairing-flow interface))))))

(defun run-pairing-flow (interface)
  "The pairing worker: register a code, hand the browser the approval
page and poll until approved, expired or the client shuts down.
Transient poll errors (the laptop's Wi-Fi blinking) do not abort the
wait; only failing to even start the pairing gives up, and the next
launch retries."
  (handler-case
      (multiple-value-bind (code interval expires-in)
          (start-pairing :label (pairing-label))
        (open-in-browser (pairing-url code))
        (set-pane-text interface #'token-status-pane (tr :pairing-waiting))
        (loop :repeat (ceiling expires-in interval)
              :do (mp:process-wait-with-timeout
                   "pairing poll" interval (lambda () *stop-requested*))
                  (when (or *stop-requested*
                            ;; A token pasted in Settings meanwhile wins.
                            (string/= (normalize-token
                                       (config-value :api-token))
                                      ""))
                    (return))
                  (multiple-value-bind (outcome token)
                      (handler-case (poll-pairing code)
                        (api-error () :pending))
                    (ecase outcome
                      (:pending)
                      (:gone
                       (set-pane-text interface #'token-status-pane
                                      (tr :pairing-expired))
                       (return))
                      (:complete
                       (finish-pairing token)
                       (return))))
              ;; Only reached when the repeat count runs out: an
              ;; explicit RETURN above skips the LOOP epilogue.
              :finally (set-pane-text interface #'token-status-pane
                                      (tr :pairing-expired))))
    (api-error (condition)
      (set-pane-text interface #'token-status-pane
                     (tr :pairing-failed (api-error-message condition))
                     :red))))

;;; login.txt file login (credentials.lisp holds the file handling):
;;; the user placed the file deliberately, so failures land in the
;;; token status pane and the browser pairing is never started on top.

(defun file-login-label ()
  (format nil "~a [login.txt]" (pairing-label)))

(defun start-file-login-flow (interface)
  (mp:process-run-function
   "eta-client-file-login" '()
   (lambda () (run-file-login-flow interface))))

(defun run-file-login-flow (interface)
  (multiple-value-bind (username password) (read-credentials)
    (if (null username)
        (set-pane-text interface #'token-status-pane
                       (tr :file-login-bad-file) :red)
        (progn
          (set-pane-text interface #'token-status-pane
                         (tr :file-login-checking))
          (handler-case
              (multiple-value-bind (outcome token)
                  (login-with-password username password
                                       :label (file-login-label))
                (ecase outcome
                  (:ok (finish-pairing token))
                  (:unauthorized
                   (set-pane-text interface #'token-status-pane
                                  (tr :file-login-invalid) :red))))
            (api-error (condition)
              (set-pane-text interface #'token-status-pane
                             (tr :file-login-failed
                                 (api-error-message condition))
                             :red)))))))

(defun finish-pairing (token)
  "Save and reflect a token that arrived over the pairing or password
login API. Uses the
live *INTERFACE* rather than the worker's captured one: a language
switch may have rebuilt the window during the wait."
  (setf (config-value :api-token) token)
  (save-config!)
  (let ((interface *interface*))
    (capi:execute-with-interface-if-alive
     interface
     (lambda ()
       (setf (capi:text-input-pane-text (api-token-input interface))
             token)))
    (check-token interface)))

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

;;; Quest-rule registration (Advanced): the in-client counterpart of the
;;; site's /mod/quests form (moderator token only). One modal form gathers
;;; everything at once - the quest is auto-selected from what was just
;;; played (*RUN-QUEST*), and the clear condition is chosen from the rooms
;;; and enemies of that run (RUN-ROOMS), so "clear when this enemy dies" or
;;; "clear this room" needs no trigger-log.txt hunting and no browser trip.

(defun timeable-quests (quests)
  "The fetched /api/quests entries that can parent a rule: those carrying
start+end detection triggers."
  (loop :for quest :across quests
        :when (and (gethash "start" quest) (gethash "end" quest))
          :collect quest))

(defun quest-parent-label (quest)
  (format nil "~a  (~a)" (gethash "name" quest) (gethash "slug" quest)))

(defun detected-parent (parents run-quest)
  "The fetched timeable quest matching the just-played RUN-QUEST (by in-game
number, else episode + name), or NIL - used to pre-select the form's quest."
  (when run-quest
    (or (let ((number (getf run-quest :number)))
          (and number (plusp number)
               (find number parents
                     :key (lambda (q) (gethash "game_number" q)) :test #'eql)))
        (let ((name (getf run-quest :name))
              (episode (getf run-quest :episode)))
          (and name
               (find-if (lambda (q)
                          (and (eql episode (gethash "episode" q))
                               (let ((names (gethash "game_names" q)))
                                 (and names (find name (coerce names 'list)
                                                  :test #'equal)))))
                        parents))))))

(defun rule-trigger-label (trigger)
  "Canonical trigger string for a preview / list cell, or \"\" for NIL."
  (if (null trigger)
      ""
      (ecase (first trigger)
        (:warp-in "warp-in")
        (:register (format nil "register:~d" (second trigger)))
        (:floor-switch (format nil "floor-switch:~d:~d"
                               (second trigger) (third trigger)))
        (:monster-dead (format nil "monster:~d" (second trigger))))))

(defun rule-error-message (payload)
  "A human string from an /api/quests error PAYLOAD - its \"message\" or
joined \"errors\" - or \"?\" when neither is present."
  (or (and (hash-table-p payload)
           (let ((message (gethash "message" payload))
                 (errors (gethash "errors" payload)))
             (cond ((and (stringp message) (string/= message "")) message)
                   ((and errors (plusp (length errors)))
                    (format nil "~{~a~^; ~}" (coerce errors 'list))))))
      "?"))

(defun rooms-row-condition (row)
  "The condition text for a RUN-ROOM-ROWS row: \"clear\" or the enemy name."
  (if (eq (getf row :kind) :clear) (tr :rooms-clear) (getf row :name)))

(defun rooms-row-label (row)
  "\"<area> - <condition>\" label for a run-room row."
  (format nil "~a - ~a" (getf row :area) (rooms-row-condition row)))

(defun rule-end-items ()
  "( (label . trigger-or-marker) ... ) for the end dropdown: this run's
rooms/enemies (from RUN-ROOM-ROWS, area-labelled) plus manual fallbacks.
A marker keyword (:monster/:floor-switch/:register) is resolved from the
form's value fields at submit time."
  (append
   (mapcar (lambda (row) (cons (rooms-row-label row) (getf row :trigger)))
           (run-room-rows))
   (list (cons (tr :rule-end-monster) :monster)
         (cons (tr :rule-end-floor-switch) :floor-switch)
         (cons (tr :rule-end-register) :register))))

(defun rule-start-items ()
  "( (label . X) ... ) for the start dropdown: inherit (default) or
warp-in. Floor-switch / register start overrides are rare - the site's
/mod/quests form covers them - so they are kept out of the client form."
  (list (cons (tr :rule-start-inherit) :inherit)
        (cons (tr :rule-start-warp-in) (list :warp-in))))

(defun rule-manual-marker-p (x)
  "True when an end item's value is a manual marker (needs the value
fields), rather than a ready trigger list."
  (member x '(:monster :floor-switch :register)))

(defun parse-int-in-range (string min max)
  "Parse STRING as an integer in [MIN, MAX], or NIL."
  (let ((n (ignore-errors (parse-integer (string-trim " " (or string ""))))))
    (and n (<= min n max) n)))

(defun resolve-manual-trigger (marker val1 val2)
  "Build a trigger from a manual MARKER and the two value-field strings, or
NIL when a value is missing/out of range."
  (ecase marker
    (:monster (let ((id (parse-int-in-range val1 0 65535)))
                (and id (list :monster-dead id))))
    (:floor-switch (let ((floor (parse-int-in-range val1 0 17))
                         (switch (parse-int-in-range val2 0 255)))
                     (and floor switch (list :floor-switch floor switch))))
    (:register (let ((n (parse-int-in-range val1 0 255)))
                 (and n (list :register n))))))

(defun post-quest-rule-in-background (interface parent-slug name description
                                      end start)
  "Fire the create-rule POST off the GUI thread; report the result back on
it. START is an internal trigger list, or NIL to inherit the parent's."
  (mp:process-run-function
   "eta-client-quest-rule-post" '()
   (lambda ()
     (handler-case
         (multiple-value-bind (outcome payload)
             (create-quest-rule :parent parent-slug :name name
                                :description description :end end :start start)
           (capi:execute-with-interface-if-alive
            interface
            (lambda ()
              (case outcome
                (:created
                 (capi:display-message
                  "~a" (tr :rule-created (gethash "slug" payload)))
                 ;; Pull the new rule into the active detection defs at once.
                 (check-server interface))
                (:duplicate
                 (capi:display-message
                  "~a" (tr :rule-duplicate (rule-error-message payload))))
                (:forbidden
                 (capi:display-message "~a" (tr :rule-forbidden)))
                (t
                 (capi:display-message
                  "~a" (tr :rule-rejected (rule-error-message payload))))))))
       (api-error (condition)
         (capi:execute-with-interface-if-alive
          interface
          (lambda ()
            (capi:display-message
             "~a" (tr :rule-post-failed (api-error-message condition))))))))))

;;; The single registration form. Everything is visible at once: the quest
;;; (pre-selected from the run just played), name, description, the clear
;;; condition (this run's rooms/enemies plus manual entries) with a live
;;; trigger preview and inline value fields for the manual options, and the
;;; start trigger (inherit by default). A Rooms-tab click opens the same
;;; form with the clicked condition pre-selected. Register validates and
;;; hands the POST to a worker; no follow-up confirmation - the form is the
;;; review.

(capi:define-interface quest-rule-dialog ()
  ()
  (:panes
   (quest-pane capi:option-pane
               :title (tr :rule-quest-label) :title-position :top
               :print-function 'quest-parent-label
               :visible-min-width '(:character 48)
               :font *ui-font* :title-font *ui-font*
               :accessor qrd-quest-pane)
   (name-pane capi:text-input-pane
              :title (tr :rule-name-label) :title-position :top
              :visible-min-width '(:character 48)
              :font *ui-font* :title-font *ui-font*
              :accessor qrd-name-pane)
   (desc-pane capi:text-input-pane
              :title (tr :rule-desc-label) :title-position :top
              :visible-min-width '(:character 48)
              :font *ui-font* :title-font *ui-font*
              :accessor qrd-desc-pane)
   (end-pane capi:option-pane
             :title (tr :rule-end-label-form) :title-position :top
             :print-function 'car
             :selection-callback 'qrd-end-changed :callback-type :interface
             :visible-min-width '(:character 48)
             :font *ui-font* :title-font *ui-font*
             :accessor qrd-end-pane)
   (preview-pane capi:title-pane
                 :text ""
                 :font *ui-font*
                 :accessor qrd-preview-pane)
   (val1-pane capi:text-input-pane
              :title (tr :rule-val1-label) :title-position :left
              :visible-min-width '(:character 10)
              :font *ui-font* :title-font *ui-font*
              :accessor qrd-val1-pane)
   (val2-pane capi:text-input-pane
              :title (tr :rule-val2-label) :title-position :left
              :visible-min-width '(:character 10)
              :font *ui-font* :title-font *ui-font*
              :accessor qrd-val2-pane)
   (start-pane capi:option-pane
               :title (tr :rule-start-label-form) :title-position :top
               :print-function 'car
               :visible-min-width '(:character 48)
               :font *ui-font* :title-font *ui-font*
               :accessor qrd-start-pane)
   (ok-button capi:push-button :text (tr :rule-register-ok)
              :callback 'qrd-ok :callback-type :interface :font *ui-font*)
   (cancel-button capi:push-button :text (tr :rule-cancel)
                  :callback 'qrd-cancel :callback-type :interface
                  :font *ui-font*))
  (:layouts
   ;; The two value fields only matter for the manual end options; they
   ;; sit under the preview, which spells out when they are needed.
   (values-row capi:row-layout '(val1-pane val2-pane))
   (buttons capi:row-layout '(nil ok-button cancel-button))
   (main capi:column-layout
         '(quest-pane name-pane desc-pane end-pane preview-pane values-row
           start-pane buttons)
         :adjust :left :internal-border 16 :gap 8))
  (:default-initargs
   :title (tr :rule-dialog-title)
   :layout 'main))

(defun qrd-end-preview-text (item)
  "Preview line for the selected end ITEM: the resolved trigger, or a hint
to fill the value fields for a manual option."
  (let ((x (cdr item)))
    (if (rule-manual-marker-p x)
        (tr :rule-manual-hint)
        (format nil "→ ~a" (rule-trigger-label x)))))

(defun qrd-end-changed (interface)
  "Refresh the preview line and enable the value fields only for the manual
options (val2 only for floor-switch), so it reads clearly that the fields
are ignored when a room/enemy from the run is selected."
  (let* ((item (capi:choice-selected-item (qrd-end-pane interface)))
         (marker (and item (cdr item)))
         (manual (and (rule-manual-marker-p marker) t)))
    (setf (capi:title-pane-text (qrd-preview-pane interface))
          (if item (qrd-end-preview-text item) "")
          (capi:simple-pane-enabled (qrd-val1-pane interface)) manual
          (capi:simple-pane-enabled (qrd-val2-pane interface))
          (eq marker :floor-switch))))

(defun qrd-resolve-end (interface item)
  "Resolve the selected end ITEM to an internal trigger: a ready trigger
list passes through; a manual marker reads the value fields. NIL when a
manual value is missing/out of range."
  (let ((x (cdr item)))
    (if (rule-manual-marker-p x)
        (resolve-manual-trigger
         x (capi:text-input-pane-text (qrd-val1-pane interface))
         (capi:text-input-pane-text (qrd-val2-pane interface)))
        x)))

(defun qrd-ok (interface)
  "Validate the form and, on success, close returning the rule. A missing
field keeps the dialog open with a message."
  (let ((parent (capi:choice-selected-item (qrd-quest-pane interface)))
        (name (string-trim " " (capi:text-input-pane-text
                                 (qrd-name-pane interface))))
        (desc (string-trim " " (capi:text-input-pane-text
                                 (qrd-desc-pane interface))))
        (end-item (capi:choice-selected-item (qrd-end-pane interface)))
        (start-item (capi:choice-selected-item (qrd-start-pane interface))))
    (cond
      ((null parent) (capi:display-message "~a" (tr :rule-need-quest)))
      ((string= name "") (capi:display-message "~a" (tr :rule-need-name)))
      ((string= desc "") (capi:display-message "~a" (tr :rule-need-desc)))
      ((null end-item) (capi:display-message "~a" (tr :rule-need-end)))
      (t
       (let ((end (qrd-resolve-end interface end-item)))
         (if (null end)
             (capi:display-message "~a" (tr :rule-need-values))
             (let ((start (cdr start-item)))
               (capi:exit-dialog
                (list :parent (gethash "slug" parent)
                      :name name :desc desc :end end
                      :start (unless (eq start :inherit) start))))))))))

(defun qrd-cancel (interface)
  (declare (ignore interface))
  (capi:abort-dialog))

(defun show-quest-rule-dialog (parents detected &optional preset)
  "Build and modally show the form. PRESET, when non-NIL, is a
(label . trigger) prepended to the end list and pre-selected (the Rooms-tab
click path). Returns a plist (:parent :name :desc :end :start) on Register,
or NIL on Cancel."
  (let* ((dlg (make-instance 'quest-rule-dialog))
         (end-items (let ((items (rule-end-items)))
                      (if preset (cons preset items) items)))
         (start-items (rule-start-items)))
    (setf (capi:collection-items (qrd-quest-pane dlg)) parents
          (capi:choice-selected-item (qrd-quest-pane dlg))
          (or detected (first parents))
          (capi:collection-items (qrd-end-pane dlg)) end-items
          (capi:choice-selected-item (qrd-end-pane dlg)) (first end-items)
          (capi:collection-items (qrd-start-pane dlg)) start-items
          (capi:choice-selected-item (qrd-start-pane dlg)) (first start-items))
    (qrd-end-changed dlg)
    (capi:display-dialog dlg)))

(defun run-quest-rule-flow (interface &optional preset)
  "Fetch the quest catalog off the GUI thread, then show the form on it;
on Register, POST the rule in a worker. PRESET pre-selects an end trigger
(from a Rooms-tab click)."
  (handler-case
      (let ((parents (timeable-quests (fetch-quests))))
        (capi:execute-with-interface-if-alive
         interface
         (lambda ()
           (if (null parents)
               (capi:display-message "~a" (tr :rule-no-parents))
               (let ((result (show-quest-rule-dialog
                              parents
                              (detected-parent parents *run-quest*)
                              preset)))
                 (when result
                   (post-quest-rule-in-background
                    interface (getf result :parent) (getf result :name)
                    (getf result :desc) (getf result :end)
                    (getf result :start))))))))
    (api-error (condition)
      (capi:execute-with-interface-if-alive
       interface
       (lambda ()
         (capi:display-message
          "~a" (tr :rule-fetch-failed (api-error-message condition))))))))

(defun register-quest-rule-callback (interface)
  "Advanced > Register quest rule: open the form (no preset)."
  (mp:process-run-function
   "eta-client-quest-rule" '()
   (lambda () (run-quest-rule-flow interface))))

(defun rooms-row-preset (row)
  "A (label . trigger) preset for the form from a clicked Rooms-tab ROW."
  (cons (rooms-row-label row) (getf row :trigger)))

(defun rooms-list-action-callback (row interface)
  "Double-click a Rooms-tab row: open the registration form with that
room/enemy as the pre-selected clear condition."
  (when (getf row :trigger)
    (mp:process-run-function
     "eta-client-quest-rule" '()
     (lambda () (run-quest-rule-flow interface (rooms-row-preset row))))))

(defun toggle-record-audio-callback (interface)
  "Apply the audio toggle immediately (no Save needed). Read at
recording start, so it takes effect from the next quest."
  (setf (config-value :record-audio)
        (capi:button-selected (record-audio-check interface)))
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

(defvar *rooms-list-signature* nil
  "Cheap signature of the rooms list last pushed to the GUI, so the ~4x/sec
tick only rebuilds the pane when the run's kills/switches changed - keeping
selection and scroll stable while the user clicks in the lobby.")

(defun rooms-list-signature ()
  "A value that changes whenever RUN-ROOM-ROWS would: kill and switch
counts plus the newest kill id."
  (list (length *run-kill-log*) (length *run-switch-log*)
        (getf (first *run-kill-log*) :id)))

(defun refresh-rooms-list (interface)
  "Rebuild the Rooms pane from RUN-ROOM-ROWS, but only when the underlying
run data changed."
  (let ((sig (rooms-list-signature)))
    (unless (equal sig *rooms-list-signature*)
      (setf *rooms-list-signature* sig)
      (let ((rows (run-room-rows)))
        (capi:execute-with-interface-if-alive
         interface
         (lambda ()
           (setf (capi:collection-items (rooms-list-pane interface))
                 (coerce rows 'vector))))))))

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

(defun moderator-role-p (role)
  "True when a /api/me ROLE string grants quest authoring (moderator or
admin); mirrors the server's MODELS:MODERATOR-P."
  (and (member role '("moderator" "admin") :test #'equal) t))

(defun apply-moderator-role (interface user)
  "Sync the moderator-only UI to the /api/me USER hash. When the role
crosses the moderator boundary, cache it and rebuild the window so the
Rooms tab and the Advanced 'register rule' button appear or vanish.
Only the change triggers a rebuild, so the CHECK-TOKEN that REBUILD
issues sees no change and does not loop."
  (let ((now (moderator-role-p (and user (gethash "role" user)))))
    (unless (eq now *moderator-p*)
      (setf *moderator-p* now
            (config-value :moderator) now)
      (save-config!)
      (capi:execute-with-interface-if-alive
       interface (lambda () (rebuild-interface interface))))))

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
                        (apply-moderator-role interface user)
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

(defun toggle-close-to-tray-callback (interface)
  "Apply the close-to-tray toggle immediately (no Save needed)."
  (setf (config-value :close-to-tray)
        (capi:button-selected (close-to-tray-check interface)))
  (save-config!))

(defun toggle-start-minimized-callback (interface)
  "Apply the start-minimized toggle immediately (no Save needed)."
  (setf (config-value :start-minimized)
        (capi:button-selected (start-minimized-check interface)))
  (save-config!))

(defun toggle-autostart-callback (interface)
  "Enable or disable the Windows logon autostart (registry-backed). On
failure, snap the checkbox back to the registry's real state so it never
lies about what will happen at logon."
  (let ((want (capi:button-selected (autostart-check interface))))
    (set-autostart! want)
    (setf (capi:button-selected (autostart-check interface))
          (autostart-enabled-p))))

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
   :title "Rappy Runs Client"))

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
         (format nil "~a~:[~; [REC]~] - Rappy Runs Client"
                 (format-run-time (detector-elapsed-ms detector))
                 recording-p)
         "Rappy Runs Client"))
    ;; Keep the live Rooms tab current (rebuilds only on change).
    (refresh-rooms-list interface)))
