(in-package :ephinea-ta-client)

;;; Config lives in %APPDATA%/ephinea-ta-client/config.sexp as a plist.

;; The six +forced-config-keys+ below are no longer user-adjustable:
;; their GUI controls were removed and MIGRATE-CONFIG scrubs any saved
;; value so CONFIG-VALUE always resolves to the default here. Edit the
;; behavior by changing the default in *DEFAULT-CONFIG*.
(defparameter +forced-config-keys+
  '(:auto-submit :submit-aborted :completion-sound
    :record-enabled :video-upload))

(defparameter *default-config*
  (list :server-url "https://rappyruns-production.up.railway.app"
        :api-token ""
        :language :en      ; UI language, :en or :ja (i18n.lisp)
        :auto-submit t          ; forced (hidden): always submit on completion
        :submit-aborted t       ; forced (hidden): record quests quit mid-run (private on the server)
        :auto-update t      ; install new GitHub releases at startup, unattended (updater.lisp)
        :completion-sound nil   ; forced (hidden): no completion sound
        :trigger-log nil
        :record-enabled t       ; forced (hidden): always record quest videos
        :record-audio t     ; game-only capture (process loopback; see audio-win32)
        :video-upload t         ; forced (hidden): always upload saved recordings
        :record-max-total-gb 20  ; cap the recordings folder; the SOLE reaper of uploaded videos now (immediate delete-after-upload was dropped - it left corrupt/discarded uploads unrecoverable). Oldest+uploaded-first past this (0/blank = unlimited). See APPLY-RECORDING-RETENTION.
        :auto-publish nil   ; cached copy of the server-side users.auto_publish flag (the server is the truth: /my/runs can flip it too; CHECK-TOKEN re-syncs from /api/me)
        :hw-encode t        ; use a GPU H.264 encoder (NVENC/AMF/QSV) when the startup probe finds one; nil forces libx264 (see PROBE-HW-ENCODER)
        :ffmpeg-path ""     ; blank = bundled copy next to the exe, or PATH
        :record-dir ""      ; blank = <user home>/Videos/RappyRuns/ (recording.lisp migrates the pre-rename folder)
        :moderator nil      ; cached /api/me role: shows the moderator-only Rooms tab + rule button on the first frame (refreshed and re-verified by CHECK-TOKEN; server enforces the real permission)
        :close-to-tray t    ; closing the window (x) hides to the system tray instead of quitting (see CLIENT-CONFIRM-DESTROY); off = x quits
        :start-minimized nil ; launch straight to the tray with no window (also forced for a single launch by --minimized; see STARTUP-MINIMIZED-P)
        :debug nil))        ; developer knobs in the GUI (see DEBUG-MODE-P)

(defvar *config* nil)

(defun config-dir ()
  (let ((appdata (or (uiop:getenv "APPDATA")
                     (namestring (user-homedir-pathname)))))
    (uiop:ensure-directory-pathname
     (merge-pathnames "ephinea-ta-client/"
                      (uiop:ensure-directory-pathname appdata)))))

(defun config-path ()
  (merge-pathnames "config.sexp" (config-dir)))

(defun read-sexp-file (path)
  "Read one form from PATH with *READ-EVAL* off; NIL if missing/corrupt."
  (when (probe-file path)
    (ignore-errors
      (with-open-file (in path :external-format :utf-8)
        (let ((*read-eval* nil))
          (read in nil nil))))))

(defun write-sexp-file (path form)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil)
            (*package* (find-package :keyword)))
        (prin1 form out))))
  form)

(defun migrate-config (config)
  "Bring an older config file up to date (pure, so the tests can pin it).
Scrubs the dropped :token-prompt-shown key (the token nudge now repeats
until a token is set) and drops any saved value for the
+FORCED-CONFIG-KEYS+ so those behaviors always fall back to the fixed
default."
  (remf config :token-prompt-shown)
  (dolist (key +forced-config-keys+)
    (remf config key))
  config)

(defun load-config! ()
  (setf *config* (migrate-config
                  (or (read-sexp-file (config-path))
                      (copy-list *default-config*)))))

(defun save-config! ()
  (write-sexp-file (config-path) *config*))

(defun debug-mode-p ()
  "Debug mode shows developer settings (currently the Server URL field,
which normal users must never change). Enabled by :debug t in
config.sexp or by launching the client with --debug."
  (or (config-value :debug)
      (and (member "--debug"
                   #+lispworks sys:*line-arguments-list*
                   #-lispworks (uiop:command-line-arguments)
                   :test #'string-equal)
           t)))

(defun startup-minimized-p ()
  "Launch straight to the tray with no window: either the :start-minimized
config is on, or --minimized was passed for this launch (the autostart
registry entry uses that flag; see autostart-win32.lisp)."
  (or (config-value :start-minimized)
      (and (member "--minimized"
                   #+lispworks sys:*line-arguments-list*
                   #-lispworks (uiop:command-line-arguments)
                   :test #'string-equal)
           t)))

(defun config-value (key)
  (unless *config* (load-config!))
  (getf *config* key (getf *default-config* key)))

(defun (setf config-value) (value key)
  (unless *config* (load-config!))
  (setf (getf *config* key) value))
