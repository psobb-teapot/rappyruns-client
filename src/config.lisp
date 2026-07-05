(in-package :ephinea-ta-client)

;;; Config lives in %APPDATA%/ephinea-ta-client/config.sexp as a plist.

(defparameter *default-config*
  (list :server-url "https://ephinea-ta-production.up.railway.app"
        :api-token ""
        :language :en      ; UI language, :en or :ja (i18n.lisp)
        :auto-submit t
        :auto-update t      ; install new GitHub releases at startup, unattended (updater.lisp)
        :completion-sound t
        :trigger-log nil
        :record-enabled t
        :record-audio t     ; game-only capture (process loopback; see audio-win32)
        :ffmpeg-path ""     ; blank = bundled copy next to the exe, or PATH
        :record-dir ""      ; blank = <user home>/Videos/EphineaTA/
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

(defun load-config! ()
  (setf *config* (or (read-sexp-file (config-path))
                     (copy-list *default-config*)))
  ;; Dropped key: the token nudge now repeats until a token is set, so
  ;; the old one-time flag is scrubbed from older config files.
  (remf *config* :token-prompt-shown)
  *config*)

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

(defun config-value (key)
  (unless *config* (load-config!))
  (getf *config* key (getf *default-config* key)))

(defun (setf config-value) (value key)
  (unless *config* (load-config!))
  (setf (getf *config* key) value))
