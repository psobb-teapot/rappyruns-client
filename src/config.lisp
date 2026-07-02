(in-package :ephinea-ta-client)

;;; Config lives in %APPDATA%/ephinea-ta-client/config.sexp as a plist.

(defparameter *default-config*
  (list :server-url "http://localhost:8080"
        :api-token ""
        :auto-submit t
        :trigger-log nil))

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
  *config*)

(defun save-config! ()
  (write-sexp-file (config-path) *config*))

(defun config-value (key)
  (unless *config* (load-config!))
  (getf *config* key (getf *default-config* key)))

(defun (setf config-value) (value key)
  (unless *config* (load-config!))
  (setf (getf *config* key) value))
