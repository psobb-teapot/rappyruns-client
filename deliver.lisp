;;; LispWorks delivery script. Produces client/dist/EphineaTAClient.exe:
;;;
;;;   "C:/Program Files/LispWorks/lispworks-8-1-0-x64-windows.exe" ^
;;;       -build client/deliver.lisp
;;;
;;; Ship the exe together with data/quest-triggers.sexp (the client looks
;;; for data/quest-triggers.sexp next to the executable).
;;;
;;; NOTE: -build reads one top-level form at a time, so forms referencing
;;; the ASDF/QL packages must come after the form that loads quicklisp.
(in-package "CL-USER")

(load-all-patches)

(defvar *client-root*
  (make-pathname :name nil :type nil :defaults *load-pathname*))

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(push *client-root* asdf:*central-registry*)

(funcall (intern "QUICKLOAD" "QL") :ephinea-ta-client)

;; Bake client/VERSION into the image so the self-updater can compare
;; itself against the latest release tag. Failing the build on a missing
;; or malformed file keeps every delivered exe versioned.
(let* ((version-path (merge-pathnames "VERSION" *client-root*))
       (version (with-open-file (in version-path)
                  (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                               (read-line in)))))
  (unless (funcall (intern "PARSE-VERSION" "EPHINEA-TA-CLIENT") version)
    (error "client/VERSION must contain an X.Y.Z version (got: ~s)" version))
  (setf (symbol-value (intern "*CLIENT-VERSION*" "EPHINEA-TA-CLIENT")) version))

(ensure-directories-exist (merge-pathnames "dist/" *client-root*))

(deliver (intern "MAIN" "EPHINEA-TA-CLIENT")
         (namestring (merge-pathnames "dist/EphineaTAClient" *client-root*))
         0                       ; delivery level: start low, raise once stable
         :interface :capi
         :startup-bitmap-file nil) ; no "built with LispWorks" splash
