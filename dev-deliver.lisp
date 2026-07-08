;;; LispWorks delivery script for a LOCAL DEV build. Produces
;;; client/dist/RappyRunsClientDev.exe.
;;;
;;;   "C:/Program Files/LispWorks/lispworks-8-1-0-x64-windows.exe" ^
;;;       -build client/dev-deliver.lisp
;;;
;;; Same as deliver.lisp EXCEPT it does NOT bake client/VERSION into the
;;; image. *CLIENT-VERSION* stays NIL, so the startup auto-update gate
;;; (main.lisp: (when (and (config-value :auto-update) *client-version*) ...))
;;; never fires and "Check for updates" only reports a dev build. This
;;; keeps a test exe from overwriting itself with the latest release.
;;;
;;; Output is named RappyRunsClientDev.exe so it never mixes with the
;;; distributed RappyRunsClient.exe. Ship the exe together with
;;; data/quest-triggers.sexp (and optionally ffmpeg/ffmpeg.exe) next to it.
(in-package "CL-USER")

(load-all-patches)

(defvar *client-root*
  (make-pathname :name nil :type nil :defaults *load-pathname*))

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(push *client-root* asdf:*central-registry*)

;; Delete the client's cached fasls BEFORE loading, so the build compiles
;; every component fresh (see deliver.lisp for the layout-consistency
;; rationale).
(let* ((sample (asdf:apply-output-translations
                (merge-pathnames "src/main.lisp" *client-root*)))
       (cache-dir (uiop:pathname-directory-pathname sample)))
  (format t "~&; clearing fasl cache: ~a~%" cache-dir)
  (uiop:delete-directory-tree cache-dir :validate t
                                        :if-does-not-exist :ignore))

(funcall (intern "QUICKLOAD" "QL") :ephinea-ta-client)

;; NOTE: VERSION is intentionally NOT baked here (unlike deliver.lisp):
;; *CLIENT-VERSION* stays NIL so auto-update is inert in this dev build.

(ensure-directories-exist (merge-pathnames "dist/" *client-root*))

(deliver (intern "MAIN" "EPHINEA-TA-CLIENT")
         (namestring (merge-pathnames "dist/RappyRunsClientDev" *client-root*))
         0                       ; delivery level: start low, raise once stable
         :interface :capi
         :icon-file (namestring (merge-pathnames "icon.ico" *client-root*))
         :startup-bitmap-file nil) ; no "built with LispWorks" splash
