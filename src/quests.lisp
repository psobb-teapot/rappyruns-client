(in-package :ephinea-ta-client)

;;; Quest trigger definitions: which memory condition marks the start and
;;; end of each supported quest. The data lives in data/quest-triggers.sexp
;;; next to the executable so new quests can be added without rebuilding.
;;; Slugs must match the server's seed.lisp slugify output; the client
;;; cross-checks them against GET /api/quests at startup.

(defstruct quest-def
  slug        ; server-side quest slug, e.g. "ep1-towards-the-future"
  episode     ; site episode (1, 2 or 4)
  names       ; in-game quest names that map to this entry
  number      ; in-game quest number, NIL when the quest has none
  start       ; trigger: (:register N) | (:warp-in) | (:floor-switch FLOOR ID)
  end)        ; trigger: (:register N) | (:floor-switch FLOOR ID)

(defvar *quest-defs* '())

(defun quest-triggers-path ()
  "data/quest-triggers.sexp next to the executable (delivered image) or
relative to this system's source directory (development)."
  (let* ((exe (ignore-errors (first (uiop:raw-command-line-arguments))))
         (image-relative
           (and exe
                (probe-file
                 (merge-pathnames "data/quest-triggers.sexp"
                                  (uiop:pathname-directory-pathname exe))))))
    (or image-relative
        (asdf:system-relative-pathname :ephinea-ta-client "data/quest-triggers.sexp"))))

(defun load-quest-defs (&optional (path (quest-triggers-path)))
  (let ((forms (with-open-file (in path :external-format :utf-8)
                 (let ((*read-eval* nil)
                       (*package* (find-package :keyword)))
                   (read in nil nil)))))
    (setf *quest-defs*
          (loop :for entry :in forms
                :collect (make-quest-def
                          :slug (getf entry :slug)
                          :episode (getf entry :episode)
                          :names (getf entry :names)
                          :number (getf entry :number)
                          :start (getf entry :start)
                          :end (getf entry :end))))
    (length *quest-defs*)))

(defun find-quest-def (&key number episode name (defs *quest-defs*))
  "Match like psostats: by in-game quest number first, then by
episode + in-game name."
  (or (and number (plusp number)
           (find number defs :key #'quest-def-number))
      (and name
           (find-if (lambda (def)
                      (and (or (null episode)
                               (eql episode (quest-def-episode def)))
                           (member name (quest-def-names def) :test #'string=)))
                    defs))))

(defun unknown-slugs (server-slugs &optional (defs *quest-defs*))
  "Trigger definitions whose slug the server does not know (misconfiguration)."
  (loop :for def :in defs
        :unless (member (quest-def-slug def) server-slugs :test #'string=)
          :collect (quest-def-slug def)))
