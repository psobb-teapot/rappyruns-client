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

(defvar *quest-defs* '()
  "Active detection definitions: the builtin sexp merged with any
trigger-carrying quests fetched from the server.")

(defvar *builtin-quest-defs* '()
  "Definitions from data/quest-triggers.sexp, kept separate so refreshing
the server-defined ones does not drop or duplicate them.")

(defvar *server-quest-defs* '()
  "Definitions built from GET /api/quests (moderator-created categories).")

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

(defun recompute-quest-defs ()
  "Server categories win over builtin ones on slug collision."
  (let ((server-slugs (mapcar #'quest-def-slug *server-quest-defs*)))
    (setf *quest-defs*
          (append *server-quest-defs*
                  (remove-if (lambda (def)
                               (member (quest-def-slug def) server-slugs
                                       :test #'string=))
                             *builtin-quest-defs*)))))

(defun load-quest-defs (&optional (path (quest-triggers-path)))
  (let ((forms (with-open-file (in path :external-format :utf-8)
                 (let ((*read-eval* nil)
                       (*package* (find-package :keyword)))
                   (read in nil nil)))))
    (setf *builtin-quest-defs*
          (loop :for entry :in forms
                :collect (make-quest-def
                          :slug (getf entry :slug)
                          :episode (getf entry :episode)
                          :names (getf entry :names)
                          :number (getf entry :number)
                          :start (getf entry :start)
                          :end (getf entry :end))))
    (recompute-quest-defs)
    (length *builtin-quest-defs*)))

(defun json-trigger (object)
  "One trigger JSON object (from GET /api/quests) -> internal form, or NIL."
  (when (hash-table-p object)
    (let ((type (gethash "type" object)))
      (cond
        ((equal type "warp-in") '(:warp-in))
        ((equal type "register") (list :register (gethash "register" object)))
        ((equal type "floor-switch")
         (list :floor-switch (gethash "floor" object) (gethash "switch" object)))
        ((equal type "monster")
         (list :monster-dead (gethash "monster" object)))))))

(defun server-quest->def (quest)
  "A GET /api/quests entry -> quest-def, or NIL when it carries no
detection triggers (a display-only catalog quest)."
  (let ((start (json-trigger (gethash "start" quest)))
        (end (json-trigger (gethash "end" quest))))
    (when (and start end)
      (let ((names (gethash "game_names" quest)))
        (make-quest-def
         :slug (gethash "slug" quest)
         :episode (gethash "episode" quest)
         :names (and names (coerce names 'list))
         :number (gethash "game_number" quest)
         :start start
         :end end)))))

(defun set-server-quest-defs (quests)
  "QUESTS is the parsed GET /api/quests vector. Rebuild the server-defined
detection entries and merge them with the builtin ones. Returns the
number of timeable server categories."
  (setf *server-quest-defs*
        (loop :for quest :across quests
              :for def := (server-quest->def quest)
              :when def :collect def))
  (recompute-quest-defs)
  (length *server-quest-defs*))

(defun quest-def-matches-p (def &key number episode name)
  "Match like psostats: by in-game quest number, or by episode + name."
  (or (and number (plusp number)
           (eql number (quest-def-number def)))
      (and name
           (or (null episode)
               (eql episode (quest-def-episode def)))
           (member name (quest-def-names def) :test #'string=))))

(defun find-quest-defs (&key number episode name (defs *quest-defs*))
  "All definitions matching the loaded quest. Several can match at once:
the full clear plus segment categories (e.g. \"(2 Rooms)\") that reuse
the same quest number with an earlier end trigger."
  (remove-if-not (lambda (def)
                   (quest-def-matches-p def :number number
                                            :episode episode :name name))
                 defs))

(defun find-quest-def (&key number episode name (defs *quest-defs*))
  (first (find-quest-defs :number number :episode episode :name name
                          :defs defs)))

(defun unknown-slugs (server-slugs &optional (defs *quest-defs*))
  "Trigger definitions whose slug the server does not know (misconfiguration)."
  (loop :for def :in defs
        :unless (member (quest-def-slug def) server-slugs :test #'string=)
          :collect (quest-def-slug def)))
