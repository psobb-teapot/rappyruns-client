(in-package :ephinea-ta-client)

;;; The quest-rule form's pure logic: which fetched quests can parent a
;;; rule, matching the just-played quest to one of them, trigger
;;; preview/resolution and the API error text. Split out of gui.lisp
;;; (LispWorks-only) so SBCL's client-tests can reach it - the GUI
;;; keeps only the CAPI panes and callbacks.

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

(defun moderator-role-p (role)
  "True when a /api/me ROLE string grants quest authoring (moderator or
admin); mirrors the server's MODELS:MODERATOR-P."
  (and (member role '("moderator" "admin") :test #'equal) t))
