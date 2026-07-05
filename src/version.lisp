(in-package :ephinea-ta-client)

;;; The client's own version, used by the self-updater (updater.lisp).
;;; *CLIENT-VERSION* stays NIL when running from source, so a dev image
;;; never offers to replace itself; deliver.lisp bakes client/VERSION
;;; into the exe, and release.ps1 refuses to publish unless that file
;;; matches the release tag.

(defvar *client-version* nil
  "Version string baked in at delivery time (e.g. \"0.6.0\"); NIL in dev.")

(defun client-version ()
  "The version to show humans; \"dev\" when running from source."
  (or *client-version* "dev"))

(defun parse-version (string)
  "\"v1.2.3\" or \"1.2.3\" -> (1 2 3); NIL for anything else. Exactly
three non-negative components - suffixes like -rc1 are rejected, so a
malformed tag can never look newer than a real release."
  (when (and (stringp string) (plusp (length string)))
    (let ((start (if (char-equal (char string 0) #\v) 1 0))
          (parts '()))
      (loop
        (let* ((dot (position #\. string :start start))
               (end (or dot (length string)))
               (number (and (< start end)
                            (ignore-errors
                              (parse-integer string :start start :end end)))))
          (unless (and number (>= number 0))
            (return-from parse-version nil))
          (push number parts)
          (unless dot
            (return))
          (setf start (1+ dot))))
      (when (= (length parts) 3)
        (nreverse parts)))))

(defun version< (a b)
  "Numeric (not textual) comparison of two PARSE-VERSION lists."
  (loop :for x :in a
        :for y :in b
        :do (cond ((< x y) (return t))
                  ((> x y) (return nil)))
        :finally (return nil)))

(defun update-available-p (current latest)
  "T only when both version strings parse and CURRENT < LATEST.
Anything malformed (including a NIL dev version) falls on the
do-not-update side."
  (let ((current (parse-version current))
        (latest (parse-version latest)))
    (and current latest (version< current latest))))
