(in-package :ephinea-ta-client)

;;; login.txt: the file-based alternative to browser pairing. The user
;;; sets a client password on the site (/my/tokens) and drops a
;;; login.txt next to RappyRunsClient.exe:
;;;
;;;   username=TheirDiscordName
;;;   password=their-client-password
;;;
;;; At startup the client exchanges the pair for an ordinary API token
;;; over POST /api/login (LOGIN-WITH-PASSWORD in api-client.lisp;
;;; gui.lisp runs the choreography). Parsing is pure CL so the tests
;;; cover it on SBCL.

(defparameter +credentials-file+ "login.txt")

(defun credentials-directory ()
  "The folder next to the exe. LW:LISP-IMAGE-NAME is authoritative in
the delivered build (argv[0] can be a bare relative name; see
updater.lisp); dev images fall back to the current directory."
  #+lispworks (uiop:pathname-directory-pathname (lw:lisp-image-name))
  #-lispworks (uiop:getcwd))

(defun credentials-path ()
  (merge-pathnames +credentials-file+ (credentials-directory)))

(defun credentials-present-p ()
  (and (ignore-errors (probe-file (credentials-path))) t))

(defun parse-credentials (text)
  "KEY=VALUE lines -> (values username password), or (values NIL NIL)
when either is missing or empty. Tolerant of what Notepad produces:
a UTF-8 BOM, CRLF line ends, blank lines, #-comment lines and spaces
around keys. Only the first = splits, so passwords may contain =."
  (let ((username nil)
        (password nil)
        (junk (list #\Space #\Tab #\Return (code-char #xFEFF))))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((line (string-trim junk line)))
        (unless (or (string= line "") (char= (char line 0) #\#))
          (let ((separator (position #\= line)))
            (when separator
              (let ((key (string-downcase
                          (string-trim junk (subseq line 0 separator))))
                    (value (string-trim junk (subseq line (1+ separator)))))
                (cond ((string= key "username") (setf username value))
                      ((string= key "password") (setf password value)))))))))
    (if (and username password
             (string/= username "") (string/= password ""))
        (values username password)
        (values nil nil))))

(defun read-credentials (&optional (path (credentials-path)))
  "Username and password from PATH, as PARSE-CREDENTIALS values;
\(values NIL NIL) when the file is absent or unreadable (e.g. saved in
a non-UTF-8 encoding)."
  (let ((text (ignore-errors
               (uiop:read-file-string path :external-format :utf-8))))
    (if text
        (parse-credentials text)
        (values nil nil))))
