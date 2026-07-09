(in-package :ephinea-ta-client)

;;; LispWorks-only: "start with Windows" via the per-user Run key
;;; (HKCU\Software\Microsoft\Windows\CurrentVersion\Run). A string value
;;; there launches the exe at logon; deleting it turns autostart off.
;;; The registry is the single source of truth - AUTOSTART-ENABLED-P
;;; seeds the settings checkbox, and there is nothing to save in config.
;;;
;;; The autostart command carries --minimized so a logon launch goes
;;; straight to the tray (STARTUP-MINIMIZED-P in config.lisp).

(fli:register-module :advapi32 :real-name "advapi32"
                     :connection-style :automatic)

;; On 64-bit Windows the predefined HKEY is (ULONG_PTR)(LONG)0x80000001,
;; i.e. sign-extended - advapi32 compares against that exact value.
(defconstant +hkey-current-user+ #xFFFFFFFF80000001)

(defconstant +key-query-value+ #x0001)
(defconstant +key-set-value+ #x0002)
(defconstant +reg-sz+ 1)

(defparameter +run-subkey+
  "Software\\Microsoft\\Windows\\CurrentVersion\\Run")

(defparameter +autostart-value-name+ "RappyRunsClient")

(fli:define-foreign-function (%reg-create-key-ex "RegCreateKeyExW")
    ((key :pointer)
     (sub-key (:reference-pass :ef-wc-string))
     (reserved (:unsigned :long))
     (class :pointer)
     (options (:unsigned :long))
     (sam (:unsigned :long))
     (security :pointer)
     (result :pointer)               ; PHKEY (pointer to HKEY)
     (disposition :pointer))
  :result-type (:signed :long)       ; LSTATUS, 0 = ERROR_SUCCESS
  :calling-convention :stdcall
  :module :advapi32)

(fli:define-foreign-function (%reg-set-value-ex "RegSetValueExW")
    ((key :pointer)
     (value-name (:reference-pass :ef-wc-string))
     (reserved (:unsigned :long))
     (type (:unsigned :long))
     (data :pointer)
     (cb-data (:unsigned :long)))
  :result-type (:signed :long)
  :calling-convention :stdcall
  :module :advapi32)

(fli:define-foreign-function (%reg-query-value-ex "RegQueryValueExW")
    ((key :pointer)
     (value-name (:reference-pass :ef-wc-string))
     (reserved :pointer)
     (type :pointer)
     (data :pointer)
     (cb-data :pointer))             ; LPDWORD, in = buffer size, out = bytes
  :result-type (:signed :long)
  :calling-convention :stdcall
  :module :advapi32)

(fli:define-foreign-function (%reg-delete-value "RegDeleteValueW")
    ((key :pointer)
     (value-name (:reference-pass :ef-wc-string)))
  :result-type (:signed :long)
  :calling-convention :stdcall
  :module :advapi32)

(fli:define-foreign-function (%reg-close-key "RegCloseKey")
    ((key :pointer))
  :result-type (:signed :long)
  :calling-convention :stdcall
  :module :advapi32)

(defun hkcu ()
  (fli:make-pointer :address +hkey-current-user+ :type :void))

(defmacro with-run-key ((key-var) &body body)
  "Open (creating if absent) the Run key with query+set access, run
BODY with KEY-VAR bound to the HKEY, and always close it. BODY is
skipped (and the form returns NIL) when the key cannot be opened."
  (let ((phk (gensym "PHK")))
    `(fli:with-dynamic-foreign-objects ()
       (let ((,phk (fli:allocate-dynamic-foreign-object :type :pointer)))
         (if (zerop (%reg-create-key-ex
                     (hkcu) +run-subkey+ 0 fli:*null-pointer*
                     0 (logior +key-query-value+ +key-set-value+)
                     fli:*null-pointer* ,phk fli:*null-pointer*))
             (let ((,key-var (fli:dereference ,phk)))
               (unwind-protect (progn ,@body)
                 (%reg-close-key ,key-var)))
             nil)))))

(defun autostart-command ()
  "The command line to register: the current exe, quoted, plus
--minimized. NIL when the exe path is unavailable (dev/SBCL)."
  (let ((exe (ignore-errors (first (uiop:raw-command-line-arguments)))))
    (when (and exe (plusp (length exe)))
      (format nil "\"~a\" --minimized" exe))))

(defun autostart-read-value (key)
  "The current Run value's string (BMP only), or NIL when absent."
  (fli:with-dynamic-foreign-objects ()
    (let* ((max-chars 1024)
           (buf (fli:allocate-dynamic-foreign-object
                 :type '(:unsigned :short) :nelems max-chars))
           (cb (fli:allocate-dynamic-foreign-object :type '(:unsigned :long))))
      (setf (fli:dereference cb) (* 2 max-chars))
      (when (zerop (%reg-query-value-ex key +autostart-value-name+
                                        fli:*null-pointer* fli:*null-pointer*
                                        buf cb))
        (let ((chars (floor (fli:dereference cb) 2)))
          (with-output-to-string (out)
            (loop :for i :below chars
                  :for code := (fli:dereference buf :index i)
                  :until (zerop code)         ; stop at the NUL terminator
                  :do (write-char (code-char code) out))))))))

(defun autostart-enabled-p ()
  "T when the Run value exists and points at the current exe. A moved
install (stale path) reads as disabled, so re-enabling rewrites it."
  (or (with-run-key (key)
        (let ((current (autostart-read-value key))
              (wanted (autostart-command)))
          (and current wanted (string= current wanted))))
      nil))

(defun set-autostart! (enable)
  "Write (or delete) the Run value. Returns T on success."
  (with-run-key (key)
    (if enable
        (let ((command (autostart-command)))
          (when command
            (fli:with-dynamic-foreign-objects ()
              (let* ((n (length command))
                     (buf (fli:allocate-dynamic-foreign-object
                           :type '(:unsigned :short) :nelems (1+ n))))
                (dotimes (i n)
                  (setf (fli:dereference buf :index i)
                        (char-code (char command i))))
                (setf (fli:dereference buf :index n) 0)
                (zerop (%reg-set-value-ex key +autostart-value-name+ 0
                                          +reg-sz+ buf (* 2 (1+ n))))))))
        ;; Deleting a value that is not there is fine (treat as success).
        (progn (%reg-delete-value key +autostart-value-name+) t))))
