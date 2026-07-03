(in-package :ephinea-ta-client)

;;; LispWorks-only: HTTP(S) over the Windows WinHTTP API. TLS comes from
;;; the OS (SChannel), so the delivered exe needs no OpenSSL DLLs and
;;; server certificates are checked against the Windows trust store.
;;; LispWorks' own COMM SSL is unusable here: it requires OpenSSL 1.1
;;; DLLs (EOL) that end-user machines do not have.

(fli:register-module :winhttp :real-name "winhttp" :connection-style :automatic)

;; kernel32 is registered in win32.lisp, which loads before this file.
(fli:define-foreign-function (%get-last-error "GetLastError")
    ()
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%win-http-open "WinHttpOpen")
    ((agent (:reference-pass :ef-wc-string))
     (access-type (:unsigned :long))
     (proxy :pointer)
     (proxy-bypass :pointer)
     (flags (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-set-timeouts "WinHttpSetTimeouts")
    ((session :pointer)
     (resolve-ms :int)
     (connect-ms :int)
     (send-ms :int)
     (receive-ms :int))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-connect "WinHttpConnect")
    ((session :pointer)
     (host (:reference-pass :ef-wc-string))
     (port (:unsigned :short))
     (reserved (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-open-request "WinHttpOpenRequest")
    ((connection :pointer)
     (verb (:reference-pass :ef-wc-string))
     (path (:reference-pass :ef-wc-string))
     (version :pointer)
     (referrer :pointer)
     (accept-types :pointer)
     (flags (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-send-request "WinHttpSendRequest")
    ((request :pointer)
     (headers :pointer)
     (headers-length (:unsigned :long)) ; in WCHARs
     (body :pointer)
     (body-length (:unsigned :long))
     (total-length (:unsigned :long))
     (context :size-t))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-receive-response "WinHttpReceiveResponse")
    ((request :pointer)
     (reserved :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

;; Only used with WINHTTP_QUERY_FLAG_NUMBER, where the result buffer is
;; a single DWORD; BUFFER-LENGTH is in/out (pass 4, the DWORD size).
(fli:define-foreign-function (%win-http-query-headers "WinHttpQueryHeaders")
    ((request :pointer)
     (info-level (:unsigned :long))
     (name :pointer)
     (buffer (:reference-return (:unsigned :long)))
     (buffer-length (:reference (:unsigned :long)))
     (index :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-read-data "WinHttpReadData")
    ((request :pointer)
     (buffer :pointer)
     (bytes-to-read (:unsigned :long))
     (bytes-read (:reference-return (:unsigned :long))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

(fli:define-foreign-function (%win-http-close-handle "WinHttpCloseHandle")
    ((handle :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :winhttp)

(defconstant +winhttp-access-type-automatic-proxy+ 4)
(defconstant +winhttp-access-type-no-proxy+ 1)
(defconstant +winhttp-flag-secure+ #x00800000)
(defconstant +winhttp-query-status-code+ 19)
(defconstant +winhttp-query-flag-number+ #x20000000)

(define-condition winhttp-error (error)
  ((message :initarg :message :reader winhttp-error-message))
  (:report (lambda (condition stream)
             (format stream "~a" (winhttp-error-message condition)))))

(defun winhttp-fail (function-name)
  (error 'winhttp-error
         :message (format nil "~a failed (Windows error ~d)"
                          function-name (%get-last-error))))

(defmacro with-winhttp-handle ((var form function-name) &body body)
  `(let ((,var ,form))
     (when (fli:null-pointer-p ,var)
       (winhttp-fail ,function-name))
     (unwind-protect (progn ,@body)
       (%win-http-close-handle ,var))))

(defun open-winhttp-session ()
  ;; AUTOMATIC_PROXY needs Windows 8.1; fall back for older systems.
  (let ((session (%win-http-open "ephinea-ta-client"
                                 +winhttp-access-type-automatic-proxy+
                                 fli:*null-pointer* fli:*null-pointer* 0)))
    (if (fli:null-pointer-p session)
        (%win-http-open "ephinea-ta-client"
                        +winhttp-access-type-no-proxy+
                        fli:*null-pointer* fli:*null-pointer* 0)
        session)))

(defun send-winhttp-request (request header-string body)
  "Sends headers plus BODY (an octet vector or NIL) in one call."
  (fli:with-dynamic-foreign-objects ()
    (let ((body-length (length body))
          (body-pointer fli:*null-pointer*))
      (when body
        (setf body-pointer (fli:allocate-dynamic-foreign-object
                            :type '(:unsigned :byte)
                            :nelems (max 1 body-length)))
        (dotimes (i body-length)
          (setf (fli:dereference body-pointer :index i) (aref body i))))
      (flet ((send (headers-pointer headers-length)
               (%win-http-send-request request headers-pointer headers-length
                                       body-pointer body-length body-length 0)))
        (unless (if (plusp (length header-string))
                    (fli:with-foreign-string (pointer element-count byte-count
                                              :external-format :unicode)
                        header-string
                      (declare (ignore byte-count))
                      (send pointer (1- element-count)))
                    (send fli:*null-pointer* 0))
          (winhttp-fail "WinHttpSendRequest"))))))

(defun read-winhttp-status (request)
  (multiple-value-bind (ok status)
      (%win-http-query-headers request
                               (logior +winhttp-query-status-code+
                                       +winhttp-query-flag-number+)
                               fli:*null-pointer*
                               0 4 ; dummy for the DWORD out-buffer, its size
                               fli:*null-pointer*)
    (unless ok
      (winhttp-fail "WinHttpQueryHeaders"))
    status))

(defun read-winhttp-body (request)
  (fli:with-dynamic-foreign-objects ()
    (let* ((chunk-size 8192)
           (chunk (fli:allocate-dynamic-foreign-object
                   :type '(:unsigned :byte) :nelems chunk-size))
           (bytes (make-array 4096 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
      (loop
        (multiple-value-bind (ok bytes-read)
            (%win-http-read-data request chunk chunk-size 0)
          (unless ok
            (winhttp-fail "WinHttpReadData"))
          (when (zerop bytes-read)
            (return bytes))
          (dotimes (i bytes-read)
            (vector-push-extend (fli:dereference chunk :index i) bytes)))))))

(defun winhttp-request (method scheme host port path &key headers body)
  "Blocking HTTP(S) request. HEADERS is an alist of (name . value)
strings; BODY is an octet vector or NIL. Returns (values status-code
body-octets). Signals WINHTTP-ERROR on transport failures."
  (let ((header-string
          (format nil "~{~a~}"
                  (loop :for (name . value) :in headers
                        :collect (format nil "~a: ~a~c~c" name value
                                         #\Return #\Linefeed)))))
    (with-winhttp-handle (session (open-winhttp-session) "WinHttpOpen")
      (%win-http-set-timeouts session 10000 10000 30000 30000)
      (with-winhttp-handle (connection (%win-http-connect session host port 0)
                            "WinHttpConnect")
        (with-winhttp-handle (request (%win-http-open-request
                                       connection method path
                                       fli:*null-pointer* fli:*null-pointer*
                                       fli:*null-pointer*
                                       (if (string= scheme "https")
                                           +winhttp-flag-secure+
                                           0))
                              "WinHttpOpenRequest")
          (send-winhttp-request request header-string body)
          (unless (%win-http-receive-response request fli:*null-pointer*)
            (winhttp-fail "WinHttpReceiveResponse"))
          (values (read-winhttp-status request)
                  (read-winhttp-body request)))))))
