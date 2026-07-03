(in-package :ephinea-ta-client)

;;; LispWorks-only: Win32 bindings and the live process-memory reader.
;;; PSOBB is a 32-bit process, so all addresses fit in 32 bits and can be
;;; read cross-bitness from 64-bit LispWorks with ReadProcessMemory.

(fli:register-module :user32 :real-name "user32" :connection-style :automatic)
(fli:register-module :kernel32 :real-name "kernel32" :connection-style :automatic)

(fli:define-foreign-function (%find-window "FindWindowW")
    ((class-name :pointer)
     (window-name (:reference-pass :ef-wc-string)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-window-text "GetWindowTextW")
    ((hwnd :pointer)
     (buffer :pointer)
     (max-count :int))
  :result-type :int
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-window-thread-process-id "GetWindowThreadProcessId")
    ((hwnd :pointer)
     (process-id (:reference-return (:unsigned :long))))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%open-process "OpenProcess")
    ((desired-access (:unsigned :long))
     (inherit-handle (:boolean :int))
     (process-id (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

;; LPCVOID base address and SIZE_T sizes are pointer-width (8 bytes on
;; 64-bit LispWorks); :size-t matches, DWORD/:long would not.
(fli:define-foreign-function (%read-process-memory "ReadProcessMemory")
    ((process :pointer)
     (base-address :size-t)
     (buffer :pointer)
     (size :size-t)
     (bytes-read (:reference-return :size-t)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%close-handle "CloseHandle")
    ((handle :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%get-exit-code-process "GetExitCodeProcess")
    ((process :pointer)
     (exit-code (:reference-return (:unsigned :long))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defconstant +process-vm-read+ #x0010)
(defconstant +process-query-information+ #x0400)
(defconstant +still-active+ 259)

(defparameter +psobb-window-names+
  '("Ephinea: Phantasy Star Online Blue Burst"
    "PHANTASY STAR ONLINE Blue Burst"))

(defclass live-reader ()
  ((handle :initarg :handle :accessor live-reader-handle)
   (pid :initarg :pid :reader live-reader-pid)
   (window-title :initarg :window-title :initform nil
                 :reader live-reader-window-title)
   (buffer :initform nil :accessor live-reader-buffer)
   (buffer-size :initform 0 :accessor live-reader-buffer-size)))

(defmethod reader-window-title ((reader live-reader))
  (live-reader-window-title reader))

(defun find-psobb-window ()
  (dolist (name +psobb-window-names+)
    (let ((hwnd (%find-window fli:*null-pointer* name)))
      (unless (fli:null-pointer-p hwnd)
        (return hwnd)))))

(defun window-title (hwnd)
  "The window's actual title via GetWindowTextW, or NIL (BMP only).
The recorder passes this to ffmpeg's gdigrab title= input, which needs
an exact match - the FindWindowW candidate name is not good enough."
  (fli:with-dynamic-foreign-objects ()
    (let* ((max-count 256)
           (buffer (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :short) :nelems max-count))
           (length (%get-window-text hwnd buffer max-count)))
      (when (plusp length)
        (with-output-to-string (out)
          (dotimes (i length)
            (write-char (code-char (fli:dereference buffer :index i))
                        out)))))))

(defun open-psobb-reader ()
  "Attach to a running PSOBB process; NIL when not found."
  (let ((hwnd (find-psobb-window)))
    (when hwnd
      ;; :reference-return parameters take a dummy argument at call time.
      (multiple-value-bind (thread-id pid)
          (%get-window-thread-process-id hwnd 0)
        (declare (ignore thread-id))
        (when (plusp pid)
          (let ((handle (%open-process
                         (logior +process-vm-read+ +process-query-information+)
                         nil pid)))
            (unless (fli:null-pointer-p handle)
              (make-instance 'live-reader :handle handle :pid pid
                                          :window-title (window-title hwnd)))))))))

(defun close-reader (reader)
  (when (live-reader-buffer reader)
    (fli:free-foreign-object (live-reader-buffer reader))
    (setf (live-reader-buffer reader) nil
          (live-reader-buffer-size reader) 0))
  (%close-handle (live-reader-handle reader)))

(defun reader-alive-p (reader)
  (multiple-value-bind (ok code)
      (%get-exit-code-process (live-reader-handle reader) 0)
    (and ok (= code +still-active+))))

(defun ensure-buffer (reader size)
  (when (> size (live-reader-buffer-size reader))
    (when (live-reader-buffer reader)
      (fli:free-foreign-object (live-reader-buffer reader)))
    (setf (live-reader-buffer reader)
          (fli:allocate-foreign-object :type '(:unsigned :byte) :nelems size)
          (live-reader-buffer-size reader) size))
  (live-reader-buffer reader))

(defmethod read-block ((reader live-reader) address size)
  (let ((buffer (ensure-buffer reader size)))
    (multiple-value-bind (ok bytes-read)
        (%read-process-memory (live-reader-handle reader)
                              address buffer size 0)
      (when (and ok (= bytes-read size))
        (let ((result (make-array size :element-type '(unsigned-byte 8))))
          (dotimes (i size result)
            (setf (aref result i)
                  (fli:dereference buffer :index i))))))))
