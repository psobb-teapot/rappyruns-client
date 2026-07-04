(in-package :ephinea-ta-client)

;;; LispWorks-only: Win32 bindings and the live process-memory reader.
;;; PSOBB is a 32-bit process, so all addresses fit in 32 bits and can be
;;; read cross-bitness from 64-bit LispWorks with ReadProcessMemory.

(fli:register-module :user32 :real-name "user32" :connection-style :automatic)
(fli:register-module :kernel32 :real-name "kernel32" :connection-style :automatic)
(fli:register-module :shell32 :real-name "shell32" :connection-style :automatic)

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

(fli:define-foreign-function (%write-file "WriteFile")
    ((handle :pointer)
     (buffer :pointer)
     (bytes-to-write (:unsigned :long))
     (bytes-written (:reference-return (:unsigned :long)))
     (overlapped :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%get-exit-code-process "GetExitCodeProcess")
    ((process :pointer)
     (exit-code (:reference-return (:unsigned :long))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

;; ShellExecuteW takes the URL as a single lpFile argument, so nothing
;; is ever parsed by a shell; returns an HINSTANCE > 32 on success.
(fli:define-foreign-function (%shell-execute "ShellExecuteW")
    ((hwnd :pointer)
     (operation (:reference-pass :ef-wc-string))
     (file (:reference-pass :ef-wc-string))
     (parameters :pointer)
     (directory :pointer)
     (show-cmd :int))
  :result-type :pointer
  :calling-convention :stdcall
  :module :shell32)

;; Same entry point with a real lpParameters string, for the rare case
;; where arguments are needed (explorer.exe /select). The primary
;; %shell-execute keeps :pointer there so URLs stay single-argument.
(fli:define-foreign-function (%shell-execute-args "ShellExecuteW")
    ((hwnd :pointer)
     (operation (:reference-pass :ef-wc-string))
     (file (:reference-pass :ef-wc-string))
     (parameters (:reference-pass :ef-wc-string))
     (directory :pointer)
     (show-cmd :int))
  :result-type :pointer
  :calling-convention :stdcall
  :module :shell32)

;;; Clipboard access, for noticing a copied YouTube URL. The sequence
;;; number changes on every clipboard update, so the poll loop only pays
;;; for OpenClipboard when there is something new to look at.

(fli:define-foreign-function (%get-clipboard-sequence-number
                              "GetClipboardSequenceNumber")
    ()
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%open-clipboard "OpenClipboard")
    ((hwnd :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%close-clipboard "CloseClipboard")
    ()
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%is-clipboard-format-available
                              "IsClipboardFormatAvailable")
    ((format (:unsigned :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-clipboard-data "GetClipboardData")
    ((format (:unsigned :int)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%global-lock "GlobalLock")
    ((handle :pointer))
  :result-type (:pointer (:unsigned :short))
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%global-unlock "GlobalUnlock")
    ((handle :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defconstant +cf-unicodetext+ 13)

(defun clipboard-sequence-number ()
  (%get-clipboard-sequence-number))

(defun clipboard-text (&key (max-chars 2048))
  "The clipboard's unicode text (BMP only, MAX-CHARS cap), or NIL.
Never signals: this is polled from the GUI loop, and a slow or hostile
clipboard owner must not take the client down with it."
  (ignore-errors
    (when (and (%is-clipboard-format-available +cf-unicodetext+)
               (%open-clipboard fli:*null-pointer*))
      (unwind-protect
           (let ((handle (%get-clipboard-data +cf-unicodetext+)))
             (unless (fli:null-pointer-p handle)
               (let ((chars (%global-lock handle)))
                 (unless (fli:null-pointer-p chars)
                   (unwind-protect
                        (with-output-to-string (out)
                          (loop :for i :from 0 :below max-chars
                                :for code := (fli:dereference chars :index i)
                                :until (zerop code)
                                :do (write-char (code-char code) out)))
                     (%global-unlock handle))))))
        (%close-clipboard)))))

;; Asynchronous and thread-safe, so the poll loop can call it directly;
;; plays the user's sound scheme (and respects mute).
(fli:define-foreign-function (%message-beep "MessageBeep")
    ((type (:unsigned :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(defconstant +sw-shownormal+ 1)
(defconstant +mb-iconasterisk+ #x40)   ; notification sound
(defconstant +mb-iconhand+ #x10)       ; error sound

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
