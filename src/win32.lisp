(in-package :ephinea-ta-client)

;;; LispWorks-only: Win32 bindings and the live process-memory reader.
;;; PSOBB is a 32-bit process, so all addresses fit in 32 bits and can be
;;; read cross-bitness from 64-bit LispWorks with ReadProcessMemory.

(fli:register-module :user32 :real-name "user32" :connection-style :automatic)
(fli:register-module :kernel32 :real-name "kernel32" :connection-style :automatic)
(fli:register-module :shell32 :real-name "shell32" :connection-style :automatic)

;; Shared by every -win32 file (winhttp's status checks, the tray's
;; mutex guard, ffmpeg's pipe errors); defined once here, first Win32
;; file in the system.
(fli:define-foreign-function (%get-last-error "GetLastError")
    ()
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :kernel32)

(defun win32-log (fmt &rest args)
  "Forward to RECORDING-LOG (defined in a later-loading file) so this
early file can log attach/read failures without a compile-time forward
reference. A no-op until the logger is loaded (never, in practice, by
the time the poll loop runs)."
  (when (fboundp 'recording-log)
    (apply 'recording-log fmt args)))

(defun win32-error-label (code)
  "Human tag for the GetLastError values the reader actually hits, so a
log tail names the cause instead of a bare number."
  (case code
    (0 "no error")
    (5 "ERROR_ACCESS_DENIED")
    (6 "ERROR_INVALID_HANDLE")
    (299 "ERROR_PARTIAL_COPY")
    (t "error")))

(defun utf16-buffer-string (buffer count)
  "COUNT UTF-16 code units from the foreign (:unsigned :short) BUFFER
as a string. BMP only - no surrogate pairing, like every caller."
  (with-output-to-string (out)
    (dotimes (i count)
      (write-char (code-char (fli:dereference buffer :index i)) out))))

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

;; EnumWindows enumerates every top-level window so 2-window (shop + play)
;; setups can be listed, not just the single one FindWindow returns.
(fli:define-foreign-function (%enum-windows "EnumWindows")
    ((callback :pointer)
     (lparam :pointer))
  :result-type (:boolean :int)
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

;; The self-updater (updater.lisp) hands our PID to its helper script,
;; which waits for this process to exit before swapping the exe.
(fli:define-foreign-function (%get-current-process-id "GetCurrentProcessId")
    ()
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :kernel32)

;; Total physical RAM, for the recorder's low-memory profile
;; (LOW-MEMORY-MACHINE-P): on an 8 GB machine the capture bitrate is
;; lowered to shrink the file-cache churn of recording + remux + upload.
(fli:define-c-struct memorystatusex
  (length (:unsigned :long))
  (memory-load (:unsigned :long))
  (total-phys (:unsigned :long-long))
  (avail-phys (:unsigned :long-long))
  (total-page-file (:unsigned :long-long))
  (avail-page-file (:unsigned :long-long))
  (total-virtual (:unsigned :long-long))
  (avail-virtual (:unsigned :long-long))
  (avail-extended-virtual (:unsigned :long-long)))

(fli:define-foreign-function (%global-memory-status-ex "GlobalMemoryStatusEx")
    ((buffer (:pointer (:struct memorystatusex))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defun %physical-memory-bytes ()
  "Total physical RAM in bytes, or NIL when the call fails."
  (fli:with-dynamic-foreign-objects ((status (:struct memorystatusex)))
    (setf (fli:foreign-slot-value status 'length)
          (fli:size-of '(:struct memorystatusex)))
    (when (%global-memory-status-ex status)
      (fli:foreign-slot-value status 'total-phys))))

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
   (buffer-size :initform 0 :accessor live-reader-buffer-size)
   ;; LAST-READ-ERROR keeps the GetLastError of the most recent failed
   ;; ReadProcessMemory (see READ-BLOCK); the poll loop logs it once reads
   ;; have been dead for a while. UNREADABLE-FRAMES counts consecutive
   ;; attached frames that produced no snapshot at all - the "attached but
   ;; cannot read" signal the GUI shows - so a single transient failed
   ;; pointer-chase mid-snapshot never trips it (that is normal during
   ;; warps and quest reloads); only a genuinely unreadable process does.
   (last-read-error :initform nil :accessor live-reader-last-read-error)
   (unreadable-frames :initform 0 :accessor live-reader-unreadable-frames)))

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
        (utf16-buffer-string buffer length)))))

(defvar *psobb-window-scan* nil
  "Accumulator bound by FIND-PSOBB-WINDOWS while ETA_PSOBB_ENUM_CB runs;
each matching window is pushed as (hwnd . pid).")

(fli:define-foreign-callable ("eta_psobb_enum_cb"
                              :result-type (:boolean :int)
                              :calling-convention :stdcall)
    ((hwnd :pointer)
     (lparam :pointer))
  ;; Runs synchronously on this thread for each top-level window during
  ;; %ENUM-WINDOWS, so the dynamic *PSOBB-WINDOW-SCAN* binding is in
  ;; effect. Returns T to keep enumerating.
  (declare (ignore lparam))
  (let ((title (ignore-errors (window-title hwnd))))
    (when (and title (member title +psobb-window-names+ :test #'string=))
      (multiple-value-bind (thread-id pid)
          (%get-window-thread-process-id hwnd 0)
        (declare (ignore thread-id))
        (when (plusp pid)
          (push (cons hwnd pid) *psobb-window-scan*)))))
  t)

(defun hwnd-pid (hwnd)
  (multiple-value-bind (thread-id pid) (%get-window-thread-process-id hwnd 0)
    (declare (ignore thread-id))
    (and (plusp pid) pid)))

(defun find-psobb-windows ()
  "Every top-level PSOBB window as (hwnd . pid). Guarded by a cheap
FindWindow first: while no PSOBB window exists (the common idle case)
this does at most two FindWindow lookups and never enumerates - so the
once-a-second search does not decode every desktop window's title. Only
once a window is present do we EnumWindows to catch a second (shop)
window; EnumWindows failure falls back to that single primary window."
  (let ((primary (find-psobb-window)))
    (when primary
      (or (ignore-errors
            (let ((*psobb-window-scan* '()))
              (%enum-windows (fli:make-pointer :symbol-name "eta_psobb_enum_cb")
                             fli:*null-pointer*)
              (nreverse *psobb-window-scan*)))
          (let ((pid (hwnd-pid primary)))
            (and pid (list (cons primary pid))))))))

(defvar *open-process-last-log* nil
  "(pid . err) of the last OpenProcess failure logged; the once-a-second
search loop would otherwise log the same failure on every probe.")

(defun maybe-log-open-failure (pid err)
  (unless (equal *open-process-last-log* (cons pid err))
    (setf *open-process-last-log* (cons pid err))
    (win32-log "OpenProcess failed: pid ~d err ~d (~a)~@[ - ~a~]"
               pid err (win32-error-label err)
               (and (= err 5) "run the client as administrator"))))

(defun open-reader-for (hwnd pid)
  "A live-reader for PSOBB process PID (window HWND), or NIL with a
logged reason on failure."
  (let ((handle (%open-process
                 (logior +process-vm-read+ +process-query-information+)
                 nil pid)))
    (cond ((not (fli:null-pointer-p handle))
           (setf *open-process-last-log* nil)
           (make-instance 'live-reader :handle handle :pid pid
                                       :window-title (window-title hwnd)))
          (t
           ;; GetLastError first, before WINDOW-TITLE or anything else
           ;; clobbers it.
           (maybe-log-open-failure pid (%get-last-error))
           nil))))

(defun close-reader (reader)
  (when (live-reader-buffer reader)
    (fli:free-foreign-object (live-reader-buffer reader))
    (setf (live-reader-buffer reader) nil
          (live-reader-buffer-size reader) 0))
  (%close-handle (live-reader-handle reader)))

;; CHOOSE-PSOBB-READER and OPEN-PSOBB-READER are defined at the end of
;; this file, after the Authenticode helpers, so candidate selection can
;; verify each window's signature (a file check, not a memory read) before
;; any of them is read from.

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
      (cond
        ((and ok (= bytes-read size))
         (let ((result (make-array size :element-type '(unsigned-byte 8))))
           ;; Bulk copy. The poll loop moves tens of KB through here 30x
           ;; a second (player blocks, monster blocks, quest registers);
           ;; a per-byte FLI:DEREFERENCE loop made that a measurable,
           ;; constant CPU tax next to a live game.
           (fli:replace-foreign-array result buffer :end2 size)
           result))
        (t
         ;; Keep the cause for the poll loop to log if reads stay dead.
         ;; Captured here, before any other FFI call clobbers GetLastError
         ;; (nothing runs between the read and this point). A single failed
         ;; read is NOT itself a "cannot read" verdict - many reads chase
         ;; pointers that are legitimately null mid-transition - so the
         ;; per-frame decision lives in the poll loop, not here.
         (setf (live-reader-last-read-error reader) (%get-last-error))
         nil)))))

;;; Authenticode verification (wintrust/crypt32). PROCESS-IMAGE-PATH
;;; finds the exe behind a process handle and AUTHENTICODE-VERIFY
;;; checks its embedded signature, so the poll loop can refuse to
;;; record from a PSOBB.exe that is not the signed official Ephinea
;;; client. The accept/reject policy itself is pure CL
;;; (PSOBB-SIGNATURE-TRUSTED-P in psobb.lisp); only the Win32 legwork
;;; lives here.

(fli:register-module :wintrust :real-name "wintrust"
                     :connection-style :automatic)
(fli:register-module :crypt32 :real-name "crypt32"
                     :connection-style :automatic)

(fli:define-foreign-function (%query-full-process-image-name
                              "QueryFullProcessImageNameW")
    ((process :pointer)
     (flags (:unsigned :long))
     (exe-name :pointer)
     (size (:reference (:unsigned :long))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defun process-image-path (reader)
  "Full path of the exe behind READER's process (BMP characters only),
or NIL. PROCESS_QUERY_INFORMATION, already requested at attach time,
is sufficient access."
  (fli:with-dynamic-foreign-objects ()
    (let* ((max-chars 1024)
           (buffer (fli:allocate-dynamic-foreign-object
                    :type '(:unsigned :short) :nelems max-chars)))
      (multiple-value-bind (ok length)
          (%query-full-process-image-name (live-reader-handle reader) 0
                                          buffer max-chars)
        (when (and ok (plusp length) (<= length max-chars))
          (utf16-buffer-string buffer length))))))

(fli:define-c-struct guid
  (data1 (:unsigned :long))
  (data2 (:unsigned :short))
  (data3 (:unsigned :short))
  (data4 (:c-array (:unsigned :byte) 8)))

;; WINTRUST_FILE_INFO / WINTRUST_DATA, transcribed from wintrust.h
;; (64-bit layout; the union member is a single pointer).
(fli:define-c-struct wintrust-file-info
  (cb-struct (:unsigned :long))
  (file-path :pointer)
  (file-handle :pointer)
  (known-subject :pointer))

(fli:define-c-struct wintrust-data
  (cb-struct (:unsigned :long))
  (policy-callback-data :pointer)
  (sip-client-data :pointer)
  (ui-choice (:unsigned :long))
  (revocation-checks (:unsigned :long))
  (union-choice (:unsigned :long))
  (file-info (:pointer wintrust-file-info))
  (state-action (:unsigned :long))
  (state-data :pointer)
  (url-reference :pointer)
  (prov-flags (:unsigned :long))
  (ui-context (:unsigned :long))
  (signature-settings :pointer))

;; Leading fields of CRYPT_PROVIDER_CERT / CRYPT_PROVIDER_SGNR
;; (wintrust.h). Only these prefixes are read, and only from structs
;; wintrust itself allocated, so the trailing members can be omitted.
(fli:define-c-struct crypt-provider-cert-prefix
  (cb-struct (:unsigned :long))
  (cert :pointer))

(fli:define-c-struct crypt-provider-sgnr-prefix
  (cb-struct (:unsigned :long))
  (verify-as-of-low (:unsigned :long))  ; FILETIME
  (verify-as-of-high (:unsigned :long))
  (cs-cert-chain (:unsigned :long))
  (pas-cert-chain (:pointer crypt-provider-cert-prefix)))

;; Returns a LONG, but declared unsigned so HRESULTs compare directly
;; against their #x8... literals.
(fli:define-foreign-function (%win-verify-trust "WinVerifyTrust")
    ((hwnd :pointer)
     (action-id (:pointer guid))
     (data (:pointer wintrust-data)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :wintrust)

(fli:define-foreign-function (%wt-helper-prov-data-from-state-data
                              "WTHelperProvDataFromStateData")
    ((state-data :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :wintrust)

(fli:define-foreign-function (%wt-helper-get-prov-signer-from-chain
                              "WTHelperGetProvSignerFromChain")
    ((prov-data :pointer)
     (idx-signer (:unsigned :long))
     (counter-signer (:boolean :int))
     (idx-counter-signer (:unsigned :long)))
  :result-type (:pointer crypt-provider-sgnr-prefix)
  :calling-convention :stdcall
  :module :wintrust)

(fli:define-foreign-function (%cert-get-name-string "CertGetNameStringW")
    ((cert-context :pointer)
     (name-type (:unsigned :long))
     (flags (:unsigned :long))
     (type-para :pointer)
     (name-string :pointer)
     (cch-name-string (:unsigned :long)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :crypt32)

(defconstant +wtd-ui-none+ 2)
(defconstant +wtd-choice-file+ 1)
(defconstant +wtd-stateaction-verify+ 1)
(defconstant +wtd-stateaction-close+ 2)
(defconstant +wtd-cache-only-url-retrieval+ #x1000)
(defconstant +trust-e-nosignature+ #x800B0100)
(defconstant +cert-name-simple-display-type+ 4)

(defun wintrust-action-guid (action)
  ;; WINTRUST_ACTION_GENERIC_VERIFY_V2 {00AAC56B-CD44-11d0-8CC2-00C04FC295EE}
  (setf (fli:foreign-slot-value action 'data1) #x00AAC56B
        (fli:foreign-slot-value action 'data2) #xCD44
        (fli:foreign-slot-value action 'data3) #x11D0)
  (let ((data4 (fli:foreign-slot-pointer action 'data4)))
    (loop :for i :from 0
          :for byte :in '(#x8C #xC2 #x00 #xC0 #x4F #xC2 #x95 #xEE)
          :do (setf (fli:foreign-aref data4 i) byte)))
  action)

(defun signer-common-name (state-data)
  "Subject CN of the leaf signing certificate, or NIL."
  (let ((prov-data (%wt-helper-prov-data-from-state-data state-data)))
    (unless (fli:null-pointer-p prov-data)
      (let ((signer (%wt-helper-get-prov-signer-from-chain prov-data 0 nil 0)))
        (unless (fli:null-pointer-p signer)
          (when (plusp (fli:foreign-slot-value signer 'cs-cert-chain))
            (let ((cert (fli:foreign-slot-value
                         (fli:foreign-slot-value signer 'pas-cert-chain)
                         'cert)))
              (unless (fli:null-pointer-p cert)
                (fli:with-dynamic-foreign-objects ()
                  (let* ((max-chars 256)
                         (buffer (fli:allocate-dynamic-foreign-object
                                  :type '(:unsigned :short) :nelems max-chars))
                         ;; Returned length includes the trailing NUL.
                         (length (%cert-get-name-string
                                  cert +cert-name-simple-display-type+ 0
                                  fli:*null-pointer* buffer max-chars)))
                    (when (> length 1)
                      (utf16-buffer-string buffer (1- length)))))))))))))

(defun authenticode-verify (path)
  "WinVerifyTrust Authenticode check of the file at PATH.
Returns (values STATUS SIGNER): STATUS is :VALID, :UNSIGNED or
:INVALID; SIGNER is the signing certificate's subject CN when STATUS
is :VALID. A timestamp countersignature keeps an expired certificate
:VALID, which is why the policy pins the signer name and not the
certificate. Revocation servers are never contacted
(+WTD-CACHE-ONLY-URL-RETRIEVAL+), so this cannot block on the network."
  (fli:with-dynamic-foreign-objects ((action guid)
                                     (file-info wintrust-file-info)
                                     (data wintrust-data))
    (wintrust-action-guid action)
    (fli:with-foreign-string (path-ptr element-count byte-count
                              :external-format :unicode)
        path
      (declare (ignore element-count byte-count))
      (setf (fli:foreign-slot-value file-info 'cb-struct)
            (fli:size-of 'wintrust-file-info)
            (fli:foreign-slot-value file-info 'file-path) path-ptr
            (fli:foreign-slot-value file-info 'file-handle) fli:*null-pointer*
            (fli:foreign-slot-value file-info 'known-subject) fli:*null-pointer*)
      (setf (fli:foreign-slot-value data 'cb-struct)
            (fli:size-of 'wintrust-data)
            (fli:foreign-slot-value data 'policy-callback-data)
            fli:*null-pointer*
            (fli:foreign-slot-value data 'sip-client-data) fli:*null-pointer*
            (fli:foreign-slot-value data 'ui-choice) +wtd-ui-none+
            (fli:foreign-slot-value data 'revocation-checks) 0
            (fli:foreign-slot-value data 'union-choice) +wtd-choice-file+
            (fli:foreign-slot-value data 'file-info) file-info
            (fli:foreign-slot-value data 'state-action)
            +wtd-stateaction-verify+
            (fli:foreign-slot-value data 'state-data) fli:*null-pointer*
            (fli:foreign-slot-value data 'url-reference) fli:*null-pointer*
            (fli:foreign-slot-value data 'prov-flags)
            +wtd-cache-only-url-retrieval+
            (fli:foreign-slot-value data 'ui-context) 0
            (fli:foreign-slot-value data 'signature-settings)
            fli:*null-pointer*)
      (let ((result (%win-verify-trust fli:*null-pointer* action data)))
        (unwind-protect
             (cond ((zerop result)
                    (values :valid (signer-common-name
                                    (fli:foreign-slot-value data 'state-data))))
                   ((eql result +trust-e-nosignature+)
                    (values :unsigned nil))
                   (t (values :invalid nil)))
          ;; Release wintrust's verification state in all cases.
          (setf (fli:foreign-slot-value data 'state-action)
                +wtd-stateaction-close+)
          (%win-verify-trust fli:*null-pointer* action data))))))

;;; Candidate selection (2-window play). Defined here, after the
;;; Authenticode helpers, so a candidate's signature - a check of the exe
;;; FILE on disk, never its process memory - is verified before any of the
;;; candidates is read from, keeping the verify-before-read invariant the
;;; single-window path has always had.

(defun reader-signature-trusted-p (reader)
  "T when READER's exe is the signed official Ephinea client. File-based
Authenticode only; reads no process memory, so it is safe to call on an
unverified candidate before deciding whether to read its memory.
PSOBB-SIGNATURE-TRUSTED-P is FUNCALLed - it lives in psobb.lisp, which
loads after this file."
  (let ((path (process-image-path reader)))
    (multiple-value-bind (status signer)
        (if path (authenticode-verify path) (values :invalid nil))
      (and (funcall 'psobb-signature-trusted-p status signer) t))))

(defun choose-psobb-reader (readers)
  "Pick which candidate READER to attach and close the rest. A single
candidate is returned untouched - the poll loop trust-checks and reads it
exactly as before. With several windows (2-window play) a signed official
client always wins over an untrusted title-squatter, and only trusted
candidates ever have their memory read (READER-QUEST-LOADED-P), so an
untrusted window can neither be probed nor shadow the real client. Among
trusted candidates the one already running a quest wins, so a shop or
lobby second window does not hide the instance being played. When none is
trusted the first untrusted reader is returned unread, so the poll loop's
own trust check can still report it as \"not official\"."
  (when readers
    (if (null (rest readers))
        (first readers)
        (let ((trusted '()) (untrusted '()))
          (dolist (r readers)
            (if (reader-signature-trusted-p r)
                (push r trusted)
                (push r untrusted)))
          (setf trusted (nreverse trusted))
          (let ((chosen
                  (cond
                    ((null trusted) (first (nreverse untrusted)))
                    ((null (rest trusted)) (first trusted))
                    (t (or (find-if (lambda (r)
                                      (funcall 'reader-quest-loaded-p r))
                                    trusted)
                           (first trusted))))))
            (dolist (r readers)
              (unless (eq r chosen) (close-reader r)))
            (win32-log "selected PSOBB pid ~d of ~d windows (~d trusted)"
                       (live-reader-pid chosen) (length readers)
                       (length trusted))
            chosen)))))

(defun open-psobb-reader ()
  "Attach to a running PSOBB process; NIL when none. With several PSOBB
windows open (2-window play), a trusted instance running a quest wins."
  (let ((candidates (find-psobb-windows)))
    (when candidates
      (when (rest candidates)
        (win32-log "multiple PSOBB windows: ~d (pids ~{~d~^, ~})"
                   (length candidates) (mapcar #'cdr candidates)))
      (let ((readers (loop :for pair :in candidates
                           :for r := (open-reader-for (car pair) (cdr pair))
                           :when r :collect r)))
        (choose-psobb-reader readers)))))
