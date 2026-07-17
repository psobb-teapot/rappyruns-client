(in-package :ephinea-ta-client)

;;; LispWorks-only: a system-tray (notification area) icon for the client.
;;;
;;; CAPI has no tray support, so we own the icon ourselves: a hidden
;;; message window with our own window procedure (a foreign callable),
;;; running its GetMessage loop on a dedicated thread. Shell_NotifyIcon
;;; posts our callback message on mouse events; a left double-click
;;; restores the main window, a right-click pops the Show / Quit menu.
;;;
;;; Everything CAPI-side (show, raise, quit) is marshalled onto the
;;; interface's process with EXECUTE-WITH-INTERFACE-IF-ALIVE - the tray
;;; runs on its own thread and must never touch a live interface
;;; directly. *INTERFACE* is read at call time (the language toggle
;;; swaps the window in REBUILD-INTERFACE).

;;; --- Win32 bindings -------------------------------------------------

;; user32/kernel32/shell32 are already registered in win32.lisp, which
;; loads first.

(fli:define-foreign-function (%get-module-handle "GetModuleHandleW")
    ((module-name :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%def-window-proc "DefWindowProcW")
    ((hwnd :pointer)
     (msg (:unsigned :int))
     (wparam :size-t)
     (lparam :size-t))
  :result-type :size-t
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%post-quit-message "PostQuitMessage")
    ((exit-code :int))
  :result-type :void
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%post-message "PostMessageW")
    ((hwnd :pointer)
     (msg (:unsigned :int))
     (wparam :size-t)
     (lparam :size-t))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%register-window-message "RegisterWindowMessageW")
    ((name (:reference-pass :ef-wc-string)))
  :result-type (:unsigned :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%set-foreground-window "SetForegroundWindow")
    ((hwnd :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%destroy-window "DestroyWindow")
    ((hwnd :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

;; Single-instance guard: a named mutex whose creation reports
;; ERROR_ALREADY_EXISTS when another instance already holds it, and
;; FindWindow-by-class so the second instance can raise the first.
(fli:define-foreign-function (%create-mutex "CreateMutexW")
    ((attributes :pointer)
     (initial-owner (:boolean :int))
     (name (:reference-pass :ef-wc-string)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%get-last-error "GetLastError")
    ()
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%find-window-class "FindWindowW")
    ((class-name (:reference-pass :ef-wc-string))
     (window-name :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

;; The last-resort terminator: guarantees Quit actually quits even if a
;; clean LW:QUIT would hang on the message-loop or poll thread.
(fli:define-foreign-function (%exit-process "ExitProcess")
    ((exit-code (:unsigned :int)))
  :result-type :void
  :calling-convention :stdcall
  :module :kernel32)

;; The exe's own embedded icon (index 0), used for the tray. NIL exe
;; (dev/SBCL) or extraction failure falls back to LoadIconW below.
;; :ef-wc-string without an explicit external format converts through
;; FLI:ASCII-WCHAR, which SIGNALS on any non-ASCII character - the exe
;; may live under a Japanese-named directory, so spell out :unicode.
(fli:define-foreign-function (%extract-icon "ExtractIconW")
    ((hinst :pointer)
     (exe-file (:reference-pass (:ef-wc-string :external-format :unicode)))
     (index (:unsigned :int)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :shell32)

(fli:define-foreign-function (%load-icon "LoadIconW")
    ((hinst :pointer)
     (icon-name :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

;; Menu (right-click Show / Quit).
(fli:define-foreign-function (%create-popup-menu "CreatePopupMenu")
    ()
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

;; The item text is localized ("表示" / "終了" in Japanese), and the
;; default :ef-wc-string external format (FLI:ASCII-WCHAR) signals on
;; non-ASCII characters. The window proc's IGNORE-ERRORS then swallowed
;; that error, so right-clicking the tray icon showed no menu at all on
;; Japanese UI - :unicode makes the conversion handle the full BMP.
(fli:define-foreign-function (%append-menu "AppendMenuW")
    ((menu :pointer)
     (flags (:unsigned :int))
     (id-new-item :size-t)              ; UINT_PTR
     (new-item (:reference-pass (:ef-wc-string :external-format :unicode))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%track-popup-menu "TrackPopupMenu")
    ((menu :pointer)
     (flags (:unsigned :int))
     (x :int) (y :int)
     (reserved :int)
     (hwnd :pointer)
     (rect :pointer))
  :result-type :int
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%destroy-menu "DestroyMenu")
    ((menu :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-c-struct win-point
  (x :int)
  (y :int))

(fli:define-foreign-function (%get-cursor-pos "GetCursorPos")
    ((point (:pointer win-point)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

;; WNDCLASSEXW. lpfnWndProc holds our foreign-callable pointer.
(fli:define-c-struct wnd-class-ex
  (cb-size (:unsigned :int))
  (style (:unsigned :int))
  (wnd-proc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance :pointer)
  (icon :pointer)
  (cursor :pointer)
  (background :pointer)
  (menu-name :pointer)
  (class-name :pointer)
  (icon-sm :pointer))

(fli:define-foreign-function (%register-class-ex "RegisterClassExW")
    ((wc (:pointer wnd-class-ex)))
  :result-type (:unsigned :short)       ; ATOM
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%create-window-ex "CreateWindowExW")
    ((ex-style (:unsigned :long))
     (class-name (:reference-pass :ef-wc-string))
     (window-name (:reference-pass :ef-wc-string))
     (style (:unsigned :long))
     (x :int) (y :int) (width :int) (height :int)
     (parent :pointer)
     (menu :pointer)
     (instance :pointer)
     (param :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

;; MSG. We never read its fields ourselves (Translate/Dispatch do), so
;; only the total size has to match; the members are laid out for that.
(fli:define-c-struct win-msg
  (hwnd :pointer)
  (message (:unsigned :int))
  (wparam :size-t)
  (lparam :size-t)
  (time (:unsigned :long))
  (pt-x :int)
  (pt-y :int))

(fli:define-foreign-function (%get-message "GetMessageW")
    ((msg (:pointer win-msg))
     (hwnd :pointer)
     (msg-filter-min (:unsigned :int))
     (msg-filter-max (:unsigned :int)))
  :result-type :int                     ; BOOL, -1 on error
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%translate-message "TranslateMessage")
    ((msg (:pointer win-msg)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%dispatch-message "DispatchMessageW")
    ((msg (:pointer win-msg)))
  :result-type :size-t
  :calling-convention :stdcall
  :module :user32)

;; NOTIFYICONDATAW. Given the uFlags we set (MESSAGE|ICON|TIP) the API
;; reads only hWnd, uID, uCallbackMessage, hIcon and szTip, so the
;; trailing state/info/guid members are left untouched. cbSize is the
;; full struct size (fli:size-of) which is what Vista+ expects.
(fli:define-c-struct notify-icon-data
  (cb-size (:unsigned :long))
  (hwnd :pointer)
  (id (:unsigned :int))
  (flags (:unsigned :int))
  (callback-message (:unsigned :int))
  (icon :pointer)
  (tip (:c-array (:unsigned :short) 128))
  (state (:unsigned :long))
  (state-mask (:unsigned :long))
  (info (:c-array (:unsigned :short) 256))
  (timeout-or-version (:unsigned :int))
  (info-title (:c-array (:unsigned :short) 64))
  (info-flags (:unsigned :long))
  (guid-item guid)
  (balloon-icon :pointer))

(fli:define-foreign-function (%shell-notify-icon "Shell_NotifyIconW")
    ((message (:unsigned :long))
     (data (:pointer notify-icon-data)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :shell32)

;;; --- Constants ------------------------------------------------------

(defconstant +wm-null+ #x0000)
(defconstant +wm-destroy+ #x0002)
(defconstant +wm-close+ #x0010)
(defconstant +wm-rbuttonup+ #x0205)
(defconstant +wm-lbuttondblclk+ #x0203)
(defconstant +wm-contextmenu+ #x007b)
(defconstant +wm-app+ #x8000)

;; Our Shell_NotifyIcon callback message and icon id.
(defconstant +tray-callback-message+ (+ +wm-app+ 1))
(defconstant +tray-icon-id+ 1)

;; Posted by a second instance (single-instance guard) to ask the
;; running one to un-hide its window instead of starting another copy.
(defconstant +tray-show-request+ (+ +wm-app+ 2))

(defconstant +error-already-exists+ 183)

(defconstant +nim-add+ 0)
(defconstant +nim-delete+ 2)
(defconstant +nif-message+ #x01)
(defconstant +nif-icon+ #x02)
(defconstant +nif-tip+ #x04)

(defconstant +mf-string+ 0)
(defconstant +tpm-leftalign+ 0)
(defconstant +tpm-rightbutton+ #x0002)
(defconstant +tpm-returncmd+ #x0100)

(defconstant +idi-application+ 32512)

;; Popup menu command ids.
(defconstant +tray-menu-show+ 1)
(defconstant +tray-menu-quit+ 2)

(defparameter +tray-class-name+ "RappyRunsTrayWindow")

(defparameter +singleton-mutex-name+ "RappyRunsClient-single-instance")

;;; --- State ----------------------------------------------------------

(defvar *singleton-mutex* nil
  "Handle of the single-instance mutex, kept for the life of the process
so the mutex is held (closing it would release the instance lock).")

(defvar *tray-hwnd* nil
  "HWND of the hidden tray-owner window, or NIL when the tray is down.")

(defvar *tray-process* nil
  "The thread running the tray window's message loop.")

(defvar *tray-class-registered* nil
  "T once the window class has been registered (once per image).")

(defvar *taskbar-created-message* 0
  "The RegisterWindowMessage(\"TaskbarCreated\") id, so the icon is
re-added when Explorer restarts.")

;;; --- CAPI-side actions (marshalled onto the interface process) ------

(defun tray-show-main-window ()
  "Un-hide and raise the main window from the tray thread."
  (let ((interface *interface*))
    (when interface
      (capi:execute-with-interface-if-alive
       interface
       (lambda ()
         (setf (capi:top-level-interface-display-state interface) :normal)
         (capi:raise-interface interface))))))

(defun tray-quit ()
  "Quit the whole app from the tray menu (defined in main.lisp)."
  (funcall 'quit-app))

;;; --- Window procedure -----------------------------------------------

(defun tray-load-icon ()
  "The exe's own icon, or the default application icon as a fallback."
  (let* ((exe (ignore-errors (first (uiop:raw-command-line-arguments))))
         (icon (and exe
                    (ignore-errors
                      (%extract-icon (%get-module-handle fli:*null-pointer*)
                                     exe 0)))))
    (if (and icon (not (fli:null-pointer-p icon)))
        icon
        (%load-icon fli:*null-pointer*
                    (fli:make-pointer :address +idi-application+ :type :void)))))

(defun tray-add-icon (hwnd)
  "Register (or re-register) the notification-area icon on HWND."
  (fli:with-dynamic-foreign-objects ()
    (let ((nid (fli:allocate-dynamic-foreign-object :type 'notify-icon-data))
          (tip (tr :tray-tooltip)))
      (setf (fli:foreign-slot-value nid 'cb-size) (fli:size-of 'notify-icon-data)
            (fli:foreign-slot-value nid 'hwnd) hwnd
            (fli:foreign-slot-value nid 'id) +tray-icon-id+
            (fli:foreign-slot-value nid 'flags)
            (logior +nif-message+ +nif-icon+ +nif-tip+)
            (fli:foreign-slot-value nid 'callback-message) +tray-callback-message+
            (fli:foreign-slot-value nid 'icon) (tray-load-icon))
      ;; szTip is a fixed WCHAR[128]; copy the tooltip and NUL-terminate.
      (let ((tip-ptr (fli:foreign-slot-pointer nid 'tip))
            (n (min (length tip) 127)))
        (dotimes (i n)
          (setf (fli:foreign-aref tip-ptr i) (char-code (char tip i))))
        (setf (fli:foreign-aref tip-ptr n) 0))
      (%shell-notify-icon +nim-add+ nid))))

(defun tray-remove-icon (hwnd)
  "Delete the notification-area icon (only cbSize/hWnd/uID are read)."
  (fli:with-dynamic-foreign-objects ()
    (let ((nid (fli:allocate-dynamic-foreign-object :type 'notify-icon-data)))
      (setf (fli:foreign-slot-value nid 'cb-size) (fli:size-of 'notify-icon-data)
            (fli:foreign-slot-value nid 'hwnd) hwnd
            (fli:foreign-slot-value nid 'id) +tray-icon-id+)
      (%shell-notify-icon +nim-delete+ nid))))

(defun tray-popup-menu (hwnd)
  "Right-click menu: Show / Quit. TPM_RETURNCMD makes TrackPopupMenu
return the chosen id; the SetForegroundWindow + WM_NULL dance is the
documented way to keep a tray menu from sticking open."
  (let ((menu (%create-popup-menu)))
    (unless (fli:null-pointer-p menu)
      (unwind-protect
           (fli:with-dynamic-foreign-objects ()
             (%append-menu menu +mf-string+ +tray-menu-show+ (tr :tray-show))
             (%append-menu menu +mf-string+ +tray-menu-quit+ (tr :tray-quit))
             (%set-foreground-window hwnd)
             (let ((pt (fli:allocate-dynamic-foreign-object :type 'win-point))
                   (x 0) (y 0))
               (when (%get-cursor-pos pt)
                 (setf x (fli:foreign-slot-value pt 'x)
                       y (fli:foreign-slot-value pt 'y)))
               (let ((cmd (%track-popup-menu
                           menu
                           (logior +tpm-leftalign+ +tpm-rightbutton+
                                   +tpm-returncmd+)
                           x y 0 hwnd fli:*null-pointer*)))
                 (%post-message hwnd +wm-null+ 0 0)
                 (cond ((= cmd +tray-menu-show+) (tray-show-main-window))
                       ((= cmd +tray-menu-quit+) (tray-quit))))))
        (%destroy-menu menu)))))

(fli:define-foreign-callable
    ("RappyTrayWndProc" :result-type :size-t :calling-convention :stdcall)
    ((hwnd :pointer)
     (msg (:unsigned :int))
     (wparam :size-t)
     (lparam :size-t))
  (cond
    ;; Tray mouse event: the low word of lParam is the mouse message.
    ((= msg +tray-callback-message+)
     (let ((event (logand lparam #xffff)))
       (cond ((= event +wm-lbuttondblclk+)
              (ignore-errors (tray-show-main-window)))
             ((or (= event +wm-rbuttonup+) (= event +wm-contextmenu+))
              (ignore-errors (tray-popup-menu hwnd)))))
     0)
    ;; A second instance asked us to surface (single-instance guard).
    ((= msg +tray-show-request+)
     (ignore-errors (tray-show-main-window))
     0)
    ;; Explorer restarted: re-add the icon.
    ((and (plusp *taskbar-created-message*)
          (= msg *taskbar-created-message*))
     (ignore-errors (tray-add-icon hwnd))
     0)
    ;; STOP-TRAY! posts WM_CLOSE; DefWindowProc turns it into
    ;; DestroyWindow -> WM_DESTROY, where we drop the icon and end the loop.
    ((= msg +wm-destroy+)
     (ignore-errors (tray-remove-icon hwnd))
     (%post-quit-message 0)
     0)
    (t (%def-window-proc hwnd msg wparam lparam))))

;;; --- Lifecycle ------------------------------------------------------

(defun tray-register-class ()
  (unless *tray-class-registered*
    (fli:with-dynamic-foreign-objects ()
      (let ((wc (fli:allocate-dynamic-foreign-object :type 'wnd-class-ex)))
        (fli:with-foreign-string (name-ptr elts bytes :external-format :unicode)
            +tray-class-name+
          (declare (ignore elts bytes))
          (setf (fli:foreign-slot-value wc 'cb-size) (fli:size-of 'wnd-class-ex)
                (fli:foreign-slot-value wc 'style) 0
                (fli:foreign-slot-value wc 'wnd-proc)
                (fli:make-pointer :symbol-name "RappyTrayWndProc")
                (fli:foreign-slot-value wc 'cls-extra) 0
                (fli:foreign-slot-value wc 'wnd-extra) 0
                (fli:foreign-slot-value wc 'instance)
                (%get-module-handle fli:*null-pointer*)
                (fli:foreign-slot-value wc 'icon) fli:*null-pointer*
                (fli:foreign-slot-value wc 'cursor) fli:*null-pointer*
                (fli:foreign-slot-value wc 'background) fli:*null-pointer*
                (fli:foreign-slot-value wc 'menu-name) fli:*null-pointer*
                (fli:foreign-slot-value wc 'class-name) name-ptr
                (fli:foreign-slot-value wc 'icon-sm) fli:*null-pointer*)
          (%register-class-ex wc))))
    (setf *tray-class-registered* t)))

(defun tray-thread-main ()
  "Create the hidden owner window on THIS thread (a window's messages
must be pumped by its creating thread), add the icon, then run the
message loop until WM_QUIT."
  (handler-case
      (progn
        (setf *taskbar-created-message*
              (%register-window-message "TaskbarCreated"))
        (tray-register-class)
        (let ((hwnd (%create-window-ex
                     0 +tray-class-name+ "RappyRunsTray"
                     0 0 0 0 0
                     fli:*null-pointer* fli:*null-pointer*
                     (%get-module-handle fli:*null-pointer*)
                     fli:*null-pointer*)))
          (when (fli:null-pointer-p hwnd)
            (return-from tray-thread-main))
          (setf *tray-hwnd* hwnd)
          (tray-add-icon hwnd)
          ;; Pump messages. GetMessage returns 0 on WM_QUIT, -1 on error.
          (fli:with-dynamic-foreign-objects ()
            (let ((msg (fli:allocate-dynamic-foreign-object :type 'win-msg)))
              (loop
                (let ((r (%get-message msg fli:*null-pointer* 0 0)))
                  (when (<= r 0) (return))
                  (%translate-message msg)
                  (%dispatch-message msg)))))))
    (error (e)
      (ignore-errors (format t "~&; tray thread error: ~a~%" e))))
  (setf *tray-hwnd* nil
        *tray-process* nil))

(defun start-tray! ()
  "Bring up the tray icon on its own thread. Idempotent; best-effort
(a failure here must never take the app down)."
  (unless (and *tray-process* (mp:process-alive-p *tray-process*))
    (ignore-errors
      (setf *tray-process*
            (mp:process-run-function "eta-client-tray" '() 'tray-thread-main)))))

(defun stop-tray! ()
  "Ask the tray thread to remove its icon and end its loop. WM_CLOSE ->
DefWindowProc -> DestroyWindow -> WM_DESTROY (see the window proc)."
  (let ((hwnd *tray-hwnd*))
    (when hwnd
      (ignore-errors (%post-message hwnd +wm-close+ 0 0)))))

(defun tray-remove-icon-now ()
  "Synchronously drop our tray icon (called on quit before ExitProcess,
so no ghost icon lingers in the notification area)."
  (let ((hwnd *tray-hwnd*))
    (when hwnd
      (ignore-errors (tray-remove-icon hwnd)))))

;;; --- Single-instance guard ------------------------------------------

(defun already-running-p ()
  "Create the process-wide singleton mutex; T when another instance
already holds it. The handle is kept in *SINGLETON-MUTEX* for the life
of this process (releasing it would drop the lock)."
  (let ((h (%create-mutex fli:*null-pointer* nil +singleton-mutex-name+)))
    ;; GetLastError must be read immediately after CreateMutex, before any
    ;; other foreign call clobbers it (the SETF below is pure Lisp).
    (prog1 (= (%get-last-error) +error-already-exists+)
      (setf *singleton-mutex* h))))

(defun signal-existing-instance ()
  "Ask the already-running instance to un-hide its window. Best-effort:
it may still be starting up and not yet own its tray window."
  (let ((hwnd (%find-window-class +tray-class-name+ fli:*null-pointer*)))
    (when (and hwnd (not (fli:null-pointer-p hwnd)))
      (ignore-errors (%post-message hwnd +tray-show-request+ 0 0)))))

(defun exit-process-now ()
  "Terminate this process unconditionally (ExitProcess)."
  (%exit-process 0))
