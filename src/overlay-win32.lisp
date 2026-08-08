(in-package :ephinea-ta-client)

;;; LispWorks-only: a small floating in-game overlay (timer + ghost gap)
;;; drawn over the PSOBB window while a quest runs. Same architecture as
;;; the tray (tray-win32.lisp, which loads first and owns the shared
;;; window-class/message-loop bindings): a window with our own procedure
;;; on a dedicated thread. PSOBB itself is never touched - this is an
;;; ordinary topmost layered window that happens to sit over the game,
;;; consistent with the read-only-access policy.
;;;
;;; WS_EX_TRANSPARENT makes it click-through and WS_EX_NOACTIVATE keeps
;;; focus on the game; SetLayeredWindowAttributes blends the whole
;;; window at a fixed alpha. The poll loop only writes the shared text
;;; state (OVERLAY-SHOW! / OVERLAY-HIDE!); a 4 Hz timer on the overlay
;;; thread repositions the window against the game window and repaints.

;;; --- Win32 bindings (user32 bits not yet bound + gdi32) -------------

(fli:register-module :gdi32 :real-name "gdi32" :connection-style :automatic)

(fli:define-foreign-function (%set-layered-window-attributes
                              "SetLayeredWindowAttributes")
    ((hwnd :pointer)
     (color (:unsigned :long))
     (alpha (:unsigned :byte))
     (flags (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%show-window "ShowWindow")
    ((hwnd :pointer)
     (cmd :int))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

;; The WIN-RECT struct and %GET-WINDOW-RECT / %GET-CLIENT-RECT already
;; live in ffmpeg-win32.lisp (loaded earlier); this file reuses them
;; (WINDOW-RECT-OF for the game window's placement).

;; hWndInsertAfter is unused (SWP_NOZORDER) so :size-t 0 is fine.
(fli:define-foreign-function (%set-window-pos "SetWindowPos")
    ((hwnd :pointer)
     (insert-after :size-t)
     (x :int) (y :int) (cx :int) (cy :int)
     (flags (:unsigned :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%invalidate-rect "InvalidateRect")
    ((hwnd :pointer)
     (rect :pointer)
     (erase (:boolean :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-c-struct win-paintstruct
  (hdc :pointer)
  (erase (:boolean :int))
  (paint-left :int)
  (paint-top :int)
  (paint-right :int)
  (paint-bottom :int)
  (restore (:boolean :int))
  (inc-update (:boolean :int))
  (reserved (:c-array (:unsigned :byte) 32)))

(fli:define-foreign-function (%begin-paint "BeginPaint")
    ((hwnd :pointer)
     (ps (:pointer win-paintstruct)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%end-paint "EndPaint")
    ((hwnd :pointer)
     (ps (:pointer win-paintstruct)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%fill-rect "FillRect")
    ((hdc :pointer)
     (rect (:pointer win-rect))
     (brush :pointer))
  :result-type :int
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%set-timer "SetTimer")
    ((hwnd :pointer)
     (id :size-t)
     (elapse (:unsigned :int))
     (proc :pointer))
  :result-type :size-t
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%kill-timer "KillTimer")
    ((hwnd :pointer)
     (id :size-t))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%create-solid-brush "CreateSolidBrush")
    ((color (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%delete-object "DeleteObject")
    ((object :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%select-object "SelectObject")
    ((hdc :pointer)
     (object :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%set-text-color "SetTextColor")
    ((hdc :pointer)
     (color (:unsigned :long)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%set-bk-mode "SetBkMode")
    ((hdc :pointer)
     (mode :int))
  :result-type :int
  :calling-convention :stdcall
  :module :gdi32)

;; :ef-wc-string with :unicode so a Japanese quest name can never make
;; the ASCII converter signal mid-paint (the tray menu's lesson).
(fli:define-foreign-function (%text-out "TextOutW")
    ((hdc :pointer)
     (x :int) (y :int)
     (text (:reference-pass (:ef-wc-string :external-format :unicode)))
     (count :int))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%create-font "CreateFontW")
    ((height :int) (width :int) (escapement :int) (orientation :int)
     (weight :int)
     (italic (:unsigned :long))
     (underline (:unsigned :long))
     (strikeout (:unsigned :long))
     (charset (:unsigned :long))
     (out-precision (:unsigned :long))
     (clip-precision (:unsigned :long))
     (quality (:unsigned :long))
     (pitch-and-family (:unsigned :long))
     (face (:reference-pass :ef-wc-string)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :gdi32)

;;; --- Constants ------------------------------------------------------

(defconstant +ws-popup+ #x80000000)
(defconstant +ws-ex-topmost+ #x8)
(defconstant +ws-ex-transparent+ #x20)
(defconstant +ws-ex-toolwindow+ #x80)
(defconstant +ws-ex-layered+ #x80000)
(defconstant +ws-ex-noactivate+ #x08000000)
(defconstant +lwa-alpha+ 2)
(defconstant +sw-hide+ 0)
(defconstant +sw-shownoactivate+ 4)
(defconstant +swp-nosize+ #x1)
(defconstant +swp-nozorder+ #x4)
(defconstant +swp-noactivate+ #x10)
(defconstant +wm-paint+ #x000F)
(defconstant +wm-timer+ #x0113)
(defconstant +bk-transparent+ 1)
(defconstant +fw-semibold+ 600)
(defconstant +default-charset+ 1)
(defconstant +cleartype-quality+ 5)

(defconstant +overlay-timer-id+ 1)
(defconstant +overlay-timer-ms+ 250)

(defparameter +overlay-width+ 250)
(defparameter +overlay-height+ 62)
(defparameter +overlay-margin-x+ 24
  "Gap from the game window's right edge.")
(defparameter +overlay-margin-y+ 48
  "Below the game window's title bar.")
(defparameter +overlay-alpha+ 215)

;; COLORREF is #x00BBGGRR.
(defparameter +overlay-bg-color+ #x201410
  "Near-black blue-tinted background.")
(defparameter +overlay-text-color+ #xF0F0F0)
(defparameter +overlay-ahead-color+ #x78DC50
  "Green: ahead of the ghost.")
(defparameter +overlay-behind-color+ #x6E6EFF
  "Red: behind the ghost.")

(defparameter +overlay-class-name+ "RappyRunsOverlay")

;;; --- State ----------------------------------------------------------

(defvar *overlay-hwnd* nil)
(defvar *overlay-process* nil)
(defvar *overlay-class-registered* nil)
(defvar *overlay-font* nil)
(defvar *overlay-brush* nil
  "The background brush, created once per overlay thread - a 4 Hz
paint must not churn GDI objects.")

(defvar *overlay-wanted* nil
  "T while the poll loop wants the overlay visible. The overlay thread's
timer reads it and shows/hides accordingly; writers never touch the
window directly (a window belongs to its creating thread).")
(defvar *overlay-visible* nil)
(defvar *overlay-line1* ""
  "Top line: the live clock (plus REC).")
(defvar *overlay-line2* nil
  "Bottom line: 'vs <ghost time> <gap>', or NIL without a ghost.")
(defvar *overlay-delta-state* :neutral
  ":ahead / :behind / :neutral - colors the bottom line.")

;;; --- Painting and geometry (overlay thread only) --------------------

(defun overlay-position (hwnd game-hwnd)
  "Pin the overlay to the game window's top-right corner."
  (let ((rect (window-rect-of game-hwnd)))
    (when rect
      (destructuring-bind (left top right bottom) rect
        (declare (ignore left bottom))
        (%set-window-pos hwnd 0
                         (- right +overlay-width+ +overlay-margin-x+)
                         (+ top +overlay-margin-y+)
                         +overlay-width+ +overlay-height+
                         (logior +swp-nozorder+ +swp-noactivate+))))))

(defun overlay-paint (hwnd)
  "WM_PAINT: dark pill, clock on top, ghost line below in its gap
color. BeginPaint/EndPaint must run even when drawing errors out, or
the invalid region never clears and WM_PAINT storms."
  (fli:with-dynamic-foreign-objects ()
    (let* ((ps (fli:allocate-dynamic-foreign-object :type 'win-paintstruct))
           (hdc (%begin-paint hwnd ps)))
      (unwind-protect
           (unless (fli:null-pointer-p hdc)
             (let ((rect (fli:allocate-dynamic-foreign-object
                          :type 'win-rect)))
               ;; The window never resizes, so the client rect is just
               ;; the fixed dimensions.
               (setf (fli:foreign-slot-value rect 'left) 0
                     (fli:foreign-slot-value rect 'top) 0
                     (fli:foreign-slot-value rect 'right) +overlay-width+
                     (fli:foreign-slot-value rect 'bottom) +overlay-height+)
               (when *overlay-brush*
                 (%fill-rect hdc rect *overlay-brush*))
               (when *overlay-font*
                 (%select-object hdc *overlay-font*))
               (%set-bk-mode hdc +bk-transparent+)
               (%set-text-color hdc +overlay-text-color+)
               (let ((line1 *overlay-line1*))
                 (%text-out hdc 12 6 line1 (length line1)))
               (let ((line2 *overlay-line2*))
                 (when line2
                   (%set-text-color
                    hdc (ecase *overlay-delta-state*
                          (:ahead +overlay-ahead-color+)
                          (:behind +overlay-behind-color+)
                          (:neutral +overlay-text-color+)))
                   (%text-out hdc 12 32 line2 (length line2))))))
        (%end-paint hwnd ps)))))

(defun overlay-conceal (hwnd)
  (when *overlay-visible*
    (%show-window hwnd +sw-hide+)
    (setf *overlay-visible* nil)))

(defun overlay-timer-tick (hwnd)
  "4 Hz: follow the game window and repaint while wanted, hide
otherwise (also when the game window is gone mid-quest)."
  (let ((game (and *overlay-wanted* (find-psobb-window))))
    (cond
      (game
       (overlay-position hwnd game)
       (unless *overlay-visible*
         (%show-window hwnd +sw-shownoactivate+)
         (setf *overlay-visible* t))
       (%invalidate-rect hwnd fli:*null-pointer* nil))
      (t (overlay-conceal hwnd)))))

(fli:define-foreign-callable
    ("RappyOverlayWndProc" :result-type :size-t :calling-convention :stdcall)
    ((hwnd :pointer)
     (msg (:unsigned :int))
     (wparam :size-t)
     (lparam :size-t))
  (cond
    ((= msg +wm-timer+)
     (ignore-errors (overlay-timer-tick hwnd))
     0)
    ((= msg +wm-paint+)
     (ignore-errors (overlay-paint hwnd))
     0)
    ((= msg +wm-destroy+)
     (ignore-errors (%kill-timer hwnd +overlay-timer-id+))
     (%post-quit-message 0)
     0)
    (t (%def-window-proc hwnd msg wparam lparam))))

;;; --- Lifecycle ------------------------------------------------------

(defun overlay-register-class ()
  (unless *overlay-class-registered*
    (fli:with-dynamic-foreign-objects ()
      (let ((wc (fli:allocate-dynamic-foreign-object :type 'wnd-class-ex)))
        (fli:with-foreign-string (name-ptr elts bytes :external-format :unicode)
            +overlay-class-name+
          (declare (ignore elts bytes))
          (setf (fli:foreign-slot-value wc 'cb-size) (fli:size-of 'wnd-class-ex)
                (fli:foreign-slot-value wc 'style) 0
                (fli:foreign-slot-value wc 'wnd-proc)
                (fli:make-pointer :symbol-name "RappyOverlayWndProc")
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
    (setf *overlay-class-registered* t)))

(defun overlay-thread-main ()
  "Create the overlay window on THIS thread and pump its messages until
WM_QUIT. Created hidden; the timer shows it once wanted."
  (handler-case
      (progn
        (overlay-register-class)
        (let ((hwnd (%create-window-ex
                     (logior +ws-ex-layered+ +ws-ex-topmost+
                             +ws-ex-transparent+ +ws-ex-toolwindow+
                             +ws-ex-noactivate+)
                     +overlay-class-name+ "RappyRunsOverlay"
                     +ws-popup+
                     0 0 +overlay-width+ +overlay-height+
                     fli:*null-pointer* fli:*null-pointer*
                     (%get-module-handle fli:*null-pointer*)
                     fli:*null-pointer*)))
          (when (fli:null-pointer-p hwnd)
            (return-from overlay-thread-main))
          (setf *overlay-hwnd* hwnd
                *overlay-visible* nil)
          (%set-layered-window-attributes hwnd 0 +overlay-alpha+ +lwa-alpha+)
          (setf *overlay-font*
                (%create-font 20 0 0 0 +fw-semibold+ 0 0 0
                              +default-charset+ 0 0 +cleartype-quality+ 0
                              "Segoe UI")
                *overlay-brush* (%create-solid-brush +overlay-bg-color+))
          (%set-timer hwnd +overlay-timer-id+ +overlay-timer-ms+
                      fli:*null-pointer*)
          (fli:with-dynamic-foreign-objects ()
            (let ((msg (fli:allocate-dynamic-foreign-object :type 'win-msg)))
              (loop
                (let ((r (%get-message msg fli:*null-pointer* 0 0)))
                  (when (<= r 0) (return))
                  (%translate-message msg)
                  (%dispatch-message msg)))))))
    (error (e)
      (ignore-errors (format t "~&; overlay thread error: ~a~%" e))))
  (when *overlay-font*
    (ignore-errors (%delete-object *overlay-font*))
    (setf *overlay-font* nil))
  (when *overlay-brush*
    (ignore-errors (%delete-object *overlay-brush*))
    (setf *overlay-brush* nil))
  (setf *overlay-hwnd* nil
        *overlay-process* nil
        *overlay-visible* nil))

;;; --- Poll-loop API --------------------------------------------------

(defun overlay-show! (line1 line2 delta-state)
  "Want the overlay visible with this content. Starts the overlay
thread lazily; best-effort - a failure here must never touch the poll
loop."
  (setf *overlay-line1* (or line1 "")
        *overlay-line2* line2
        *overlay-delta-state* (or delta-state :neutral)
        *overlay-wanted* t)
  (unless (and *overlay-process* (mp:process-alive-p *overlay-process*))
    (ignore-errors
      (setf *overlay-process*
            (mp:process-run-function "eta-client-overlay" '()
                                     'overlay-thread-main)))))

(defun overlay-hide! ()
  "Want the overlay gone; the overlay thread's next tick hides it. The
thread itself stays up - an idle 4 Hz timer costs nothing and the next
quest reuses it."
  (setf *overlay-wanted* nil))

(defun update-ghost-overlay (detector recording-p)
  "Reflect the quest state into the overlay: timer (+ ghost gap when
racing) during a quest, hidden otherwise or when the setting is off.
Called from UPDATE-GAME-STATUS at its 4 Hz cadence."
  (if (and (config-value :ghost-overlay)
           (eq (detector-state detector) :in-quest))
      (let* ((race *ghost-race*)
             (delta (and race (ghost-race-delta-ms race))))
        (overlay-show!
         (format nil "~a~:[~; REC~]"
                 (format-run-time (or (detector-elapsed-ms detector) 0))
                 recording-p)
         (and race (ghost-vs-text race))
         (cond ((null delta) :neutral)
               ((minusp delta) :ahead)
               (t :behind))))
      (overlay-hide!)))
