(in-package :ephinea-ta-client)

;;; LispWorks-only: the in-game overlay (timer + ghost gap + room-split
;;; rows + in-world ghost marker) drawn over the PSOBB window while a
;;; quest runs. Same architecture as the tray (tray-win32.lisp, which loads
;;; first and owns the shared window-class/message-loop bindings): a
;;; window with our own procedure on a dedicated thread. PSOBB itself
;;; is never touched - this is an ordinary topmost layered window that
;;; happens to sit over the game, consistent with the read-only-access
;;; policy.
;;;
;;; The window spans the game's whole client area: everything painted
;;; in the key color (+OVERLAY-KEY-COLOR+) is fully transparent via
;;; LWA_COLORKEY, so the game shows through except where the corner
;;; panel and the in-world ghost marker are drawn - those blend at the
;;; fixed LWA_ALPHA. WS_EX_TRANSPARENT makes the whole window
;;; click-through and WS_EX_NOACTIVATE keeps focus on the game. The
;;; poll loop only writes the shared state (OVERLAY-SHOW! /
;;; OVERLAY-HIDE! and ghost.lisp's *LIVE-CAMERA*); a timer on the
;;; overlay thread repositions the window against the game window and
;;; repaints - at 30 Hz while a ghost course is live (the marker must
;;; pan with the camera), 10 Hz for the plain timer pill.

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

(fli:define-foreign-function (%ellipse "Ellipse")
    ((hdc :pointer)
     (left :int) (top :int) (right :int) (bottom :int))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%get-stock-object "GetStockObject")
    ((index :int))
  :result-type :pointer
  :calling-convention :stdcall
  :module :gdi32)

(defconstant +null-pen+ 8)

;; Shifts the logical origin so the corner panel's drawing code can
;; keep painting at (0,0) inside the now full-client-area window.
(fli:define-foreign-function (%set-viewport-org "SetViewportOrgEx")
    ((hdc :pointer)
     (x :int) (y :int)
     (old :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-c-struct win-text-size
  (cx :int)
  (cy :int))

;; Sizes the marker's name pill to its text.
(fli:define-foreign-function (%get-text-extent
                              "GetTextExtentPoint32W")
    ((hdc :pointer)
     (text (:reference-pass (:ef-wc-string :external-format :unicode)))
     (count :int)
     (size (:pointer win-text-size)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

;; Keeps the overlay out of EVERY screen capture (Win10 2004+): the
;; fullscreen recording path duplicates the whole monitor (ddagrab), and
;; a topmost overlay would otherwise be burned into the submitted run
;; footage. Best-effort - on older Windows the call just fails and the
;; overlay stays capturable, as it was in v0.51.0.
(fli:define-foreign-function (%set-window-display-affinity
                              "SetWindowDisplayAffinity")
    ((hwnd :pointer)
     (affinity (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(defconstant +wda-excludefromcapture+ #x11)

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

;;; Ctrl+drag support: key state, mouse capture and the WS_EX_TRANSPARENT
;;; toggle (GWL_EXSTYLE via the LONG_PTR accessors on 64-bit).

(fli:define-foreign-function (%get-async-key-state "GetAsyncKeyState")
    ((vkey :int))
  :result-type (:signed :short)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-foreground-window "GetForegroundWindow")
    ()
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%set-capture "SetCapture")
    ((hwnd :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%release-capture "ReleaseCapture")
    ()
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-window-long-ptr "GetWindowLongPtrW")
    ((hwnd :pointer)
     (index :int))
  :result-type (:signed :long-long)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%set-window-long-ptr "SetWindowLongPtrW")
    ((hwnd :pointer)
     (index :int)
     (value (:signed :long-long)))
  :result-type (:signed :long-long)
  :calling-convention :stdcall
  :module :user32)

;;; --- Constants ------------------------------------------------------

(defconstant +ws-popup+ #x80000000)
(defconstant +ws-ex-topmost+ #x8)
(defconstant +ws-ex-transparent+ #x20)
(defconstant +ws-ex-toolwindow+ #x80)
(defconstant +ws-ex-layered+ #x80000)
(defconstant +ws-ex-noactivate+ #x08000000)
(defconstant +lwa-alpha+ 2)
(defconstant +lwa-colorkey+ 1)
(defconstant +sw-hide+ 0)
(defconstant +sw-shownoactivate+ 4)
(defconstant +swp-nosize+ #x1)
(defconstant +swp-nomove+ #x2)
(defconstant +swp-nozorder+ #x4)
(defconstant +swp-noactivate+ #x10)
(defconstant +swp-framechanged+ #x20)
(defconstant +wm-paint+ #x000F)
(defconstant +wm-timer+ #x0113)
(defconstant +wm-mousemove+ #x0200)
(defconstant +wm-lbuttondown+ #x0201)
(defconstant +wm-lbuttonup+ #x0202)
(defconstant +wm-capturechanged+ #x0215)
(defconstant +wm-nchittest+ #x0084)
(defconstant +ht-client+ 1)
(defconstant +ht-transparent+ #xFFFFFFFFFFFFFFFF
  "HTTRANSPARENT (-1) encoded as the window procedure's unsigned
LRESULT: hit testing skips this window and continues to the one below,
so the click lands on the game.")
(defconstant +gwl-exstyle+ -20)
(defconstant +vk-control+ #x11)
(defconstant +bk-transparent+ 1)
(defconstant +fw-semibold+ 600)
(defconstant +default-charset+ 1)
(defconstant +cleartype-quality+ 5)

(defconstant +overlay-timer-id+ 1)
(defconstant +overlay-timer-ms+ 100
  "10 Hz while no ghost course is live: the compact pill's clock does
not need more.")
(defconstant +overlay-marker-timer-ms+ 33
  "30 Hz while a ghost course is live: the in-world marker is glued to
the world through the camera, and a camera turn at 10 Hz visibly drags
it behind the scenery. The full-window repaint is solid fills, lines
and two text runs - cheap enough for GDI at this rate.")

(defparameter +overlay-width+ 260)
(defparameter +overlay-compact-height+ 62
  "The v0.51.0 timer pill: two text lines, no map. Used whenever there
is no ghost course to draw, so a ghostless run keeps the small pill
instead of a mostly-empty panel.")
(defparameter +overlay-split-top+ 58
  "Where the room-split rows start, under the two header lines.")
(defparameter +overlay-split-row-height+ 20)
(defparameter +overlay-ghost-height+
  (+ +overlay-split-top+
     (* +overlay-split-rows+ +overlay-split-row-height+)
     6)
  "Header (two text lines) + the reserved room-split rows.")
(defparameter +overlay-margin-x+ 24
  "The corner panel's gap from the client area's right edge.")
(defparameter +overlay-margin-y+ 16
  "The corner panel's gap from the client area's top edge (the window
now spans the client area, so no title bar to clear).")
(defparameter +overlay-alpha+ 215)

;; COLORREF is #x00BBGGRR.
(defparameter +overlay-key-color+ #xFF00FF
  "Magenta color key: every pixel painted in this exact color is fully
transparent (LWA_COLORKEY), which is what lets the window span the
whole client area without dimming the game. Never used by any visible
element, and GDI solid fills reproduce it exactly (no dithering /
anti-aliasing on FillRect and Ellipse; text always sits on a solid
panel or pill so ClearType never blends against the key).")
(defparameter +overlay-bg-color+ #x201410
  "Near-black blue-tinted background.")
(defparameter +overlay-text-color+ #xF0F0F0)
(defparameter +overlay-ahead-color+ #x78DC50
  "Green: ahead of the ghost.")
(defparameter +overlay-behind-color+ #x6E6EFF
  "Red: behind the ghost.")
(defparameter +overlay-ghost-color+ #x00A8FF
  "Orange: the in-world marker's dot and name pill text.")

(defparameter +overlay-class-name+ "RappyRunsOverlay")

;;; --- State ----------------------------------------------------------

(defvar *overlay-hwnd* nil)
(defvar *overlay-process* nil)
(defvar *overlay-class-registered* nil)
(defvar *overlay-font* nil)
(defvar *overlay-small-font* nil
  "Split-row and marker-pill font, a step smaller than the clock.")
;; GDI objects below are created once per overlay thread - a 10-30 Hz
;; paint must not churn them.
(defvar *overlay-key-brush* nil)
(defvar *overlay-brush* nil)
(defvar *overlay-ghost-brush* nil)

(defvar *overlay-wanted* nil
  "T while the poll loop wants the overlay visible. The overlay thread's
timer reads it and shows/hides accordingly; writers never touch the
window directly (a window belongs to its creating thread).")
(defvar *overlay-visible* nil)
(defvar *overlay-line1* ""
  "Top line: the live clock (plus REC).")
(defvar *overlay-line2* nil
  "Second line: 'vs <ghost time> <gap>', or NIL without a ghost.")
(defvar *overlay-delta-state* :neutral
  ":ahead / :behind / :neutral - colors the second line.")
(defvar *overlay-ghost-data* nil
  "Ghost-panel snapshot from GHOST-OVERLAY-DATA, or NIL. The overlay
thread interpolates the in-world marker from its :track/:elapsed-at,
so the marker glides between the poll loop's updates.")

(defvar *overlay-corner* :top-right
  "Which corner of the game's client area the panel sits in - the
:overlay-corner setting, handed over by OVERLAY-SHOW! so the overlay
thread never touches CONFIG-VALUE.")

(defvar *overlay-custom-pos* nil
  "The configured :overlay-position (x-frac y-frac) that rode in with
OVERLAY-SHOW!; read when *OVERLAY-CORNER* is :custom.")

(defvar *overlay-input-enabled* nil
  "T while WS_EX_TRANSPARENT is lifted (Ctrl held): the opaque panel
takes mouse input so it can be dragged, while the color-keyed rest of
the window stays click-through by transparency.")

(defvar *overlay-drag* nil
  "(:grab-dx DX :grab-dy DY :pos (x-frac y-frac)) while a Ctrl+drag is
in progress; :pos overrides the configured anchor for the duration.
Overlay thread only.")

(defvar *overlay-drop-pos* nil
  "The (x-frac y-frac) a finished drag left as the live position, so
the panel does not snap back to the old anchor during the <=250 ms
until the poll loop persists it. Cleared by the next OVERLAY-SHOW!,
whose arguments then carry the persisted position.")

(defvar *overlay-dragged-pos* nil
  "The (x-frac y-frac) a finished drag left for the poll loop to write
into the config (UPDATE-GHOST-OVERLAY consumes it; the overlay thread
must not touch CONFIG-VALUE).")

(defvar *overlay-game-hwnd* nil
  "The game window the last timer tick fitted against, for the mouse
handlers (a FindWindow per WM_MOUSEMOVE would be wasteful).")

(defvar *overlay-game-rect* nil
  "The game client area's screen rect (left top right bottom) from the
last successful fit, for the drag's screen-to-area conversion.")

(defvar *overlay-size* nil
  "(width . height) the overlay window was last sized to, written by
the timer's reposition and read by the paint. NIL before the first
fit; the paint then falls back to the panel size.")

(defvar *overlay-full-p* nil
  "T while the window spans the game's whole client area (in-world
marker mode); NIL while it is just the corner panel. Decides where the
paint puts the panel origin.")

(defvar *overlay-placement* nil
  "(x y w h) last applied via SetWindowPos, so a tick with an unmoved
game window skips the call (it would otherwise fire at the marker's
30 Hz for nothing).")

(defvar *overlay-timer-current-ms* nil
  "The interval the overlay timer is currently set to, so the tick can
re-arm it only when the wanted rate actually changes.")

(defun overlay-ghost-active-p ()
  "Show the ghost panel (vs header + room-split rows) only while a
ghost is attached; a ghostless run keeps the compact v0.51.0 timer
pill."
  (and *overlay-ghost-data* t))

(defun overlay-marker-wanted-p ()
  "T when the in-world marker should be live: the ghost has a track to
interpolate AND the :ghost-marker setting rode in on the ghost data.
This - not the mere ghost panel - is what makes the window span the
client area and repaint at 30 Hz; with the marker off, the overlay
stays the panel-sized, 10 Hz window it was in v0.52."
  (let ((data *overlay-ghost-data*))
    (and data (getf data :marker)
         (let ((track (getf data :track)))
           (and (vectorp track) (plusp (length track))))
         t)))

(defun overlay-current-height ()
  (if (overlay-ghost-active-p) +overlay-ghost-height+ +overlay-compact-height+))

;;; --- Painting and geometry (overlay thread only) --------------------

(defun overlay-panel-position (area-w area-h)
  "The panel origin within the game's client area: an in-progress drag
first, then a just-dropped position awaiting persistence, then the
configured anchor / custom spot."
  (let ((live (or (getf *overlay-drag* :pos) *overlay-drop-pos*)))
    (overlay-panel-origin (if live :custom *overlay-corner*)
                          (or live *overlay-custom-pos*)
                          area-w area-h
                          +overlay-width+ (overlay-current-height)
                          +overlay-margin-x+ +overlay-margin-y+)))

(defun overlay-position (hwnd game-hwnd)
  "Fit the overlay against the game window's client area: the whole
area while the in-world marker is live (it can land anywhere; the
color key keeps all but the panel and marker transparent), just the
corner panel (at the :overlay-corner corner) otherwise. Skips the
SetWindowPos when the placement has not moved - this runs at up to
30 Hz. Returns :FIT on a usable placement, :DEGENERATE for a
zero-sized client rect (minimized game), NIL when the rect queries
fail outright (transient during display-mode switches; the caller
keeps the overlay up)."
  (let ((rect (window-client-screen-rect game-hwnd)))
    (when rect
      (destructuring-bind (left top right bottom) rect
        (let ((client-w (- right left))
              (client-h (- bottom top)))
          (if (or (<= client-w 0) (<= client-h 0))
              :degenerate
              (multiple-value-bind (panel-x panel-y)
                  (overlay-panel-position client-w client-h)
                (setf *overlay-game-rect* rect)
                (let* ((full (overlay-marker-wanted-p))
                       (x (if full left (+ left panel-x)))
                       (y (if full top (+ top panel-y)))
                       (w (if full client-w +overlay-width+))
                       (h (if full client-h (overlay-current-height)))
                       (placement (list x y w h)))
                  (setf *overlay-size* (cons w h)
                        *overlay-full-p* full)
                  (unless (equal placement *overlay-placement*)
                    (setf *overlay-placement* placement)
                    (%set-window-pos hwnd 0 x y w h
                                     (logior +swp-nozorder+
                                             +swp-noactivate+)))
                  :fit))))))))

(defun overlay-fill-rect (hdc left top right bottom brush)
  (fli:with-dynamic-foreign-objects ()
    (let ((rect (fli:allocate-dynamic-foreign-object :type 'win-rect)))
      (setf (fli:foreign-slot-value rect 'left) left
            (fli:foreign-slot-value rect 'top) top
            (fli:foreign-slot-value rect 'right) right
            (fli:foreign-slot-value rect 'bottom) bottom)
      (%fill-rect hdc rect brush))))

(defun overlay-draw-dot (hdc px py radius brush)
  (when brush
    (let ((old-brush (%select-object hdc brush))
          (old-pen (%select-object hdc (%get-stock-object +null-pen+))))
      (%ellipse hdc (- px radius) (- py radius)
                (+ px radius 1) (+ py radius 1))
      (%select-object hdc old-pen)
      (%select-object hdc old-brush))))

(defun overlay-effective-elapsed (data)
  "The live elapsed-ms right now, extrapolated from the poll loop's
last snapshot so the ghost dot glides at the repaint rate."
  (+ (getf data :elapsed-ms)
     (round (* 1000 (- (get-internal-real-time) (getf data :elapsed-at)))
            internal-time-units-per-second)))

(defun overlay-ghost-position (data elapsed)
  "(floor map x z [y]) of the ghost at ELAPSED on DATA's track, or NIL
before its first sample or once the ghost has finished - the in-world
marker's interpolated instant."
  (let ((track (getf data :track))
        (ghost-time-ms (getf data :ghost-time-ms)))
    (when (and (vectorp track) (plusp (length track))
               ghost-time-ms (<= elapsed ghost-time-ms))
      (let ((vals (multiple-value-list
                   (ghost-track-position track elapsed))))
        (and (first vals) vals)))))

(defun overlay-draw-splits (hdc data)
  "The room-split rows under the header: the newest matched rooms
(oldest of them first, so the list reads downward like the run), each
row the room label, its enter clock and the cumulative gap at that
entry - colored like the header's gap line."
  (let ((splits (reverse (getf data :splits)))
        (precision (getf data :precision)))
    (when (and splits *overlay-small-font*)
      (%select-object hdc *overlay-small-font*)
      (loop :for split :in splits
            :for y :from +overlay-split-top+ :by +overlay-split-row-height+
            :do (let ((label (tr :overlay-room (getf split :room)))
                      (clock (format-split-clock (getf split :ms)))
                      (delta (getf split :delta)))
                  (%set-text-color hdc +overlay-text-color+)
                  (%text-out hdc 12 y label (length label))
                  (%text-out hdc 116 y clock (length clock))
                  (let ((text (format-ghost-delta delta precision)))
                    (%set-text-color hdc (if (minusp delta)
                                             +overlay-ahead-color+
                                             +overlay-behind-color+))
                    (%text-out hdc 180 y text (length text))))))))

(defun overlay-draw-ghost-marker (hdc width height data ghost-pos)
  "The in-world ghost marker: GHOST-POS (the paint's shared (floor map
x z [y]) resolution) projected through the live game camera onto the
client area - an orange dot at the ghost's feet with a name pill above
it. Drawn only when the camera is fresh and the ghost stands on the
live player's floor. A ghost whose track predates the height column
borrows the live player's own height (same floor, so usually the same
ground)."
  (let ((camera *live-camera*))
    (when (and camera ghost-pos
               (eql (first ghost-pos) (getf data :floor)))
      (destructuring-bind (gfloor gmap gx gz &optional gy) ghost-pos
        (declare (ignore gfloor gmap))
        (multiple-value-bind (sx sy)
            (ghost-screen-position camera width height
                                   gx (or gy (getf data :own-y) 0.0)
                                   gz)
          (when (and sx
                     (< -50 sx (+ width 50))
                     (< -50 sy (+ height 50)))
            (overlay-draw-dot hdc sx sy 6 *overlay-ghost-brush*)
            (let ((label (let ((name (getf data :label)))
                           (if (and (stringp name)
                                    (string/= name ""))
                               name
                               "ghost"))))
              (when *overlay-small-font*
                (%select-object hdc *overlay-small-font*))
              (fli:with-dynamic-foreign-objects ((size win-text-size))
                (when (%get-text-extent hdc label (length label) size)
                  (let* ((tw (fli:foreign-slot-value size 'cx))
                         (th (fli:foreign-slot-value size 'cy))
                         (pill-bottom (- sy 10))
                         (pill-top (- pill-bottom th 4))
                         (pill-left (- sx (floor tw 2) 6))
                         (pill-right (+ sx (ceiling tw 2) 6)))
                    ;; A solid pill under the text: ClearType must
                    ;; never blend against the color key.
                    (overlay-fill-rect hdc pill-left pill-top
                                       pill-right pill-bottom
                                       *overlay-brush*)
                    (%set-text-color hdc +overlay-ghost-color+)
                    (%text-out hdc (+ pill-left 6) (+ pill-top 2)
                               label (length label))))))))))))

(defun overlay-paint (hwnd)
  "WM_PAINT: the key-color ground (transparent on screen), the dark
corner panel - clock, ghost gap line, room-split rows - and the
in-world ghost marker. BeginPaint/EndPaint must run even when drawing
errors out, or the invalid region never clears and WM_PAINT storms."
  (fli:with-dynamic-foreign-objects ()
    (let* ((ps (fli:allocate-dynamic-foreign-object :type 'win-paintstruct))
           (hdc (%begin-paint hwnd ps)))
      (unwind-protect
           (unless (fli:null-pointer-p hdc)
             (let* ((size *overlay-size*)
                    (full *overlay-full-p*)
                    (width (or (car size) +overlay-width+))
                    (height (or (cdr size) (overlay-current-height)))
                    (origin (and full
                                 (multiple-value-list
                                  (overlay-panel-position width height))))
                    (panel-x (if origin (first origin) 0))
                    (panel-y (if origin (second origin) 0))
                    (data *overlay-ghost-data*)
                    (ghost-active (overlay-ghost-active-p))
                    ;; The marker's position: only the full-window mode
                    ;; draws it, so skip the interpolation otherwise.
                    (ghost-pos (and data full
                                    (overlay-ghost-position
                                     data
                                     (overlay-effective-elapsed data)))))
               (when *overlay-key-brush*
                 (overlay-fill-rect hdc 0 0 width height
                                    *overlay-key-brush*))
               ;; The corner panel paints in its own coordinates via a
               ;; shifted viewport origin, exactly as it did when the
               ;; window WAS the panel.
               (%set-viewport-org hdc panel-x panel-y fli:*null-pointer*)
               (when *overlay-brush*
                 (overlay-fill-rect hdc 0 0 +overlay-width+
                                    (overlay-current-height)
                                    *overlay-brush*))
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
                   (%text-out hdc 12 32 line2 (length line2))))
               (when ghost-active
                 (overlay-draw-splits hdc data))
               (%set-viewport-org hdc 0 0 fli:*null-pointer*)
               (when (and full ghost-active)
                 (overlay-draw-ghost-marker hdc width height
                                            data ghost-pos))))
        (%end-paint hwnd ps)))))

;;; --- Ctrl+drag (overlay thread only) --------------------------------

(defun overlay-apply-input (hwnd enabled)
  "Lift / restore WS_EX_TRANSPARENT. With it lifted, the opaque panel
receives mouse input (Ctrl+drag can grab it) while the color-keyed
rest of the window stays click-through; restored, every pixel passes
clicks to the game again."
  (let* ((style (%get-window-long-ptr hwnd +gwl-exstyle+))
         (wanted (if enabled
                     (logandc2 style +ws-ex-transparent+)
                     (logior style +ws-ex-transparent+))))
    (unless (= style wanted)
      (%set-window-long-ptr hwnd +gwl-exstyle+ wanted)
      (%set-window-pos hwnd 0 0 0 0 0
                       (logior +swp-nomove+ +swp-nosize+ +swp-nozorder+
                               +swp-noactivate+ +swp-framechanged+)))
    (setf *overlay-input-enabled* enabled)))

(defun overlay-cursor-position ()
  "(values x y) of the cursor in screen coordinates, or NIL."
  (fli:with-dynamic-foreign-objects ()
    (let ((point (fli:allocate-dynamic-foreign-object :type 'win-point)))
      (when (%get-cursor-pos point)
        (values (fli:foreign-slot-value point 'x)
                (fli:foreign-slot-value point 'y))))))

(defun overlay-lparam-point (lparam)
  "(values x y) of the screen point packed into a WM_NCHITTEST lParam:
two signed 16-bit halves (negative on monitors left of / above the
primary)."
  (flet ((signed16 (value)
           (if (>= value #x8000) (- value #x10000) value)))
    (values (signed16 (ldb (byte 16 0) lparam))
            (signed16 (ldb (byte 16 16) lparam)))))

(defun overlay-panel-screen-origin ()
  "(values sx sy) of the panel's top-left in screen coordinates, or
NIL before the first good fit."
  (let ((rect *overlay-game-rect*))
    (when rect
      (destructuring-bind (left top right bottom) rect
        (multiple-value-bind (px py)
            (overlay-panel-position (- right left) (- bottom top))
          (values (+ left px) (+ top py)))))))

(defun overlay-panel-hit-p (x y)
  "Is the screen point (X Y) on the panel? Shared by the drag grab and
WM_NCHITTEST, so what is grabbable and what swallows a click are the
same pixels."
  (multiple-value-bind (sx sy) (overlay-panel-screen-origin)
    (and sx
         (<= sx x (+ sx +overlay-width+))
         (<= sy y (+ sy (overlay-current-height))))))

(defun overlay-drag-begin (hwnd)
  "Grab the panel under the cursor (input is enabled, so Ctrl is
held). T when the press landed on the panel; a miss cannot normally
reach here - WM_NCHITTEST already returned HTTRANSPARENT for it."
  (multiple-value-bind (cx cy) (overlay-cursor-position)
    (when (and cx (overlay-panel-hit-p cx cy))
      (multiple-value-bind (sx sy) (overlay-panel-screen-origin)
        (setf *overlay-drag*
              (list :grab-dx (- cx sx) :grab-dy (- cy sy)
                    :pos nil))
        (%set-capture hwnd)
        t))))

(defun overlay-drag-move (hwnd)
  "Follow the cursor: the panel origin clamps inside the game's client
area and is stored as slack fractions, then the window refits (compact
mode moves the window, full mode repaints the panel elsewhere)."
  (let ((drag *overlay-drag*)
        (rect *overlay-game-rect*))
    (when (and drag rect)
      (multiple-value-bind (cx cy) (overlay-cursor-position)
        (when cx
          (destructuring-bind (left top right bottom) rect
            (let* ((slack-x (max 0 (- right left +overlay-width+)))
                   (slack-y (max 0 (- bottom top
                                      (overlay-current-height))))
                   (px (min slack-x
                            (max 0 (- cx (getf drag :grab-dx) left))))
                   (py (min slack-y
                            (max 0 (- cy (getf drag :grab-dy) top)))))
              (setf (getf drag :pos)
                    (list (if (zerop slack-x) 0.0 (/ (float px) slack-x))
                          (if (zerop slack-y) 0.0 (/ (float py) slack-y))))
              (let ((game *overlay-game-hwnd*))
                (when game
                  (overlay-position hwnd game)))
              (%invalidate-rect hwnd fli:*null-pointer* nil))))))))

(defun overlay-drag-end ()
  "Finish the drag: keep the drop position live (no snap-back while
the poll loop catches up) and publish it for persistence. The publish
(*OVERLAY-DRAGGED-POS*) is written BEFORE the local override
(*OVERLAY-DROP-POS*): OVERLAY-SHOW! only clears the override when no
unconsumed publish exists, and this order means it can never observe
the override without the publish. Also runs on WM_CAPTURECHANGED,
including the one our own ReleaseCapture raises - *OVERLAY-DRAG* is
already NIL by then, so the guard makes it a no-op."
  (let ((drag *overlay-drag*))
    (when drag
      (setf *overlay-drag* nil)
      (let ((pos (getf drag :pos)))
        (when pos
          (setf *overlay-dragged-pos* (copy-list pos)
                *overlay-drop-pos* pos)))
      (ignore-errors (%release-capture)))))

(defun overlay-conceal (hwnd)
  (when *overlay-drag*
    (ignore-errors (overlay-drag-end)))
  (when *overlay-input-enabled*
    (ignore-errors (overlay-apply-input hwnd nil)))
  (when *overlay-visible*
    (%show-window hwnd +sw-hide+)
    (setf *overlay-visible* nil)))

(defun overlay-timer-tick (hwnd)
  "Follow the game window and repaint while wanted, hide otherwise
(also when the game window is gone mid-quest, or minimized to a
degenerate client rect). A transient rect-query failure (display-mode
switch, window recreation) keeps the overlay up at its last placement
rather than blinking it off. Re-arms the timer between the pill rate
and the marker rate as the in-world marker comes and goes."
  (let ((wanted-ms (if (overlay-marker-wanted-p)
                       +overlay-marker-timer-ms+
                       +overlay-timer-ms+)))
    (unless (eql wanted-ms *overlay-timer-current-ms*)
      (setf *overlay-timer-current-ms* wanted-ms)
      (%set-timer hwnd +overlay-timer-id+ wanted-ms fli:*null-pointer*)))
  (let* ((game (and *overlay-wanted* (find-psobb-window)))
         (fit (and game (overlay-position hwnd game))))
    (setf *overlay-game-hwnd* game)
    (cond
      ((or (null game) (eq fit :degenerate))
       (overlay-conceal hwnd))
      ;; :FIT, or a transient NIL after at least one good fit.
      ((or (eq fit :fit) *overlay-placement*)
       (unless *overlay-visible*
         (%show-window hwnd +sw-shownoactivate+)
         (setf *overlay-visible* t))
       ;; Ctrl arms the drag - but only while the game itself is the
       ;; foreground window, so Ctrl+clicks aimed at another app that
       ;; overlaps the topmost panel are never eaten. Input goes back
       ;; to full click-through once Ctrl is released (an in-flight
       ;; drag keeps it until its button-up).
       (let ((arm (and (minusp (%get-async-key-state +vk-control+))
                       (fli:pointer-eq game (%get-foreground-window)))))
         (cond ((and arm (not *overlay-input-enabled*))
                (ignore-errors (overlay-apply-input hwnd t)))
               ((and (not arm) *overlay-input-enabled*
                     (null *overlay-drag*))
                (ignore-errors (overlay-apply-input hwnd nil)))))
       (%invalidate-rect hwnd fli:*null-pointer* nil)))))

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
    ;; Hit testing runs only while Ctrl has lifted WS_EX_TRANSPARENT.
    ;; Everything outside the panel - including the in-world marker's
    ;; opaque pixels - reports HTTRANSPARENT so the click continues to
    ;; the game window below; only the panel itself takes input.
    ((= msg +wm-nchittest+)
     (if (or *overlay-drag*
             (multiple-value-bind (x y) (overlay-lparam-point lparam)
               (ignore-errors (overlay-panel-hit-p x y))))
         +ht-client+
         +ht-transparent+))
    ((= msg +wm-lbuttondown+)
     (if (ignore-errors (overlay-drag-begin hwnd))
         0
         (%def-window-proc hwnd msg wparam lparam)))
    ((= msg +wm-mousemove+)
     (when *overlay-drag*
       (ignore-errors (overlay-drag-move hwnd)))
     0)
    ((= msg +wm-lbuttonup+)
     (when *overlay-drag*
       (ignore-errors (overlay-drag-end)))
     0)
    ((= msg +wm-capturechanged+)
     (when *overlay-drag*
       (ignore-errors (overlay-drag-end)))
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
                     0 0 +overlay-width+ +overlay-ghost-height+
                     fli:*null-pointer* fli:*null-pointer*
                     (%get-module-handle fli:*null-pointer*)
                     fli:*null-pointer*)))
          (when (fli:null-pointer-p hwnd)
            (return-from overlay-thread-main))
          (setf *overlay-hwnd* hwnd
                *overlay-visible* nil
                *overlay-size* nil
                *overlay-full-p* nil
                *overlay-placement* nil
                ;; Drag state must not survive a thread restart: the
                ;; fresh window starts WS_EX_TRANSPARENT, so a stale
                ;; *overlay-drag* could never receive its button-up
                ;; and would pin the panel forever.
                *overlay-drag* nil
                *overlay-drop-pos* nil
                *overlay-input-enabled* nil
                *overlay-timer-current-ms* +overlay-timer-ms+)
          ;; Alpha blends the visible elements; the color key punches
          ;; the rest of the client-area-sized window fully out.
          (%set-layered-window-attributes hwnd +overlay-key-color+
                                          +overlay-alpha+
                                          (logior +lwa-alpha+
                                                  +lwa-colorkey+))
          ;; Keep the overlay out of screen captures (see the FLI
          ;; binding's comment); the submitted run footage must show
          ;; the game, not our panel. Best-effort on older Windows.
          (%set-window-display-affinity hwnd +wda-excludefromcapture+)
          (setf *overlay-font*
                (%create-font 20 0 0 0 +fw-semibold+ 0 0 0
                              +default-charset+ 0 0 +cleartype-quality+ 0
                              "Segoe UI")
                *overlay-small-font*
                (%create-font 16 0 0 0 +fw-semibold+ 0 0 0
                              +default-charset+ 0 0 +cleartype-quality+ 0
                              "Segoe UI")
                *overlay-key-brush* (%create-solid-brush +overlay-key-color+)
                *overlay-brush* (%create-solid-brush +overlay-bg-color+)
                *overlay-ghost-brush* (%create-solid-brush +overlay-ghost-color+))
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
  (dolist (holder '(*overlay-font* *overlay-small-font* *overlay-key-brush*
                    *overlay-brush* *overlay-ghost-brush*))
    (when (symbol-value holder)
      (ignore-errors (%delete-object (symbol-value holder)))
      (setf (symbol-value holder) nil)))
  (setf *overlay-hwnd* nil
        *overlay-process* nil
        *overlay-visible* nil))

;;; --- Poll-loop API --------------------------------------------------

(defun overlay-show! (line1 line2 delta-state ghost-data corner
                      custom-pos)
  "Want the overlay visible with this content, its panel in CORNER of
the game's client area (CUSTOM-POS is the :overlay-position fractions
read when CORNER is :custom). Starts the overlay thread lazily;
best-effort - a failure here must never touch the poll loop. Clears
the drop-position override only when no unconsumed drag publish
exists: the caller consumed *OVERLAY-DRAGGED-POS* before building
these arguments, so normally they already carry any dragged spot - but
a drag that finished DURING this tick must keep its override until the
next tick persists it, or the panel would snap back for ~250 ms."
  (setf *overlay-line1* (or line1 "")
        *overlay-line2* line2
        *overlay-delta-state* (or delta-state :neutral)
        *overlay-ghost-data* ghost-data
        *overlay-corner* (or corner :top-right)
        *overlay-custom-pos* custom-pos
        *overlay-wanted* t)
  (when (null *overlay-dragged-pos*)
    (setf *overlay-drop-pos* nil))
  (unless (and *overlay-process* (mp:process-alive-p *overlay-process*))
    (ignore-errors
      (setf *overlay-process*
            (mp:process-run-function "eta-client-overlay" '()
                                     'overlay-thread-main)))))

(defun overlay-hide! ()
  "Want the overlay gone; the overlay thread's next tick hides it. The
thread itself stays up - an idle timer costs nothing and the next
quest reuses it."
  (setf *overlay-wanted* nil
        *overlay-ghost-data* nil))

(defun update-ghost-overlay (detector recording-p)
  "Reflect the quest state into the overlay: timer, ghost gap and the
room splits during a quest, hidden otherwise or when the setting is
off. Called from UPDATE-GAME-STATUS at its 4 Hz cadence; the overlay
thread interpolates the in-world marker in between."
  ;; A finished Ctrl+drag is persisted BEFORE the show arguments are
  ;; built from the config, so they already reflect the new spot and
  ;; the overlay can drop its local override without a snap-back.
  (let ((dragged *overlay-dragged-pos*))
    (when dragged
      (setf *overlay-dragged-pos* nil
            (config-value :overlay-corner) :custom
            (config-value :overlay-position) dragged)
      (save-config!)
      (ignore-errors (refresh-overlay-corner-pane))))
  (if (and (config-value :ghost-overlay)
           (eq (detector-state detector) :in-quest))
      (let* ((race *ghost-race*)
             (delta (and race (ghost-race-delta-ms race)))
             (telemetry (detector-telemetry detector))
             (elapsed (and telemetry
                           (telemetry-elapsed-ms
                            telemetry (get-internal-real-time))))
             (ghost-data (and race elapsed
                              (ghost-overlay-data
                               race elapsed
                               :marker (config-value :ghost-marker)))))
        (overlay-show!
         (format nil "~a~:[~; REC~]"
                 (format-run-time (or (detector-elapsed-ms detector) 0))
                 recording-p)
         (and race (ghost-vs-text race))
         (cond ((null delta) :neutral)
               ((minusp delta) :ahead)
               (t :behind))
         ghost-data
         (config-value :overlay-corner)
         (config-value :overlay-position)))
      (overlay-hide!)))
