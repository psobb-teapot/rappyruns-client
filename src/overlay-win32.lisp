(in-package :ephinea-ta-client)

;;; LispWorks-only: the in-game overlay (timer + ghost gap + course map
;;; + in-world ghost marker) drawn over the PSOBB window while a quest
;;; runs. Same architecture as the tray (tray-win32.lisp, which loads
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

(fli:define-foreign-function (%create-pen "CreatePen")
    ((style :int)
     (width :int)
     (color (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :gdi32)

;; POINT* as a flat LONG pair buffer; POINT is two 32-bit LONGs.
(fli:define-foreign-function (%polyline "Polyline")
    ((hdc :pointer)
     (points :pointer)
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

(fli:define-foreign-function (%save-dc "SaveDC")
    ((hdc :pointer))
  :result-type :int
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%restore-dc "RestoreDC")
    ((hdc :pointer)
     (saved :int))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :gdi32)

(fli:define-foreign-function (%intersect-clip-rect "IntersectClipRect")
    ((hdc :pointer)
     (left :int) (top :int) (right :int) (bottom :int))
  :result-type :int
  :calling-convention :stdcall
  :module :gdi32)

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
(defconstant +swp-nozorder+ #x4)
(defconstant +swp-noactivate+ #x10)
(defconstant +wm-paint+ #x000F)
(defconstant +wm-timer+ #x0113)
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
(defparameter +overlay-full-height+ 352
  "Header (two text lines) + course map + footer line.")
(defparameter +overlay-map-left+ 10)
(defparameter +overlay-map-top+ 58)
(defparameter +overlay-map-size+ 240
  "The square course-map area's side length.")
(defparameter +overlay-footer-y+ 306)
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
anti-aliasing on FillRect, Polyline, Ellipse; text always sits on a
solid panel or pill so ClearType never blends against the key).")
(defparameter +overlay-bg-color+ #x201410
  "Near-black blue-tinted background.")
(defparameter +overlay-text-color+ #xF0F0F0)
(defparameter +overlay-ahead-color+ #x78DC50
  "Green: ahead of the ghost.")
(defparameter +overlay-behind-color+ #x6E6EFF
  "Red: behind the ghost.")
(defparameter +overlay-map-bg-color+ #x2A1C16
  "The course-map panel, a step lighter than the window.")
(defparameter +overlay-ghost-color+ #x00A8FF
  "Orange: the ghost's course line and dot.")
(defparameter +overlay-own-color+ #xF0F0F0
  "White: the player's own breadcrumbs and dot.")

(defparameter +overlay-class-name+ "RappyRunsOverlay")

;;; --- State ----------------------------------------------------------

(defvar *overlay-hwnd* nil)
(defvar *overlay-process* nil)
(defvar *overlay-class-registered* nil)
(defvar *overlay-font* nil)
(defvar *overlay-small-font* nil
  "Footer font (distance line), a step smaller than the clock.")
;; GDI objects below are created once per overlay thread - a 10-30 Hz
;; paint must not churn them.
(defvar *overlay-key-brush* nil)
(defvar *overlay-brush* nil)
(defvar *overlay-map-brush* nil)
(defvar *overlay-ghost-brush* nil)
(defvar *overlay-own-brush* nil)
(defvar *overlay-ghost-pen* nil)
(defvar *overlay-own-pen* nil)

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
(defvar *overlay-map-data* nil
  "Course-map snapshot from GHOST-MAP-DATA, or NIL. The overlay thread
interpolates the ghost dot from its :track/:elapsed-at, so the dot
glides between the poll loop's updates.")

(defvar *overlay-corner* :top-right
  "Which corner of the game's client area the panel sits in - the
:overlay-corner setting, handed over by OVERLAY-SHOW! so the overlay
thread never touches CONFIG-VALUE.")

(defvar *overlay-proj-cache* nil
  "(:track T :floor F :project FN :ghost-points ((px py)...)): the
ghost's floor trace projected once per (track, floor) pair. The trace
is static for a floor, and reprojecting a long track's every row per
repaint pegged a core; the cache reduces per-paint work to the own
breadcrumbs and two dots.")

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

(defun overlay-map-active-p ()
  "Draw the course map only when there is a ghost course to draw; a
ghostless run keeps the compact v0.51.0 timer pill."
  (let ((data *overlay-map-data*))
    (and data
         (let ((track (getf data :track)))
           (and (vectorp track) (plusp (length track)))))))

(defun overlay-marker-wanted-p ()
  "T when the in-world marker should be live: a ghost course exists AND
the :ghost-marker setting rode in on the map data. This - not the mere
course map - is what makes the window span the client area and repaint
at 30 Hz; with the marker off, the overlay stays the panel-sized,
10 Hz window it was in v0.52."
  (let ((data *overlay-map-data*))
    (and data (getf data :marker) (overlay-map-active-p) t)))

(defun overlay-current-height ()
  (if (overlay-map-active-p) +overlay-full-height+ +overlay-compact-height+))

;;; --- Painting and geometry (overlay thread only) --------------------

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
                  (overlay-corner-origin *overlay-corner*
                                         client-w client-h
                                         +overlay-width+
                                         (overlay-current-height)
                                         +overlay-margin-x+
                                         +overlay-margin-y+)
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

(defun overlay-draw-polyline (hdc points pen)
  "POINTS is a list of (px py) pairs; under two points draws nothing."
  (let ((n (length points)))
    (when (and (>= n 2) pen)
      (fli:with-dynamic-foreign-objects ()
        (let ((buffer (fli:allocate-dynamic-foreign-object
                       :type :int :nelems (* 2 n))))
          (loop :for (px py) :in points
                :for k :from 0 :by 2
                :do (setf (fli:dereference buffer :index k) px
                          (fli:dereference buffer :index (1+ k)) py))
          (let ((old (%select-object hdc pen)))
            (%polyline hdc buffer n)
            (%select-object hdc old)))))))

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

(defun overlay-ghost-projection (track floor)
  "The cached projection for TRACK's trace on FLOOR (see
*OVERLAY-PROJ-CACHE*). Recomputed only when the track object or the
floor changes."
  (let ((cache *overlay-proj-cache*))
    (if (and cache
             (eq (getf cache :track) track)
             (eql (getf cache :floor) floor))
        cache
        (let* ((points (ghost-floor-track-points track floor))
               (project (map-projection (list points)
                                        +overlay-map-size+
                                        +overlay-map-size+)))
          (setf *overlay-proj-cache*
                (list :track track :floor floor :project project
                      :ghost-points
                      (and project
                           (mapcar (lambda (point)
                                     (multiple-value-bind (px py)
                                         (funcall project (first point)
                                                  (second point))
                                       (list px py)))
                                   points))))))))

(defun overlay-ghost-position (data elapsed)
  "(floor map x z [y]) of the ghost at ELAPSED on DATA's track, or NIL
before its first sample or once the ghost has finished. Resolved once
per paint and shared by the course map and the in-world marker, so
both draw the same interpolated instant."
  (let ((track (getf data :track))
        (ghost-time-ms (getf data :ghost-time-ms)))
    (when (and (vectorp track) (plusp (length track))
               ghost-time-ms (<= elapsed ghost-time-ms))
      (let ((vals (multiple-value-list
                   (ghost-track-position track elapsed))))
        (and (first vals) vals)))))

(defun overlay-draw-map (hdc data ghost-pos)
  "The course map: the ghost's route on the current floor as a line,
both runners as dots, and the footer distance line. The fit is
anchored to the ghost's floor trace (cached per floor) so the frame
stays stable while the player moves; positions outside it simply run
off the map - drawing is clipped to the panel, never clamped into it,
so the dots always show true relative positions."
  (let ((left +overlay-map-left+)
        (top +overlay-map-top+)
        (size +overlay-map-size+))
    (overlay-fill-rect hdc left top (+ left size) (+ top size)
                       (or *overlay-map-brush* *overlay-brush*))
    (when data
      (let* ((floor (getf data :floor))
             (own (getf data :own))
             (own-points (getf data :own-points))
             (track (getf data :track))
             (cache (and track (overlay-ghost-projection track floor)))
             (project (or (and cache (getf cache :project))
                          (map-projection (list own-points (list own))
                                          size size))))
        (when project
          (let ((saved (%save-dc hdc)))
            (unwind-protect
                 (progn
                   (%intersect-clip-rect hdc left top
                                         (+ left size) (+ top size))
                   (flet ((shift (point)
                            (list (+ left (first point))
                                  (+ top (second point))))
                          (at (point)
                            (multiple-value-bind (px py)
                                (funcall project (first point)
                                         (second point))
                              (list (+ left px) (+ top py)))))
                     (when cache
                       (overlay-draw-polyline
                        hdc (mapcar #'shift (getf cache :ghost-points))
                        *overlay-ghost-pen*))
                     (overlay-draw-polyline hdc (mapcar #'at own-points)
                                            *overlay-own-pen*)
                     (when (and ghost-pos (eql (first ghost-pos) floor))
                       (destructuring-bind (gx gy) (at (cddr ghost-pos))
                         (overlay-draw-dot hdc gx gy 5
                                           *overlay-ghost-brush*)))
                     (destructuring-bind (ox oy) (at own)
                       (overlay-draw-dot hdc ox oy 4 *overlay-own-brush*))))
              (%restore-dc hdc saved))))
        ;; Footer: where the ghost is, relative to the player. Map -1
        ;; marks pre-map-column telemetry - the area is unknown, not
        ;; Pioneer II (map 0).
        (let ((text
                (cond
                  ((null ghost-pos) nil)
                  ((eql (first ghost-pos) floor)
                   (let ((dx (- (third ghost-pos) (first own)))
                         (dz (- (fourth ghost-pos) (second own))))
                     (tr :overlay-ghost-away
                         (ghost-direction-arrow dx dz)
                         (format-ghost-distance dx dz))))
                  ((let ((gmap (second ghost-pos)))
                     (and (integerp gmap) (>= gmap 0)))
                   (tr :overlay-ghost-in
                       (client-map-name (second ghost-pos))))
                  (t (tr :overlay-ghost-elsewhere)))))
          (when text
            (when *overlay-small-font*
              (%select-object hdc *overlay-small-font*))
            (%set-text-color hdc +overlay-ghost-color+)
            (%text-out hdc 12 +overlay-footer-y+ text (length text))))))))

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
corner panel - clock, ghost gap line, course map, footer - and the
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
                                  (overlay-corner-origin
                                   *overlay-corner* width height
                                   +overlay-width+
                                   (overlay-current-height)
                                   +overlay-margin-x+
                                   +overlay-margin-y+))))
                    (panel-x (if origin (first origin) 0))
                    (panel-y (if origin (second origin) 0))
                    (data *overlay-map-data*)
                    (map-active (overlay-map-active-p))
                    ;; The ghost's position is resolved ONCE per paint
                    ;; so the map dot and the in-world marker show the
                    ;; same interpolated instant.
                    (ghost-pos (and data map-active
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
               (when map-active
                 (overlay-draw-map hdc data ghost-pos))
               (%set-viewport-org hdc 0 0 fli:*null-pointer*)
               (when (and full map-active)
                 (overlay-draw-ghost-marker hdc width height
                                            data ghost-pos))))
        (%end-paint hwnd ps)))))

(defun overlay-conceal (hwnd)
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
    (cond
      ((or (null game) (eq fit :degenerate))
       (overlay-conceal hwnd))
      ;; :FIT, or a transient NIL after at least one good fit.
      ((or (eq fit :fit) *overlay-placement*)
       (unless *overlay-visible*
         (%show-window hwnd +sw-shownoactivate+)
         (setf *overlay-visible* t))
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
                     0 0 +overlay-width+ +overlay-full-height+
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
                *overlay-map-brush* (%create-solid-brush +overlay-map-bg-color+)
                *overlay-ghost-brush* (%create-solid-brush +overlay-ghost-color+)
                *overlay-own-brush* (%create-solid-brush +overlay-own-color+)
                *overlay-ghost-pen* (%create-pen 0 2 +overlay-ghost-color+)
                *overlay-own-pen* (%create-pen 0 1 #x808080))
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
                    *overlay-brush* *overlay-map-brush*
                    *overlay-ghost-brush* *overlay-own-brush*
                    *overlay-ghost-pen* *overlay-own-pen*))
    (when (symbol-value holder)
      (ignore-errors (%delete-object (symbol-value holder)))
      (setf (symbol-value holder) nil)))
  (setf *overlay-hwnd* nil
        *overlay-process* nil
        *overlay-visible* nil))

;;; --- Poll-loop API --------------------------------------------------

(defun overlay-show! (line1 line2 delta-state map-data corner)
  "Want the overlay visible with this content, its panel in CORNER of
the game's client area. Starts the overlay thread lazily; best-effort
- a failure here must never touch the poll loop."
  (setf *overlay-line1* (or line1 "")
        *overlay-line2* line2
        *overlay-delta-state* (or delta-state :neutral)
        *overlay-map-data* map-data
        *overlay-corner* (or corner :top-right)
        *overlay-wanted* t)
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
        *overlay-map-data* nil))

(defun update-ghost-overlay (detector recording-p)
  "Reflect the quest state into the overlay: timer, ghost gap and the
course map during a quest, hidden otherwise or when the setting is
off. Called from UPDATE-GAME-STATUS at its 4 Hz cadence; the overlay
thread interpolates the ghost dot in between."
  (if (and (config-value :ghost-overlay)
           (eq (detector-state detector) :in-quest))
      (let* ((race *ghost-race*)
             (delta (and race (ghost-race-delta-ms race)))
             (telemetry (detector-telemetry detector))
             (elapsed (and telemetry
                           (telemetry-elapsed-ms
                            telemetry (get-internal-real-time))))
             (map-data (and race elapsed
                            (ghost-map-data
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
         map-data
         (config-value :overlay-corner)))
      (overlay-hide!)))
