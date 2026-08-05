(in-package :ephinea-ta-client)

;;; LispWorks-only: Windows.Graphics.Capture (WGC) window capture - the
;;; overlap-proof, flip-model-proof way to record a windowed game. The
;;; DWM hands over the window's OWN composited frames, so unlike the
;;; ddagrab monitor capture nothing overlapping the game ever lands in
;;; a recording, and unlike gdigrab it works for surfaces GDI cannot
;;; read (run 949). ffmpeg has no WGC input device, so the frames go to
;;; ffmpeg the same way the game audio already does: this file captures
;;; them itself and serves raw BGRA over a named pipe that
;;; BUILD-FFMPEG-ARGS wires up as a rawvideo input.
;;;
;;; Everything is raw WinRT-over-COM through FLI - no delegates: the
;;; frame pool is created with CreateFreeThreaded and polled with
;;; TryGetNextFrame from a worker thread (validated end-to-end on this
;;; machine before integration, 2026-08-05). WinRT interface vtables
;;; start after IInspectable's six slots (QI, AddRef, Release, GetIids,
;;; GetRuntimeClassName, GetTrustLevel).
;;;
;;; Availability: Windows 10 1903+ (CreateForWindow + free-threaded
;;; pool). WGC-AVAILABLE-P probes once and older machines keep the
;;; gdigrab-probe / ddagrab paths. Windows draws a border around the
;;; captured window on some versions; PUT-IsBorderRequired=false is
;;; attempted and quietly ignored where the OS refuses - the border is
;;; an on-screen indicator only and never appears in the frames.

(fli:register-module :combase :real-name "combase" :connection-style :automatic)
(fli:register-module :d3d11-dll :real-name "d3d11" :connection-style :automatic)

(fli:define-foreign-function (%ro-initialize "RoInitialize")
    ((init-type :int))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :combase)

(fli:define-foreign-function (%windows-create-string "WindowsCreateString")
    ((source :pointer)
     (length (:unsigned :int))
     (hstring (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :combase)

(fli:define-foreign-function (%windows-delete-string "WindowsDeleteString")
    ((hstring :pointer))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :combase)

(fli:define-foreign-function (%ro-get-activation-factory "RoGetActivationFactory")
    ((classid :pointer)
     (iid :pointer)
     (factory (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :combase)

(fli:define-foreign-function (%d3d11-create-device "D3D11CreateDevice")
    ((adapter :pointer) (driver-type :int) (software :pointer)
     (flags (:unsigned :int)) (feature-levels :pointer)
     (num-levels (:unsigned :int)) (sdk-version (:unsigned :int))
     (device (:reference-return :pointer))
     (feature-level :pointer)
     (context (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :d3d11-dll)

(fli:define-foreign-function (%create-direct3d11-device-from-dxgi-device
                              "CreateDirect3D11DeviceFromDXGIDevice")
    ((dxgi-device :pointer)
     (inspectable (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall
  :module :d3d11-dll)

(fli:define-foreign-function (%rtl-move-memory "RtlMoveMemory")
    ((destination :pointer)
     (source :pointer)
     (bytes (:unsigned :long-long)))
  :result-type :void :calling-convention :stdcall
  :module :kernel32)

;;; WinRT/D3D vtable calls (COM-METHOD fetches the slot).

(fli:define-foreign-funcallable wgc-call-create-for-window
    ((this :pointer) (hwnd :pointer) (riid :pointer)
     (item (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-create-free-threaded
    ((this :pointer) (device :pointer) (pixel-format :int) (buffers :int)
     (size :long-long)                  ; SizeInt32 by value: w | h<<32
     (pool (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-get-object
    ((this :pointer) (object (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-create-capture-session
    ((this :pointer) (item :pointer) (session (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-get-size
    ((this :pointer) (size :pointer))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-get-interface
    ((this :pointer) (riid :pointer) (object (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-put-bool
    ((this :pointer) (value (:unsigned :byte)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable wgc-call-get-bool
    ((this :pointer) (value (:reference-return (:unsigned :byte))))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable d3d-call-create-texture2d
    ((this :pointer) (desc :pointer) (initial :pointer)
     (texture (:reference-return :pointer)))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable d3d-call-copy-resource
    ((this :pointer) (destination :pointer) (source :pointer))
  :result-type :void :calling-convention :stdcall)

(fli:define-foreign-funcallable d3d-call-map
    ((this :pointer) (resource :pointer) (subresource (:unsigned :int))
     (map-type :int) (flags (:unsigned :int)) (mapped :pointer))
  :result-type (:unsigned :long) :calling-convention :stdcall)

(fli:define-foreign-funcallable d3d-call-unmap
    ((this :pointer) (resource :pointer) (subresource (:unsigned :int)))
  :result-type :void :calling-convention :stdcall)

(fli:define-c-struct wgc-texture2d-desc
  (width (:unsigned :int)) (height (:unsigned :int))
  (mip-levels (:unsigned :int)) (array-size (:unsigned :int))
  (tex-format (:unsigned :int))
  (sample-count (:unsigned :int)) (sample-quality (:unsigned :int))
  (usage :int) (bind-flags (:unsigned :int))
  (cpu-access-flags (:unsigned :int)) (misc-flags (:unsigned :int)))

(fli:define-c-struct wgc-mapped-subresource
  (data :pointer) (row-pitch (:unsigned :int)) (depth-pitch (:unsigned :int)))

(fli:define-c-struct wgc-size-int32
  (cx :int) (cy :int))

;;; GUIDs, allocated lazily at run time (the delivery lesson: foreign
;;; memory allocated at image-build time dangles in the shipped exe).

(defvar *wgc-guids* nil
  "Plist of the WGC/D3D interface IIDs, built on first use.")

(defun wgc-guid (name)
  (unless *wgc-guids*
    (setf *wgc-guids*
          (list
           ;; IGraphicsCaptureItemInterop
           :item-interop (make-guid #x3628E81B #x3CAC #x4C60
                                    '(#xB7 #xF4 #x23 #xCE #x0E #x0C #x33 #x56))
           ;; IGraphicsCaptureItem
           :item (make-guid #x79C3F95B #x31F7 #x4EC2
                            '(#xA4 #x64 #x63 #x2E #xF5 #xD3 #x07 #x60))
           ;; IDirect3D11CaptureFramePoolStatics2 (CreateFreeThreaded)
           :pool-statics2 (make-guid #x589B103F #x6BBC #x5DF5
                                     '(#xA9 #x91 #x02 #xE2 #x8B #x3B #x66 #xD5))
           ;; IGraphicsCaptureSessionStatics (IsSupported)
           :session-statics (make-guid #x2224A540 #x5974 #x49AA
                                       '(#xB2 #x32 #x08 #x82 #x53 #x6F #x4C #xB5))
           ;; IGraphicsCaptureSession2 (IsCursorCaptureEnabled)
           :session2 (make-guid #x2C39AE40 #x7D2E #x5044
                                '(#x80 #x4E #x8B #x67 #x99 #xD4 #xCF #x9E))
           ;; IGraphicsCaptureSession3 (IsBorderRequired)
           :session3 (make-guid #xF2CDD966 #x22AE #x5EA1
                                '(#x95 #x96 #x3A #x28 #x93 #x44 #xC3 #xBE))
           ;; IClosable
           :closable (make-guid #x30D5A829 #x7FA4 #x4026
                                '(#x83 #xBB #xD7 #x5B #xAE #x4E #xA9 #x9E))
           ;; IDXGIDevice
           :dxgi-device (make-guid #x54EC77FA #x1377 #x44E6
                                   '(#x8C #x32 #x88 #xFD #x5F #x44 #xC8 #x4C))
           ;; IDirect3DDevice (WinRT)
           :winrt-device (make-guid #xA37624AB #x8D5F #x4650
                                    '(#x9D #x3E #x9E #xAE #x3D #x9B #xC6 #x70))
           ;; IDirect3DDxgiInterfaceAccess
           :dxgi-access (make-guid #xA9B3D012 #x3DF2 #x4EE3
                                   '(#xB8 #xD1 #x86 #x95 #xF4 #x57 #xD3 #xC1))
           ;; ID3D11Texture2D
           :texture2d (make-guid #x6F15AAF2 #xD208 #x4E89
                                 '(#x9A #xB4 #x48 #x95 #x35 #xD3 #x4F #x9C)))))
  (getf *wgc-guids* name))

(defun wgc-hstring (string)
  "A WinRT HSTRING of STRING; the caller owns it (WindowsDeleteString)."
  (fli:with-foreign-string (pointer element-count byte-count
                            :external-format :unicode)
      string
    (declare (ignore byte-count))
    (multiple-value-bind (hr hstring)
        (%windows-create-string pointer (1- element-count) 0)
      (unless (zerop hr) (error "WindowsCreateString hr=~x" hr))
      hstring)))

(defun wgc-query (object iid-name)
  "QueryInterface (slot 0), or NIL. Same call shape as
COM-CALL-GET-SERVICE."
  (multiple-value-bind (hr out)
      (com-call-get-service (com-method object 0) object
                            (wgc-guid iid-name) 0)
    (and (zerop hr) (not (fli:null-pointer-p out)) out)))

(defun wgc-activation-factory (class-name iid-name)
  "The activation factory of CLASS-NAME under the IID-NAME interface,
or NIL when the OS does not have it (pre-WGC Windows 10)."
  (let ((hstring (ignore-errors (wgc-hstring class-name))))
    (when hstring
      (unwind-protect
           (multiple-value-bind (hr factory)
               (%ro-get-activation-factory hstring (wgc-guid iid-name) 0)
             (and (zerop hr) (not (fli:null-pointer-p factory)) factory))
        (%windows-delete-string hstring)))))

;;; Availability. Cached: the OS does not grow the API mid-session.

(defvar *wgc-support* nil
  "NIL until probed, then :YES or :NO.")

(defun wgc-available-p ()
  "T when this Windows can capture a window via WGC: the activation
factories exist AND GraphicsCaptureSession.IsSupported() says yes.
Never signals; any surprise reads as unsupported, which only means the
old capture paths stay in charge."
  (when (null *wgc-support*)
    (setf *wgc-support*
          (handler-case
              (progn
                (%ro-initialize 1)     ; RO_INIT_MULTITHREADED; idempotent
                (let ((statics (wgc-activation-factory
                                "Windows.Graphics.Capture.GraphicsCaptureSession"
                                :session-statics))
                      (interop (wgc-activation-factory
                                "Windows.Graphics.Capture.GraphicsCaptureItem"
                                :item-interop)))
                  (unwind-protect
                       (if (and statics interop
                                (multiple-value-bind (hr supported)
                                    (wgc-call-get-bool (com-method statics 6)
                                                       statics 0)
                                  (and (zerop hr) (plusp supported))))
                           :yes
                           :no)
                    (when statics (com-release statics))
                    (when interop (com-release interop)))))
            (error (condition)
              (recording-log "wgc probe failed: ~a" condition)
              :no)))
    (recording-log "wgc support: ~a" *wgc-support*))
  (eq *wgc-support* :yes))

;;; The capture session

(defun video-pipe-name ()
  "Named pipe ffmpeg reads raw BGRA video from (first input)."
  "\\\\.\\pipe\\ephinea-ta-video")

(defstruct wgc-session
  pipe-name
  pipe-handle
  thread
  width height          ; frame (window) size the pipe carries
  (framerate 30)
  ;; COM object pointers, released by CLOSE-WGC-SESSION
  device context winrt-inspectable winrt-device item pool session staging
  frame-buffer          ; foreign W*H*4 BGRA scratch the pipe writes from
  (stop-flag nil)
  ;; diagnostics
  connect-ms
  (frames-written 0)
  (frames-fresh 0)      ; frames that came from a new WGC delivery
  error)

(defun wgc-window-size (item)
  "The capture item's SizeInt32 as (values width height)."
  (fli:with-dynamic-foreign-objects ((size wgc-size-int32))
    (let ((hr (wgc-call-get-size (com-method item 7) item size)))
      (unless (zerop hr) (error "get_Size hr=~x" hr))
      (values (fli:foreign-slot-value size 'cx)
              (fli:foreign-slot-value size 'cy)))))

(defun wgc-make-staging-texture (device width height)
  "A CPU-readable staging texture the GPU frames are copied into."
  (fli:with-dynamic-foreign-objects ((desc wgc-texture2d-desc))
    (setf (fli:foreign-slot-value desc 'width) width
          (fli:foreign-slot-value desc 'height) height
          (fli:foreign-slot-value desc 'mip-levels) 1
          (fli:foreign-slot-value desc 'array-size) 1
          (fli:foreign-slot-value desc 'tex-format) 87 ; B8G8R8A8_UNORM
          (fli:foreign-slot-value desc 'sample-count) 1
          (fli:foreign-slot-value desc 'sample-quality) 0
          (fli:foreign-slot-value desc 'usage) 3       ; STAGING
          (fli:foreign-slot-value desc 'bind-flags) 0
          (fli:foreign-slot-value desc 'cpu-access-flags) #x20000 ; READ
          (fli:foreign-slot-value desc 'misc-flags) 0)
    (multiple-value-bind (hr staging)
        (d3d-call-create-texture2d (com-method device 5) device desc
                                   fli:*null-pointer* 0)
      (unless (zerop hr) (error "CreateTexture2D(staging) hr=~x" hr))
      staging)))

(defun start-wgc-session (hwnd &key (framerate +record-framerate+))
  "Create the whole WGC capture for HWND - D3D device, capture item,
free-threaded frame pool, session - plus the pipe server end and the
feeder thread, WITHOUT starting the capture yet (the thread starts it
once ffmpeg connects, so video time 0 is the first delivered frame).
Returns the WGC-SESSION, or NIL with the reason logged; the caller
falls back to the ddagrab/gdigrab paths. Synchronous and cheap (~a few
ms): everything that can fail does so here, at capture-start, never
mid-quest."
  (handler-case
      (progn
        (%ro-initialize 1)
        (multiple-value-bind (hr device context)
            (%d3d11-create-device fli:*null-pointer* 1 fli:*null-pointer*
                                  #x20 ; BGRA support
                                  fli:*null-pointer* 0 7 0
                                  fli:*null-pointer* 0)
          (unless (zerop hr) (error "D3D11CreateDevice hr=~x" hr))
          (let ((session (make-wgc-session :device device :context context
                                           :framerate framerate)))
            (handler-case
                (progn
                  (let* ((dxgi (or (wgc-query device :dxgi-device)
                                   (error "QI IDXGIDevice failed")))
                         (inspectable
                           (unwind-protect
                                (multiple-value-bind (hr inspectable)
                                    (%create-direct3d11-device-from-dxgi-device
                                     dxgi 0)
                                  (unless (zerop hr)
                                    (error "CreateDirect3D11Device hr=~x" hr))
                                  inspectable)
                             (com-release dxgi))))
                    (setf (wgc-session-winrt-inspectable session) inspectable
                          (wgc-session-winrt-device session)
                          (or (wgc-query inspectable :winrt-device)
                              (error "QI IDirect3DDevice failed"))))
                  (let ((interop (or (wgc-activation-factory
                                      "Windows.Graphics.Capture.GraphicsCaptureItem"
                                      :item-interop)
                                     (error "no GraphicsCaptureItem factory"))))
                    (unwind-protect
                         (multiple-value-bind (hr item)
                             (wgc-call-create-for-window
                              (com-method interop 3) interop hwnd
                              (wgc-guid :item) 0)
                           (unless (zerop hr)
                             (error "CreateForWindow hr=~x" hr))
                           (setf (wgc-session-item session) item))
                      (com-release interop)))
                  (multiple-value-bind (width height)
                      (wgc-window-size (wgc-session-item session))
                    (when (or (< width +capture-crop-min-pixels+)
                              (< height +capture-crop-min-pixels+))
                      (error "window too small for capture: ~dx~d"
                             width height))
                    (setf (wgc-session-width session) width
                          (wgc-session-height session) height)
                    (let ((statics (or (wgc-activation-factory
                                        "Windows.Graphics.Capture.Direct3D11CaptureFramePool"
                                        :pool-statics2)
                                       (error "no FramePool factory"))))
                      (unwind-protect
                           (multiple-value-bind (hr pool)
                               (wgc-call-create-free-threaded
                                (com-method statics 6) statics
                                (wgc-session-winrt-device session)
                                87 2
                                (logior (ldb (byte 32 0) width)
                                        (ash (ldb (byte 32 0) height) 32))
                                0)
                             (unless (zerop hr)
                               (error "CreateFreeThreaded hr=~x" hr))
                             (setf (wgc-session-pool session) pool))
                        (com-release statics)))
                    (multiple-value-bind (hr capture-session)
                        (wgc-call-create-capture-session
                         (com-method (wgc-session-pool session) 10)
                         (wgc-session-pool session)
                         (wgc-session-item session) 0)
                      (unless (zerop hr)
                        (error "CreateCaptureSession hr=~x" hr))
                      (setf (wgc-session-session session) capture-session))
                    ;; Cosmetics, both quietly optional: no mouse cursor
                    ;; in the recording (parity with draw_mouse=0), and
                    ;; no on-screen capture border where the OS lets an
                    ;; unpackaged app turn it off.
                    (let ((session2 (wgc-query (wgc-session-session session)
                                               :session2)))
                      (when session2
                        (wgc-call-put-bool (com-method session2 7) session2 0)
                        (com-release session2)))
                    (let ((session3 (wgc-query (wgc-session-session session)
                                               :session3)))
                      (when session3
                        (wgc-call-put-bool (com-method session3 7) session3 0)
                        (com-release session3)))
                    (setf (wgc-session-staging session)
                          (wgc-make-staging-texture device width height)
                          (wgc-session-frame-buffer session)
                          ;; Zero-filled: if the first WGC delivery is
                          ;; ever late, the pipe carries black, never
                          ;; uninitialized memory.
                          (fli:allocate-foreign-object
                           :type '(:unsigned :byte)
                           :nelems (* width height 4)
                           :initial-element 0))
                    (let ((pipe (%create-named-pipe (video-pipe-name)
                                                    +pipe-access-outbound+
                                                    0 ; byte mode, blocking
                                                    1 (* 4 1024 1024) 0 0
                                                    fli:*null-pointer*)))
                      (when (invalid-handle-p pipe)
                        (error "CreateNamedPipe(video) failed (~d)"
                               (%get-last-error)))
                      (setf (wgc-session-pipe-name session) (video-pipe-name)
                            (wgc-session-pipe-handle session) pipe))
                    (setf (wgc-session-thread session)
                          (mp:process-run-function
                           "eta-wgc-capture" '()
                           (lambda ()
                             (handler-case (wgc-capture-loop session)
                               (error (condition)
                                 (setf (wgc-session-error session)
                                       (format nil "~a" condition))
                                 (recording-log "wgc capture loop died: ~a"
                                                condition)
                                 (ignore-errors
                                   (%close-handle
                                    (wgc-session-pipe-handle session))))))))
                    (recording-log "wgc session ready: ~dx~d @~d"
                                   width height framerate)
                    session))
              (error (condition)
                (close-wgc-session session)
                (error condition))))))
    (error (condition)
      (recording-log "wgc session failed (falling back): ~a" condition)
      nil)))

(defun wgc-drain-latest-frame (session)
  "The newest pending frame's texture copied into the staging texture;
T when a fresh frame arrived, NIL when nothing new was delivered (the
window content did not change). Every COM temporary is released before
returning - the pool only holds 2 buffers and starves if frames leak."
  (let ((pool (wgc-session-pool session))
        (fresh nil))
    (loop
      (multiple-value-bind (hr frame)
          (wgc-call-get-object (com-method pool 7) pool 0)
        (when (or (not (zerop hr)) (fli:null-pointer-p frame))
          (return fresh))
        (unwind-protect
             (multiple-value-bind (hr surface)
                 (wgc-call-get-object (com-method frame 6) frame 0)
               (when (and (zerop hr) (not (fli:null-pointer-p surface)))
                 (unwind-protect
                      (let ((access (wgc-query surface :dxgi-access)))
                        (when access
                          (unwind-protect
                               (multiple-value-bind (hr texture)
                                   (wgc-call-get-interface
                                    (com-method access 3) access
                                    (wgc-guid :texture2d) 0)
                                 (when (and (zerop hr)
                                            (not (fli:null-pointer-p texture)))
                                   (unwind-protect
                                        (progn
                                          (d3d-call-copy-resource
                                           (com-method (wgc-session-context session) 47)
                                           (wgc-session-context session)
                                           (wgc-session-staging session)
                                           texture)
                                          (setf fresh t))
                                     (com-release texture))))
                            (com-release access))))
                   (com-release surface))))
          (com-release frame))))))

(defun wgc-read-staging (session)
  "Map the staging texture and pack its rows into the session's frame
buffer (the GPU pads rows to ROW-PITCH; the pipe carries them tight)."
  (fli:with-dynamic-foreign-objects ((mapped wgc-mapped-subresource))
    (let* ((context (wgc-session-context session))
           (staging (wgc-session-staging session))
           (hr (d3d-call-map (com-method context 14) context staging
                             0 1 0 mapped)))
      (unless (zerop hr) (error "Map(staging) hr=~x" hr))
      (unwind-protect
           (let* ((data (fli:foreign-slot-value mapped 'data))
                  (pitch (fli:foreign-slot-value mapped 'row-pitch))
                  (width-bytes (* 4 (wgc-session-width session)))
                  (buffer (wgc-session-frame-buffer session)))
             (if (= pitch width-bytes)
                 (%rtl-move-memory buffer data
                                   (* width-bytes
                                      (wgc-session-height session)))
                 (loop :for row :from 0 :below (wgc-session-height session)
                       :do (%rtl-move-memory
                            (fli:make-pointer
                             :address (+ (fli:pointer-address buffer)
                                         (* row width-bytes)))
                            (fli:make-pointer
                             :address (+ (fli:pointer-address data)
                                         (* row pitch)))
                            width-bytes))))
        (d3d-call-unmap (com-method context 15) context staging 0)))))

(defun wgc-write-frame (session)
  "One frame from the frame buffer down the pipe; NIL once ffmpeg is
gone."
  (let ((bytes (* 4 (wgc-session-width session)
                  (wgc-session-height session))))
    (multiple-value-bind (ok written)
        (%write-file (wgc-session-pipe-handle session)
                     (wgc-session-frame-buffer session) bytes 0
                     fli:*null-pointer*)
      (and ok (= written bytes)))))

(defun wgc-capture-loop (session)
  "Feeder thread: wait for ffmpeg to open the pipe, start the WGC
delivery, then write one frame per tick at the session framerate -
fresh when the window changed, the previous frame again when it did
not (WGC only delivers on change; rawvideo needs constant cadence,
same as ddagrab's dup_frames). Ends when the stop flag is set or the
reader disappears; closing the pipe is ffmpeg's video EOF."
  (let ((connect-epoch (get-internal-real-time)))
    (unless (or (%connect-named-pipe (wgc-session-pipe-handle session)
                                     fli:*null-pointer*)
                (= (%get-last-error) +error-pipe-connected+))
      (error "ffmpeg never opened the video pipe"))
    (setf (wgc-session-connect-ms session)
          (round (* 1000 (- (get-internal-real-time) connect-epoch))
                 internal-time-units-per-second)))
  (let ((hr (com-call-no-args (com-method (wgc-session-session session) 6)
                              (wgc-session-session session))))
    (unless (zerop hr) (error "StartCapture hr=~x" hr)))
  ;; First frame: WGC delivers one immediately after StartCapture;
  ;; wait for it so the video never opens on an all-black buffer.
  (loop :repeat 100
        :until (or (wgc-session-stop-flag session)
                   (wgc-drain-latest-frame session))
        :do (sleep 0.01))
  (wgc-read-staging session)
  (let* ((units-per-frame (/ internal-time-units-per-second
                             (wgc-session-framerate session)))
         (deadline (get-internal-real-time)))
    (loop
      (when (wgc-session-stop-flag session) (return))
      (when (wgc-drain-latest-frame session)
        (wgc-read-staging session)
        (incf (wgc-session-frames-fresh session)))
      (unless (wgc-write-frame session)
        (return))
      (incf (wgc-session-frames-written session))
      (incf deadline units-per-frame)
      (let ((now (get-internal-real-time)))
        (cond ((> deadline now)
               (sleep (/ (- deadline now) internal-time-units-per-second)))
              ;; Fell behind (blocking pipe writes under encoder load):
              ;; never try to catch up with a frame burst, just carry
              ;; on from now.
              ((> (- now deadline) internal-time-units-per-second)
               (setf deadline now))))))
  (ignore-errors (%close-handle (wgc-session-pipe-handle session)))
  (setf (wgc-session-pipe-handle session) nil)
  (recording-log "wgc capture ended: ~d frames (~d fresh), connect ~a ms"
                 (wgc-session-frames-written session)
                 (wgc-session-frames-fresh session)
                 (wgc-session-connect-ms session)))

(defun stop-wgc-session (session)
  "Ask the feeder thread to finish. A thread still blocked waiting for
a reader is unblocked by briefly connecting to the pipe ourselves (the
audio session's trick)."
  (when session
    (setf (wgc-session-stop-flag session) t)
    (let ((client (%create-file (wgc-session-pipe-name session)
                                +generic-read+ 0 fli:*null-pointer*
                                +open-existing+ 0 fli:*null-pointer*)))
      (unless (invalid-handle-p client)
        (%close-handle client)))))

(defun close-wgc-session (session)
  "Release everything the session holds. Idempotent; called once the
feeder thread is done (or never ran). The WinRT session and pool get a
proper IClosable Close first, which stops the DWM delivery (and the
on-screen capture border) immediately."
  (when session
    (setf (wgc-session-stop-flag session) t)
    (let ((thread (wgc-session-thread session)))
      (when (and thread (mp:process-alive-p thread))
        (ignore-errors (mp:process-join thread :timeout 2))
        (when (mp:process-alive-p thread)
          ;; The feeder is somehow still running (a WriteFile that
          ;; never returned): leaking the COM objects is better than
          ;; releasing them out from under a live thread.
          (recording-log "wgc close: feeder thread still alive, leaking session")
          (return-from close-wgc-session))))
    (flet ((close-and-release (object)
             (when object
               (let ((closable (ignore-errors (wgc-query object :closable))))
                 (when closable
                   (ignore-errors
                     (com-call-no-args (com-method closable 6) closable))
                   (com-release closable)))
               (com-release object))))
      (close-and-release (shiftf (wgc-session-session session) nil))
      (close-and-release (shiftf (wgc-session-pool session) nil)))
    (com-release (shiftf (wgc-session-item session) nil))
    (com-release (shiftf (wgc-session-staging session) nil))
    (com-release (shiftf (wgc-session-winrt-device session) nil))
    (com-release (shiftf (wgc-session-winrt-inspectable session) nil))
    (com-release (shiftf (wgc-session-context session) nil))
    (com-release (shiftf (wgc-session-device session) nil))
    (let ((buffer (shiftf (wgc-session-frame-buffer session) nil)))
      (when buffer (fli:free-foreign-object buffer)))
    (let ((pipe (shiftf (wgc-session-pipe-handle session) nil)))
      (when pipe (ignore-errors (%close-handle pipe))))))
