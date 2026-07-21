(in-package :ephinea-ta-client)

;;; LispWorks-only: the live capture backend. Spawns ffmpeg.exe with
;;; CreateProcessW, holding a pipe to its stdin so a single "q" stops it
;;; gracefully (flushing the final MP4 fragment); TerminateProcess is
;;; the fallback. kernel32 is registered in win32.lisp.

(fli:define-c-struct security-attributes
  (nlength (:unsigned :long))
  (security-descriptor :pointer)
  (inherit-handle (:boolean :int)))

(fli:define-c-struct startupinfo-w
  (cb (:unsigned :long))
  (reserved :pointer)
  (desktop :pointer)
  (title :pointer)
  (x (:unsigned :long))
  (y (:unsigned :long))
  (x-size (:unsigned :long))
  (y-size (:unsigned :long))
  (x-count-chars (:unsigned :long))
  (y-count-chars (:unsigned :long))
  (fill-attribute (:unsigned :long))
  (flags (:unsigned :long))
  (show-window (:unsigned :short))
  (cb-reserved2 (:unsigned :short))
  (reserved2 :pointer)
  (std-input :pointer)
  (std-output :pointer)
  (std-error :pointer))

(fli:define-c-struct process-information
  (process :pointer)
  (thread :pointer)
  (pid (:unsigned :long))
  (tid (:unsigned :long)))

(fli:define-foreign-function (%create-pipe "CreatePipe")
    ((read-handle (:reference-return :pointer))
     (write-handle (:reference-return :pointer))
     (pipe-attributes (:pointer (:struct security-attributes)))
     (size (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%set-handle-information "SetHandleInformation")
    ((handle :pointer)
     (mask (:unsigned :long))
     (flags (:unsigned :long)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%create-process "CreateProcessW")
    ((application-name :pointer)
     (command-line :pointer)      ; writable WCHAR buffer, per the API
     (process-attributes :pointer)
     (thread-attributes :pointer)
     (inherit-handles (:boolean :int))
     (creation-flags (:unsigned :long))
     (environment :pointer)
     (current-directory :pointer)
     (startup-info (:pointer (:struct startupinfo-w)))
     (process-information (:pointer (:struct process-information))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

;; %write-file lives in win32.lisp (shared with the audio pipe).

(fli:define-foreign-function (%terminate-process "TerminateProcess")
    ((process :pointer)
     (exit-code (:unsigned :int)))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(defparameter +audio-drain-seconds+ 3
  "After the audio pipe EOF, how long ffmpeg gets to drain the buffered
audio tail before \"q\" stops it reading.")

(defconstant +startf-usestdhandles+ #x100)
(defconstant +create-no-window+ #x08000000)
(defconstant +below-normal-priority-class+ #x4000)
(defconstant +handle-flag-inherit+ 1)
;; For the child's stderr file (%create-file is bound in audio-win32).
(defconstant +generic-write+ #x40000000)
(defconstant +file-share-read+ 1)
(defconstant +create-always+ 2)
(defconstant +file-attribute-normal+ #x80)

;;; Windows command lines are one string; arguments must be quoted per
;;; the CommandLineToArgvW rules (the gdigrab title= argument contains
;;; spaces, and paths may contain spaces and quotes).

(defun quote-windows-arg (arg)
  (if (and (plusp (length arg))
           (notany (lambda (char) (find char '(#\Space #\Tab #\"))) arg))
      arg
      (with-output-to-string (out)
        (write-char #\" out)
        (let ((backslashes 0))
          (loop :for char :across arg
                :do (case char
                      (#\\ (incf backslashes))
                      (#\"
                       ;; Backslashes before a quote must be doubled,
                       ;; plus one to escape the quote itself.
                       (dotimes (i (1+ (* 2 backslashes)))
                         (write-char #\\ out))
                       (setf backslashes 0)
                       (write-char #\" out))
                      (t
                       (dotimes (i backslashes) (write-char #\\ out))
                       (setf backslashes 0)
                       (write-char char out))))
          ;; Trailing backslashes must not escape the closing quote.
          (dotimes (i (* 2 backslashes)) (write-char #\\ out)))
        (write-char #\" out))))

(defun argv->command-line (program args)
  (format nil "~{~a~^ ~}" (mapcar #'quote-windows-arg (cons program args))))

;;; Spawning

(defstruct ffmpeg-capture
  process-handle
  thread-handle
  stdin-write   ; our end of the child's stdin pipe
  pid
  audio         ; audio-session (audio-win32.lisp) or NIL
  stderr-path)  ; file the child's stderr goes to, or NIL

(defun create-stdin-pipe ()
  "An anonymous pipe whose read end the child may inherit as stdin.
Returns (values read-end write-end)."
  (fli:with-dynamic-foreign-objects ((attributes (:struct security-attributes)))
    (setf (fli:foreign-slot-value attributes 'nlength)
          (fli:size-of '(:struct security-attributes))
          (fli:foreign-slot-value attributes 'security-descriptor)
          fli:*null-pointer*
          (fli:foreign-slot-value attributes 'inherit-handle) t)
    (multiple-value-bind (ok read-end write-end)
        (%create-pipe 0 0 attributes 0)
      (unless ok
        (error "CreatePipe failed (Windows error ~d)" (%get-last-error)))
      ;; CreatePipe made both ends inheritable; only the child's may be.
      (%set-handle-information write-end +handle-flag-inherit+ 0)
      (values read-end write-end))))

(defun zero-startupinfo (startup)
  (setf (fli:foreign-slot-value startup 'cb)
        (fli:size-of '(:struct startupinfo-w)))
  (dolist (pointer-slot '(reserved desktop title reserved2
                          std-input std-output std-error))
    (setf (fli:foreign-slot-value startup pointer-slot) fli:*null-pointer*))
  (dolist (dword-slot '(x y x-size y-size x-count-chars y-count-chars
                        fill-attribute flags show-window cb-reserved2))
    (setf (fli:foreign-slot-value startup dword-slot) 0)))

(defun create-inheritable-log-file (path)
  "An inheritable write handle to a fresh file at PATH, for wiring a
child's stderr. NIL when the file cannot be created - stderr capture
is diagnostics, never worth failing the recording over."
  (fli:with-dynamic-foreign-objects
      ((attributes (:struct security-attributes)))
    (setf (fli:foreign-slot-value attributes 'nlength)
          (fli:size-of '(:struct security-attributes))
          (fli:foreign-slot-value attributes 'security-descriptor)
          fli:*null-pointer*
          (fli:foreign-slot-value attributes 'inherit-handle) t)
    (let ((handle (%create-file path +generic-write+ +file-share-read+
                                attributes +create-always+
                                +file-attribute-normal+ fli:*null-pointer*)))
      (unless (invalid-handle-p handle)
        handle))))

(defun spawn-process (program args &key stderr-path)
  "CreateProcessW PROGRAM with ARGS, stdin wired to a pipe we keep.
Returns an FFMPEG-CAPTURE; signals an error on failure. A bare PROGRAM
name (e.g. \"ffmpeg.exe\") is searched on PATH by the API.
With STDERR-PATH, the child's stderr is redirected to that file - at
-loglevel error anything ffmpeg says there is the reason a capture
died or recorded garbage, and TRANSCRIBE-CAPTURE-STDERR folds it into
the recording log when the capture is closed.
The child runs at below-normal priority: every process spawned here is
an ffmpeg (capture, remux, -version probe) sharing the machine with a
live game, and x264 at normal priority visibly stole frame time from
PSOBB. Windows still gives ffmpeg the leftover cores, so a capture
keeps up whenever the machine has headroom at all."
  (multiple-value-bind (stdin-read stdin-write) (create-stdin-pipe)
    (let ((stderr-handle (and stderr-path
                              (ignore-errors
                                (create-inheritable-log-file stderr-path)))))
      (handler-case
          (fli:with-dynamic-foreign-objects
              ((startup (:struct startupinfo-w))
               (process-info (:struct process-information)))
            (zero-startupinfo startup)
            (setf (fli:foreign-slot-value startup 'flags) +startf-usestdhandles+
                  (fli:foreign-slot-value startup 'std-input) stdin-read)
            (when stderr-handle
              (setf (fli:foreign-slot-value startup 'std-error) stderr-handle))
            (let ((ok (fli:with-foreign-string (command element-count byte-count
                                                :external-format :unicode)
                          (argv->command-line program args)
                        (declare (ignore element-count byte-count))
                        (%create-process fli:*null-pointer* command
                                         fli:*null-pointer* fli:*null-pointer*
                                         t (logior +create-no-window+
                                                   +below-normal-priority-class+)
                                         fli:*null-pointer* fli:*null-pointer*
                                         startup process-info))))
              (unless ok
                (error "could not start ~a (Windows error ~d)"
                       program (%get-last-error)))
              ;; The child inherited its own copies; drop ours.
              (%close-handle stdin-read)
              (when stderr-handle (%close-handle stderr-handle))
              (make-ffmpeg-capture
               :process-handle (fli:foreign-slot-value process-info 'process)
               :thread-handle (fli:foreign-slot-value process-info 'thread)
               :stdin-write stdin-write
               :pid (fli:foreign-slot-value process-info 'pid)
               :stderr-path (and stderr-handle stderr-path))))
        (error (condition)
          (%close-handle stdin-read)
          (%close-handle stdin-write)
          (when stderr-handle (%close-handle stderr-handle))
          (error condition))))))

(defun close-capture-handles (capture)
  (%close-handle (ffmpeg-capture-stdin-write capture))
  (%close-handle (ffmpeg-capture-thread-handle capture))
  (%close-handle (ffmpeg-capture-process-handle capture)))

(defun capture-alive-p (capture)
  (multiple-value-bind (ok code)
      (%get-exit-code-process (ffmpeg-capture-process-handle capture) 0)
    (and ok (= code +still-active+))))

(defun ffmpeg-available-p ()
  "T when RESOLVE-FFMPEG-PATH points at something that starts. Used by
the GUI to validate the recording checkbox; -version exits on its own."
  (handler-case
      (let ((capture (spawn-process (resolve-ffmpeg-path) (list "-version"))))
        (close-capture-handles capture)
        t)
    (error () nil)))

;;; Hardware-encoder probe. Runs once, in the background, at startup;
;;; the winner lands in *HW-VIDEO-ENCODER* (recording.lisp) and every
;;; later capture uses it. Probing at capture start instead would
;;; delay the recording past the run's start trigger.

(defparameter +hw-probe-timeout-seconds+ 15
  "Per-encoder cap on the probe encode. A usable encoder finishes the
8 black frames in well under a second; an unusable one fails to open
even faster - the margin only covers a cold ffmpeg start under load.")

(defun probe-hw-encoder ()
  "First +HW-ENCODER-CANDIDATES+ entry this machine's ffmpeg can
actually encode with, or NIL when only libx264 remains. The second
value is the probe's confidence: :DONE when at least one candidate got
ffmpeg running (so a NIL really means \"no hardware encoder here\"),
:SPAWN-FAILED when ffmpeg itself never started - an external block
\(Smart App Control's \"Windows error 4551\" on a fresh install) that
can lift later, so a :SPAWN-FAILED NIL is provisional."
  (let ((ffmpeg (resolve-ffmpeg-path))
        (backend (make-instance 'win32-ffmpeg-backend))
        (spawned nil))
    (dolist (encoder +hw-encoder-candidates+
                     (values nil (if spawned :done :spawn-failed)))
      (handler-case
          (let ((capture (spawn-process ffmpeg
                                        (hw-encoder-probe-args encoder))))
            (setf spawned t)
            (unwind-protect
                 (progn
                   (wait-for-capture backend capture
                                     +hw-probe-timeout-seconds+)
                   (when (backend-capture-succeeded-p backend capture)
                     (return (values encoder :done))))
              (backend-close-capture backend capture)))
        (error (condition)
          (recording-log "hw encoder probe ~a failed to spawn: ~a"
                         encoder condition)
          nil)))))

(defun probe-hw-gpu-chain ()
  "T when the zero-copy fullscreen capture chain works on this machine
\(see HW-GPU-CHAIN-PROBE-ARGS). Only worth asking once the encoder
probe picked h264_qsv; the probe grabs a few frames of the primary
desktop, which every interactive session allows."
  (let ((backend (make-instance 'win32-ffmpeg-backend)))
    (handler-case
        (let ((capture (spawn-process (resolve-ffmpeg-path)
                                      (hw-gpu-chain-probe-args))))
          (unwind-protect
               (progn
                 (wait-for-capture backend capture
                                   +hw-probe-timeout-seconds+)
                 (backend-capture-succeeded-p backend capture))
            (backend-close-capture backend capture)))
      (error (condition)
        (recording-log "gpu chain probe failed to spawn: ~a" condition)
        nil))))

(defvar *hw-probe-process* nil
  "The worker thread of the probe currently in flight, so the capture
-start retry (START-RECORDING on a :SPAWN-FAILED verdict) never stacks
a second probe on a slow first one.")

(defun start-hw-encoder-probe ()
  "Probe in a worker thread and publish the result. Called from MAIN at
startup and again from START-RECORDING while the verdict is
:SPAWN-FAILED; until it lands, captures fall back to libx264 (see
*HW-VIDEO-ENCODER*). A QSV winner is probed further for the zero-copy
fullscreen chain (*HW-FULLSCREEN-GPU-CHAIN*). No-op while a probe is
already running."
  (unless (and *hw-probe-process* (mp:process-alive-p *hw-probe-process*))
    (setf *hw-probe-process*
          (mp:process-run-function
           "eta-hw-encoder-probe" '()
           (lambda ()
             (multiple-value-bind (encoder state)
                 (handler-case (probe-hw-encoder)
                   ;; RESOLVE-FFMPEG-PATH signaling (no ffmpeg at all)
                   ;; lands here; treat it like a spawn failure so a
                   ;; later repair (say, an update restoring the file)
                   ;; is picked up by the capture-start retry.
                   (error (condition)
                     (recording-log "hw encoder probe failed: ~a" condition)
                     (values nil :spawn-failed)))
               (setf *hw-video-encoder* encoder
                     *hw-encoder-probe-state* state)
               (recording-log "hw encoder probe: using ~a~:[~; (provisional - ffmpeg would not start)~]"
                              (or encoder "libx264 (no hardware encoder)")
                              (eq state :spawn-failed))
               (when (equal encoder "h264_qsv")
                 (setf *hw-fullscreen-gpu-chain* (probe-hw-gpu-chain))
                 (recording-log "gpu chain probe: fullscreen captures ~:[keep the hwdownload fallback~;stay on the GPU (hwmap -> vpp_qsv)~]"
                                *hw-fullscreen-gpu-chain*))))))))

;;; Capture-monitor resolution. An exclusive-fullscreen PSOBB renders
;;; past GDI, so a gdigrab capture records black frames (field-observed
;;; on a Boot Camp machine, 2026-07-07) - and so does a windowed PSOBB
;;; whose presentation Windows composites via the flip model (run 949's
;;; all-black recordings on Windows 11, 2026-07-16). ddagrab (Desktop
;;; Duplication) reads the composited monitor and sees both, so
;;; BACKEND-CAPTURE-MONITOR routes every capture through it: whole
;;; monitor when the window covers it (RECT-COVERS-P - borderless
;;; matches too), cropped to the client area otherwise. gdigrab remains
;;; only as the fallback when the monitor's DXGI output index or the
;;; window's client rect cannot be resolved.

(fli:define-c-struct win-rect
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

;; MONITORINFOEXW: MONITORINFO plus the GDI device name
;; ("\\.\DISPLAYn"), which maps the monitor to a ddagrab output index
;; (DISPLAY-DEVICE-OUTPUT-INDEX).
(fli:define-c-struct monitorinfoex
  (cb-size (:unsigned :long))
  (rc-monitor (:struct win-rect))
  (rc-work (:struct win-rect))
  (flags (:unsigned :long))
  (device (:c-array (:unsigned :short) 32)))

(fli:define-foreign-function (%get-window-rect "GetWindowRect")
    ((hwnd :pointer)
     (rect (:pointer (:struct win-rect))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-c-struct win-point
  (x :long)
  (y :long))

(fli:define-foreign-function (%get-client-rect "GetClientRect")
    ((hwnd :pointer)
     (rect (:pointer (:struct win-rect))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%client-to-screen "ClientToScreen")
    ((hwnd :pointer)
     (point (:pointer (:struct win-point))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%monitor-from-window "MonitorFromWindow")
    ((hwnd :pointer)
     (flags (:unsigned :long)))
  :result-type :pointer
  :calling-convention :stdcall
  :module :user32)

(fli:define-foreign-function (%get-monitor-info "GetMonitorInfoW")
    ((monitor :pointer)
     (info (:pointer (:struct monitorinfoex))))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :user32)

(defconstant +monitor-defaulttonearest+ 2)

(defun rect-list (rect)
  "(left top right bottom) from a WIN-RECT pointer."
  (list (fli:foreign-slot-value rect 'left)
        (fli:foreign-slot-value rect 'top)
        (fli:foreign-slot-value rect 'right)
        (fli:foreign-slot-value rect 'bottom)))

(defun monitor-device-name (info)
  "The szDevice field of a MONITORINFOEX pointer as a string."
  (let ((device (fli:foreign-slot-pointer info 'device)))
    (with-output-to-string (out)
      (loop :for i :from 0 :below 32
            :for code := (fli:foreign-aref device i)
            :until (zerop code)
            :do (write-char (code-char code) out)))))

;;; DXGI output lookup. ddagrab picks its monitor by DXGI output index
;;; on the default adapter, and DXGI's enumeration order has nothing to
;;; do with GDI's DISPLAYn numbering: the old n-1 guess captured a
;;; neighboring monitor - someone's Discord - on the first multi-monitor
;;; field machine (run 1047, 2026-07-17). A wrong monitor is a privacy
;;; leak, not a recoverable recording, so the index now comes from
;;; enumerating the default adapter's outputs (the same set ddagrab
;;; sees) and matching DXGI_OUTPUT_DESC.DeviceName - the same
;;; \\.\DISPLAYn string MONITORINFOEX hands us. No match, no ddagrab.

(fli:register-module :dxgi :real-name "dxgi" :connection-style :automatic)

(fli:define-c-struct dxgi-output-desc
  (device-name (:c-array (:unsigned :short) 32))
  (desktop-coordinates (:struct win-rect))
  (attached-to-desktop (:boolean :int))
  (rotation :int)
  (monitor :pointer))

(fli:define-foreign-function (%create-dxgi-factory1 "CreateDXGIFactory1")
    ((riid :pointer)
     (factory (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :dxgi)

;; Lazily built (see the audio-win32 note: load-time foreign memory
;; does not survive delivery). IID_IDXGIFactory, not Factory1: this
;; machine's CreateDXGIFactory1 answers E_NOINTERFACE for the Factory1
;; IID, and EnumAdapters/EnumOutputs/GetDesc are plain-Factory methods
;; anyway.
(defvar *iid-idxgifactory* nil)

(defun iid-idxgifactory ()
  (or *iid-idxgifactory*
      (setf *iid-idxgifactory*
            (make-guid #x7B7166EC #x21C7 #x44AE
                       '(#xB2 #x1A #xC9 #xAE #x32 #x1A #xE3 #x69)))))

;; EnumAdapters, EnumOutputs and GetDesc all sit at vtable slot 7 of
;; their interfaces (IUnknown 0-2 + IDXGIObject 3-6 come first).
(fli:define-foreign-funcallable com-call-enum-child
    ((this :pointer)
     (index (:unsigned :int))
     (child (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-output-get-desc
    ((this :pointer)
     (desc (:pointer (:struct dxgi-output-desc))))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(defun dxgi-desc-device-name (desc)
  (let ((chars (fli:foreign-slot-pointer desc 'device-name)))
    (with-output-to-string (out)
      (loop :for i :from 0 :below 32
            :for code := (fli:foreign-aref chars i)
            :until (zerop code)
            :do (write-char (code-char code) out)))))

(defparameter +dxgi-max-outputs+ 8
  "Enumeration cap; EnumOutputs returns DXGI_ERROR_NOT_FOUND well
before this on any real machine.")

(defun dxgi-output-index-for-device (device-name)
  "The default adapter's DXGI output index whose DeviceName is
DEVICE-NAME (\"\\\\.\\DISPLAYn\"), or NIL when DXGI enumeration fails
or no output matches - including a monitor driven by another adapter,
which ddagrab (a default-adapter D3D11 device) could not capture
anyway. A failed lookup logs what WAS there, because that list is the
whole diagnosis on a remote machine."
  (multiple-value-bind (hr factory)
      (%create-dxgi-factory1 (iid-idxgifactory) 0)
    (when (and (zerop hr) (not (fli:null-pointer-p factory)))
      (unwind-protect
           (multiple-value-bind (hr adapter)
               (com-call-enum-child (com-method factory 7) factory 0 0)
             (when (and (zerop hr) (not (fli:null-pointer-p adapter)))
               (unwind-protect
                    (fli:with-dynamic-foreign-objects
                        ((desc (:struct dxgi-output-desc)))
                      (let ((seen '()))
                        (loop :for index :from 0 :below +dxgi-max-outputs+
                              :do (multiple-value-bind (hr output)
                                      (com-call-enum-child
                                       (com-method adapter 7) adapter index 0)
                                    (when (or (not (zerop hr))
                                              (fli:null-pointer-p output))
                                      (recording-log "capture check: no DXGI output named ~s (adapter 0 has ~{~s~^, ~})"
                                                     device-name (nreverse seen))
                                      (return nil))
                                    (unwind-protect
                                         (when (zerop (com-call-output-get-desc
                                                       (com-method output 7)
                                                       output desc))
                                           (let ((name (dxgi-desc-device-name desc)))
                                             (push name seen)
                                             (when (string= name device-name)
                                               (return index))))
                                      (com-release output))))))
                 (com-release adapter))))
        (com-release factory)))))

(defun window-client-screen-rect (hwnd)
  "(left top right bottom) of HWND's client area in screen coordinates
\(what gdigrab title= would capture: no borders, no caption), or NIL
when the queries fail."
  (fli:with-dynamic-foreign-objects ((rect win-rect)
                                     (point win-point))
    (when (%get-client-rect hwnd rect)
      (let ((width (fli:foreign-slot-value rect 'right))
            (height (fli:foreign-slot-value rect 'bottom)))
        (setf (fli:foreign-slot-value point 'x) 0
              (fli:foreign-slot-value point 'y) 0)
        (when (%client-to-screen hwnd point)
          (let ((left (fli:foreign-slot-value point 'x))
                (top (fli:foreign-slot-value point 'y)))
            (list left top (+ left width) (+ top height))))))))

(defun psobb-capture-monitor ()
  "Plist (:output-idx :width :height [:crop (x y w h)]) of the monitor
the PSOBB window sits on, or NIL when the window is absent or the
monitor/crop cannot be resolved (BUILD-FFMPEG-ARGS then falls back to
gdigrab). A window covering the monitor gets the bare monitor plist (a
fullscreen capture, and the dimensions size the GPU-side scale); a
smaller window gets its client area as a monitor-relative :CROP.
Every outcome logs its inputs: a wrong verdict here is exactly how a
capture silently records black (gdigrab on a surface GDI cannot see -
run 949), and the log is all a remote diagnosis has to go on."
  (let ((hwnd (find-psobb-window)))
    (if (null hwnd)
        (recording-log "capture check: no PSOBB window")
        (fli:with-dynamic-foreign-objects ((rect win-rect)
                                           (info monitorinfoex))
          (setf (fli:foreign-slot-value info 'cb-size)
                (fli:size-of '(:struct monitorinfoex)))
          (let ((monitor (%monitor-from-window hwnd
                                               +monitor-defaulttonearest+)))
            (if (not (and (not (fli:null-pointer-p monitor))
                          (%get-window-rect hwnd rect)
                          (%get-monitor-info monitor info)))
                (recording-log "capture check: window/monitor query failed")
                (let* ((window-rect (rect-list rect))
                       (monitor-rect (rect-list (fli:foreign-slot-pointer
                                                 info 'rc-monitor)))
                       (device (monitor-device-name info))
                       (covers (rect-covers-p window-rect monitor-rect))
                       ;; DXGI name match, never the DISPLAYn-1 guess:
                       ;; the guess put run 1047's capture on the wrong
                       ;; (Discord) monitor. DXGI-OUTPUT-INDEX-FOR-DEVICE
                       ;; logs the outputs it saw when nothing matches.
                       (index (ignore-errors
                               (dxgi-output-index-for-device device))))
                  (recording-log "capture check: window=~a monitor=~a (~a) covers=~a dxgi-idx=~a"
                                 window-rect monitor-rect device covers index)
                  (destructuring-bind (left top right bottom) monitor-rect
                    (cond
                      ((null index)
                       (recording-log "capture check: monitor unresolvable in DXGI, staying on gdigrab")
                       nil)
                      (covers
                       (list :output-idx index
                             :width (- right left)
                             :height (- bottom top)))
                      (t
                       ;; Windowed: monitor capture cropped to the
                       ;; client area, because GDI cannot read a
                       ;; flip-model-composited window (run 949).
                       (let ((client (window-client-screen-rect hwnd)))
                         (multiple-value-bind (x y width height)
                             (and client
                                  (capture-crop-rect client monitor-rect))
                           (if (null x)
                               (progn
                                 (recording-log "capture check: unusable client rect ~a, staying on gdigrab"
                                                client)
                                 nil)
                               (list :output-idx index
                                     :width (- right left)
                                     :height (- bottom top)
                                     :crop (list x y width height)))))))))))))))

;;; The live backend

(defclass win32-ffmpeg-backend () ())

(defparameter +recording-log-max-bytes+ (* 1024 1024)
  "Rotation threshold for the recording log. The log is append-only
across sessions and now also receives ffmpeg stderr transcripts, so an
unrotated file would grow without bound; one file of history plus a
.old generation is plenty for diagnostics.")

(defun rotate-recording-log ()
  "Move an oversized recording log aside (overwriting the previous
generation) so the live file stays small enough to read whole."
  (let ((path (recording-log-path)))
    (when (> (or (ignore-errors
                   (with-open-file (s path :element-type '(unsigned-byte 8))
                     (file-length s)))
                 0)
             +recording-log-max-bytes+)
      (uiop:rename-file-overwriting-target
       path (make-pathname :type "old" :defaults path)))))

(defun recording-log (fmt &rest args)
  "Append a timestamped line to %TEMP%\\ephinea-ta-recording.log. The
GUI can only show a one-line error label; capture-start failures in
the field (e.g. \"pointer out of memory bounds\" right after a game
crash, 2026-07-06) need the full story to be diagnosable after the
fact. Never signals."
  (ignore-errors
    (rotate-recording-log)
    (with-open-file (s (recording-log-path)
                       :direction :output :if-exists :append
                       :if-does-not-exist :create
                       :external-format :utf-8)
      (multiple-value-bind (sec min hour day month)
          (decode-universal-time (get-universal-time))
        (format s "~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d "
                month day hour min sec))
      (apply #'format s fmt args)
      (terpri s))))

(defun log-session-info ()
  "One line of machine context at startup, so any recording-log tail
\(the unit DIAGNOSTICS-REPORT ships to the server) identifies the
client build and hardware class it came from."
  (recording-log "session: client ~a, ~a ~a, ram-gb ~a, cores ~a, ffmpeg ~a"
                 (client-version)
                 (ignore-errors (software-type))
                 (ignore-errors (software-version))
                 (let ((bytes (physical-memory-bytes)))
                   (and bytes (round bytes (expt 2 30))))
                 (logical-processor-count)
                 (ignore-errors (resolve-ffmpeg-path))))

(defmethod backend-capture-monitor ((backend win32-ffmpeg-backend))
  (let ((monitor (ignore-errors (psobb-capture-monitor))))
    (when monitor
      (recording-log "capturing via ddagrab output_idx=~d (~dx~d)~@[ crop=~a~]"
                     (getf monitor :output-idx)
                     (getf monitor :width) (getf monitor :height)
                     (getf monitor :crop)))
    monitor))

(defmethod backend-start-capture ((backend win32-ffmpeg-backend)
                                  ffmpeg-path args output-path
                                  &key audio-pipe audio-pid)
  (handler-case
      (progn
        (ensure-directories-exist output-path)
        ;; The pipe server end must exist before ffmpeg opens its
        ;; inputs. Any audio-side failure - returning NIL or signaling
        ;; (a COM/FLI error right after a game crash was seen in the
        ;; field) - drops the audio arguments and records video-only
        ;; rather than fail the capture; otherwise point ffmpeg at the
        ;; session's actual capture format.
        (let ((audio (and audio-pipe
                          (handler-case
                              (start-audio-session audio-pipe audio-pid)
                            (error (condition)
                              (recording-log
                               "audio session failed (video-only fallback), pid=~a: ~a"
                               audio-pid condition)
                              nil)))))
          (setf args
                (if audio
                    (retarget-audio-args
                     args
                     :sample-format (audio-session-sample-format audio)
                     :rate (audio-session-rate audio)
                     :channels (audio-session-channels audio))
                    (strip-audio-args args audio-pipe)))
          (handler-case
              (let ((capture (spawn-process ffmpeg-path args
                                            :stderr-path (stderr-file-for
                                                          output-path))))
                (setf (ffmpeg-capture-audio capture) audio)
                ;; The full argv, because remote diagnosis of a bad
                ;; recording starts from what ffmpeg was actually told
                ;; (which grab, which filter chain, which encoder).
                (recording-log "capture argv: ~a"
                               (argv->command-line ffmpeg-path args))
                (recording-log "capture started: audio=~a"
                               (and audio (audio-session-scope audio)))
                capture)
            (error (condition)
              (when audio (stop-audio-session audio))
              (error condition)))))
    (error (condition)
      (recording-log "capture start FAILED: ~a~%  ffmpeg=~a output=~a pid=~a"
                     condition ffmpeg-path output-path audio-pid)
      (values nil (format nil "~a" condition)))))

(defmethod backend-capture-alive-p ((backend win32-ffmpeg-backend) capture)
  (capture-alive-p capture))

(defun stderr-file-for (output-path)
  "Where a spawned ffmpeg's stderr goes: next to its output, so
concurrent processes (a remux finishing while the next capture starts)
never share a file."
  (concatenate 'string (namestring output-path) ".stderr.txt"))

(defmethod backend-start-remux ((backend win32-ffmpeg-backend)
                                ffmpeg-path args)
  ;; The remux ffmpeg reads no stdin, but SPAWN-PROCESS's pipe is
  ;; harmless and keeps the capture token uniform.
  (handler-case (spawn-process ffmpeg-path args
                               :stderr-path (stderr-file-for
                                             (first (last args))))
    (error (condition)
      (recording-log "remux start FAILED: ~a" condition)
      (values nil (format nil "~a" condition)))))

(defmethod backend-capture-succeeded-p ((backend win32-ffmpeg-backend) capture)
  (multiple-value-bind (ok code)
      (%get-exit-code-process (ffmpeg-capture-process-handle capture) 0)
    (and ok (zerop code))))

(defun write-quit (capture)
  ;; A lone "q" on stdin makes ffmpeg finish the output cleanly. Two
  ;; bytes always fit the pipe buffer, so this never blocks.
  (fli:with-dynamic-foreign-objects ()
    (let ((buffer (fli:allocate-dynamic-foreign-object
                   :type '(:unsigned :byte) :nelems 2)))
      (setf (fli:dereference buffer :index 0) (char-code #\q)
            (fli:dereference buffer :index 1) (char-code #\Newline))
      (%write-file (ffmpeg-capture-stdin-write capture) buffer 2 0
                   fli:*null-pointer*))))

(defmethod backend-request-stop ((backend win32-ffmpeg-backend) capture)
  ;; End the audio stream first: closing the pipe is the audio EOF
  ;; ffmpeg needs. ffmpeg reads the piped audio a couple of seconds
  ;; behind real time, and "q" makes it stop reading at once - so wait
  ;; (off-thread; the poll loop must not block) for the buffered tail
  ;; to drain before quitting, or the last seconds of audio are lost.
  (let ((audio (ffmpeg-capture-audio capture)))
    (cond (audio
           (stop-audio-session audio)
           (mp:process-run-function
            "eta-ffmpeg-stop" '()
            (lambda ()
              (sleep +audio-drain-seconds+)
              (ignore-errors (write-quit capture)))))
          (t (write-quit capture)))))

(defmethod backend-kill-capture ((backend win32-ffmpeg-backend) capture)
  (when (ffmpeg-capture-audio capture)
    (stop-audio-session (ffmpeg-capture-audio capture)))
  (%terminate-process (ffmpeg-capture-process-handle capture) 1))

(defparameter +stderr-transcript-chars+ 8192
  "How much of a dead ffmpeg's stderr file survives into the recording
log. At -loglevel error the file is empty on a good run; when it is
not, the head repeats one complaint and the tail has the fatal one.")

(defun transcribe-capture-stderr (capture)
  "Fold the tail of the child's stderr file into the recording log and
drop the file. Runs at close, when the process is dead and the file is
complete; a clean run leaves it empty (-loglevel error) and logs
nothing. Never signals - this is diagnostics, not control flow."
  (ignore-errors
    (let ((path (ffmpeg-capture-stderr-path capture)))
      (when path
        (let ((tail (file-tail path +stderr-transcript-chars+)))
          (when (and tail (find-if (lambda (char) (char> char #\Space)) tail))
            (recording-log "ffmpeg stderr (~a):~%~a" path tail)))
        (uiop:delete-file-if-exists path)
        (setf (ffmpeg-capture-stderr-path capture) nil)))))

(defmethod backend-close-capture ((backend win32-ffmpeg-backend) capture)
  ;; Idempotent; also reached when ffmpeg died on its own, where the
  ;; capture thread must not be left serving a dead pipe.
  (when (ffmpeg-capture-audio capture)
    (stop-audio-session (ffmpeg-capture-audio capture)))
  (transcribe-capture-stderr capture)
  (close-capture-handles capture))

(defmethod backend-rename-file ((backend win32-ffmpeg-backend) from to)
  (uiop:rename-file-overwriting-target from to))

(defmethod backend-delete-file ((backend win32-ffmpeg-backend) path)
  (uiop:delete-file-if-exists path))

(defmethod backend-list-stale-files ((backend win32-ffmpeg-backend) dir)
  (when (probe-file dir)
    (mapcar #'namestring (uiop:directory-files dir "rec-tmp-*.mp4"))))

(defmethod backend-list-recordings ((backend win32-ffmpeg-backend) dir)
  (when (probe-file dir)
    (loop :for path :in (uiop:directory-files dir "*.mp4")
          ;; Skip the in-progress work files; only the kept, run-named
          ;; recordings count against the budget.
          :unless (uiop:string-prefix-p "rec-tmp-" (pathname-name path))
            :collect (list (namestring path)
                           (or (ignore-errors
                                 (with-open-file (s path
                                                    :element-type '(unsigned-byte 8))
                                   (file-length s)))
                               0)
                           (or (ignore-errors (file-write-date path)) 0)))))
