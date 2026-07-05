(in-package :ephinea-ta-client)

;;; LispWorks-only: audio capture for recordings. ffmpeg has no native
;;; Windows loopback input, so the client captures the audio itself and
;;; serves the raw PCM to ffmpeg over a named pipe as a second input.
;;;
;;; Preferred method: the Windows process-loopback API (Windows 10
;;; 2004+) scoped to the PSOBB process tree - ONLY the game is heard,
;;; never Discord or notification sounds. Fallback when that activation
;;; fails (older Windows): WASAPI endpoint loopback, i.e. everything
;;; the default playback device plays.
;;;
;;; Two field-debugging lessons encoded here (see also volumedetect
;;; numbers in the repo history):
;;; - Both loopback flavors observe the signal AFTER the Windows volume
;;;   mixer. A low per-app slider (5% observed in the field) makes a
;;;   perfectly working capture look like "no audio at all"; that is
;;;   why recordings are loudness-normalized (ffmpeg loudnorm, in the
;;;   post-capture remux - never live, see BUILD-FFMPEG-ARGS) and
;;;   why capture requests float32 - boosting a quiet float mix loses
;;;   nothing, unlike 16-bit.
;;; - The COM interfaces are called through their raw vtables with FLI;
;;;   the one COM callback process loopback requires
;;;   (IActivateAudioInterfaceCompletionHandler) is a hand-built vtable
;;;   of foreign callables that just signals a Win32 event.
;;;
;;; Failure policy: if every WASAPI path fails, the capture thread
;;; keeps serving SILENCE at the right rate, so ffmpeg still gets a
;;; well-formed audio stream and the video is never lost.

;;; Win32 plumbing (kernel32 is registered in win32.lisp)

(fli:register-module :ole32 :real-name "ole32" :connection-style :automatic)
(fli:register-module :mmdevapi :real-name "mmdevapi" :connection-style :automatic)

(fli:define-foreign-function (%co-initialize-ex "CoInitializeEx")
    ((reserved :pointer)
     (concurrency-model (:unsigned :long)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :ole32)

(fli:define-foreign-function (%co-uninitialize "CoUninitialize")
    ()
  :result-type :void
  :calling-convention :stdcall
  :module :ole32)

(fli:define-foreign-function (%co-create-instance "CoCreateInstance")
    ((clsid :pointer)
     (outer :pointer)
     (context (:unsigned :long))
     (riid :pointer)
     (object (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :ole32)

(fli:define-foreign-function (%co-task-mem-free "CoTaskMemFree")
    ((pointer :pointer))
  :result-type :void
  :calling-convention :stdcall
  :module :ole32)

(fli:define-foreign-function (%activate-audio-interface-async
                              "ActivateAudioInterfaceAsync")
    ((device-path (:reference-pass :ef-wc-string))
     (riid :pointer)
     (activation-params :pointer)
     (completion-handler :pointer)
     (operation (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :mmdevapi)

(fli:define-foreign-function (%create-event "CreateEventW")
    ((attributes :pointer)
     (manual-reset (:boolean :int))
     (initial-state (:boolean :int))
     (name :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%set-event "SetEvent")
    ((event :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%wait-for-single-object "WaitForSingleObject")
    ((handle :pointer)
     (timeout-ms (:unsigned :long)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%flush-file-buffers "FlushFileBuffers")
    ((handle :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%create-named-pipe "CreateNamedPipeW")
    ((name (:reference-pass :ef-wc-string))
     (open-mode (:unsigned :long))
     (pipe-mode (:unsigned :long))
     (max-instances (:unsigned :long))
     (out-buffer-size (:unsigned :long))
     (in-buffer-size (:unsigned :long))
     (default-timeout (:unsigned :long))
     (security-attributes :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%connect-named-pipe "ConnectNamedPipe")
    ((pipe :pointer)
     (overlapped :pointer))
  :result-type (:boolean :int)
  :calling-convention :stdcall
  :module :kernel32)

(fli:define-foreign-function (%create-file "CreateFileW")
    ((name (:reference-pass :ef-wc-string))
     (desired-access (:unsigned :long))
     (share-mode (:unsigned :long))
     (security-attributes :pointer)
     (creation-disposition (:unsigned :long))
     (flags (:unsigned :long))
     (template :pointer))
  :result-type :pointer
  :calling-convention :stdcall
  :module :kernel32)

(defconstant +pipe-access-outbound+ 2)
(defconstant +generic-read+ #x80000000)
(defconstant +open-existing+ 3)
(defconstant +error-pipe-connected+ 535)
(defconstant +wait-object-0+ 0)
(defconstant +coinit-multithreaded+ 0)
(defconstant +clsctx-all+ #x17)
(defconstant +invalid-handle+ -1)      ; INVALID_HANDLE_VALUE as address

(defun invalid-handle-p (handle)
  (or (fli:null-pointer-p handle)
      (= (fli:pointer-address handle)
         (ldb (byte 64 0) +invalid-handle+))))

(defun hresult-failed-p (hresult)
  (logbitp 31 hresult))

;;; GUIDs as 16 raw bytes (Data1/2/3 little-endian, Data4 verbatim).

(defun make-guid (d1 d2 d3 d4-list)
  (let ((bytes (fli:allocate-foreign-object :type '(:unsigned :byte)
                                            :nelems 16)))
    (loop :for i :from 0 :below 4
          :do (setf (fli:dereference bytes :index i) (ldb (byte 8 (* 8 i)) d1)))
    (loop :for i :from 0 :below 2
          :do (setf (fli:dereference bytes :index (+ 4 i))
                    (ldb (byte 8 (* 8 i)) d2)))
    (loop :for i :from 0 :below 2
          :do (setf (fli:dereference bytes :index (+ 6 i))
                    (ldb (byte 8 (* 8 i)) d3)))
    (loop :for byte :in d4-list
          :for i :from 8
          :do (setf (fli:dereference bytes :index i) byte))
    bytes))

;; GUID pointers are allocated LAZILY at run time, never at load time:
;; foreign memory allocated while the delivered exe is being built does
;; not survive into the running image - the saved pointers would
;; dangle, every COM call would get a garbage riid, and the audio path
;; would silently fail (observed in the field as all-zero recordings
;; from the exe while the dev image worked).

(defvar *iid-iunknown* nil)
(defvar *iid-iagileobject* nil)
(defvar *iid-iactivate-completion-handler* nil)
(defvar *clsid-mmdeviceenumerator* nil)
(defvar *iid-immdeviceenumerator* nil)
(defvar *iid-iaudioclient* nil)
(defvar *iid-iaudiocaptureclient* nil)

(defun ensure-audio-guids ()
  (unless *iid-iunknown*
    (setf *iid-iunknown*
          (make-guid #x00000000 #x0000 #x0000
                     '(#xC0 #x00 #x00 #x00 #x00 #x00 #x00 #x46))
          *iid-iagileobject*
          (make-guid #x94EA2B94 #xE9CC #x49E0
                     '(#xC0 #xFF #xEE #x64 #xCA #x8F #x5B #x90))
          *iid-iactivate-completion-handler*
          (make-guid #x41D949AB #x9862 #x444A
                     '(#x80 #xF6 #xC2 #x61 #x33 #x4D #xA5 #xEB))
          *clsid-mmdeviceenumerator*
          (make-guid #xBCDE0395 #xE52F #x467C
                     '(#x8E #x3D #xC4 #x57 #x92 #x91 #x69 #x2E))
          *iid-immdeviceenumerator*
          (make-guid #xA95664D2 #x9614 #x4F35
                     '(#xA7 #x46 #xDE #x8D #xB6 #x36 #x17 #xE6))
          *iid-iaudioclient*
          (make-guid #x1CB9AD4C #xDBFA #x4C32
                     '(#xB1 #x78 #xC2 #xF5 #x68 #xA7 #x03 #xB2))
          *iid-iaudiocaptureclient*
          (make-guid #xC8ADBD64 #xE71E #x48A0
                     '(#xA4 #xDE #x18 #x5C #x39 #x5C #xD3 #x17)))))

(defun guid-equal-p (a b)
  (loop :for i :from 0 :below 16
        :always (= (fli:dereference a :type '(:unsigned :byte) :index i)
                   (fli:dereference b :type '(:unsigned :byte) :index i))))

;;; Indirect (vtable) calls. Each COM object is a pointer to a pointer
;;; to an array of function pointers; these funcallables make the call
;;; once the slot has been fetched with COM-METHOD.

(defun com-method (object index)
  (let ((vtable (fli:dereference object :type :pointer)))
    (fli:dereference vtable :type :pointer :index index)))

(fli:define-foreign-funcallable com-call-release
    ((this :pointer))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-activate-result
    ((this :pointer)
     (activate-hresult (:reference-return (:unsigned :long)))
     (activated-interface (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-default-audio-endpoint
    ((this :pointer)
     (data-flow :int)
     (role :int)
     (endpoint (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-device-activate
    ((this :pointer)
     (riid :pointer)
     (cls-context (:unsigned :long))
     (activation-params :pointer)
     (object (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-mix-format
    ((this :pointer)
     (format (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-initialize
    ((this :pointer)
     (share-mode :int)
     (stream-flags (:unsigned :long))
     (buffer-duration :long-long)
     (periodicity :long-long)
     (format :pointer)
     (session-guid :pointer))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-set-event-handle
    ((this :pointer)
     (event :pointer))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-no-args
    ((this :pointer))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-service
    ((this :pointer)
     (riid :pointer)
     (service (:reference-return :pointer)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-buffer
    ((this :pointer)
     (data (:reference-return :pointer))
     (frames (:reference-return (:unsigned :long)))
     (flags (:reference-return (:unsigned :long)))
     (device-position :pointer)
     (qpc-position :pointer))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-release-buffer
    ((this :pointer)
     (frames (:unsigned :long)))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(fli:define-foreign-funcallable com-call-get-next-packet-size
    ((this :pointer)
     (frames (:reference-return (:unsigned :long))))
  :result-type (:unsigned :long)
  :calling-convention :stdcall)

(defun com-release (object)
  (when (and object (not (fli:null-pointer-p object)))
    (ignore-errors (com-call-release (com-method object 2) object))))

;;; The completion handler process-loopback activation requires: a
;;; static COM object whose ActivateCompleted just signals
;;; *ACTIVATION-EVENT*. Never freed, refcount is a lie.

(defconstant +e-nointerface+ #x80004002)

(defvar *activation-event* nil
  "Auto-reset event signalled by the completion handler. Activations
are serialized by *AUDIO-ACTIVATION-LOCK*, so one event is enough.")

(defvar *audio-activation-lock* (mp:make-lock :name "eta-audio-activation"))

(fli:define-foreign-callable ("eta_handler_query_interface"
                              :result-type (:unsigned :long)
                              :calling-convention :stdcall)
    ((this :pointer) (riid :pointer) (out :pointer))
  (cond ((or (guid-equal-p riid *iid-iunknown*)
             (guid-equal-p riid *iid-iagileobject*)
             (guid-equal-p riid *iid-iactivate-completion-handler*))
         (setf (fli:dereference out :type :pointer) this)
         0)
        (t
         (setf (fli:dereference out :type :pointer) fli:*null-pointer*)
         +e-nointerface+)))

(fli:define-foreign-callable ("eta_handler_add_ref"
                              :result-type (:unsigned :long)
                              :calling-convention :stdcall)
    ((this :pointer))
  (declare (ignore this))
  1)

(fli:define-foreign-callable ("eta_handler_release"
                              :result-type (:unsigned :long)
                              :calling-convention :stdcall)
    ((this :pointer))
  (declare (ignore this))
  1)

(fli:define-foreign-callable ("eta_handler_activate_completed"
                              :result-type (:unsigned :long)
                              :calling-convention :stdcall)
    ((this :pointer) (operation :pointer))
  (declare (ignore this operation))
  (when *activation-event*
    (%set-event *activation-event*))
  0)

(defvar *completion-handler* nil
  "Pointer to the static handler object: one vtable pointer whose
vtable holds the four callables above.")

(defun ensure-completion-handler ()
  (unless *completion-handler*
    (let ((vtable (fli:allocate-foreign-object :type :pointer :nelems 4))
          (object (fli:allocate-foreign-object :type :pointer)))
      (setf (fli:dereference vtable :index 0)
            (fli:make-pointer :symbol-name "eta_handler_query_interface"
                              :functionp t)
            (fli:dereference vtable :index 1)
            (fli:make-pointer :symbol-name "eta_handler_add_ref" :functionp t)
            (fli:dereference vtable :index 2)
            (fli:make-pointer :symbol-name "eta_handler_release" :functionp t)
            (fli:dereference vtable :index 3)
            (fli:make-pointer :symbol-name "eta_handler_activate_completed"
                              :functionp t))
      (setf (fli:dereference object) vtable)
      (setf *completion-handler* object)))
  (unless *activation-event*
    (setf *activation-event*
          (%create-event fli:*null-pointer* nil nil fli:*null-pointer*)))
  *completion-handler*)

;;; Audio formats and activation

(defconstant +activation-type-process-loopback+ 1)
(defconstant +loopback-mode-include-tree+ 0)
(defconstant +vt-blob+ 65)
(defconstant +edataflow-render+ 0)
(defconstant +erole-multimedia+ 1)
(defconstant +audclnt-shared+ 0)
(defconstant +audclnt-streamflags-loopback+ #x00020000)
(defconstant +audclnt-streamflags-eventcallback+ #x00040000)
(defconstant +audclnt-bufferflags-silent+ 2)
(defconstant +wave-format-pcm+ 1)
(defconstant +wave-format-ieee-float+ 3)
(defconstant +wave-format-extensible+ #xFFFE)

;; The format we request from process loopback. Float32 keeps full
;; precision of the (mixer-attenuated) signal for loudnorm to boost.
(defparameter +audio-rate+ 48000)
(defconstant +audio-channels+ 2)

(fli:define-c-struct waveformatex
  (format-tag (:unsigned :short))
  (channels (:unsigned :short))
  (samples-per-sec (:unsigned :long))
  (avg-bytes-per-sec (:unsigned :long))
  (block-align (:unsigned :short))
  (bits-per-sample (:unsigned :short))
  (cb-size (:unsigned :short)))

;; AUDIOCLIENT_ACTIVATION_PARAMS with the process-loopback member.
(fli:define-c-struct activation-params
  (activation-type (:unsigned :long))
  (target-pid (:unsigned :long))
  (loopback-mode (:unsigned :long)))

;; PROPVARIANT carrying a VT_BLOB (x64 layout: pointer lands at offset 16).
(fli:define-c-struct propvariant-blob
  (vt (:unsigned :short))
  (reserved1 (:unsigned :short))
  (reserved2 (:unsigned :short))
  (reserved3 (:unsigned :short))
  (blob-size (:unsigned :long))
  (blob-data :pointer))

(defun fill-float-format (format rate channels)
  (setf (fli:foreign-slot-value format 'format-tag) +wave-format-ieee-float+
        (fli:foreign-slot-value format 'channels) channels
        (fli:foreign-slot-value format 'samples-per-sec) rate
        (fli:foreign-slot-value format 'avg-bytes-per-sec)
        (* rate channels 4)
        (fli:foreign-slot-value format 'block-align) (* channels 4)
        (fli:foreign-slot-value format 'bits-per-sample) 32
        (fli:foreign-slot-value format 'cb-size) 0))

(defun setup-capture-client (client format stream-flags event)
  "Initialize CLIENT, wire EVENT if given, get the capture service and
Start. Returns the IAudioCaptureClient or signals."
  (let ((hr (com-call-initialize (com-method client 3) client
                                 +audclnt-shared+ stream-flags
                                 2000000 0 format fli:*null-pointer*)))
    (when (hresult-failed-p hr)
      (error "IAudioClient::Initialize failed (#x~x)" hr)))
  (when event
    (com-call-set-event-handle (com-method client 13) client event))
  (multiple-value-bind (hr service)
      (com-call-get-service (com-method client 14) client
                            *iid-iaudiocaptureclient* 0)
    (when (hresult-failed-p hr)
      (error "GetService(IAudioCaptureClient) failed (#x~x)" hr))
    (com-call-no-args (com-method client 10) client) ; Start
    service))

(defun activate-process-loopback (pid event)
  "IAudioClient capturing only PID's process tree (float32 stereo at
+AUDIO-RATE+), started and ready to drain; returns (values client
capture-client). Caller must hold *AUDIO-ACTIVATION-LOCK* and have COM
initialized."
  (ensure-completion-handler)
  (fli:with-dynamic-foreign-objects
      ((params (:struct activation-params))
       (prop (:struct propvariant-blob))
       (format (:struct waveformatex)))
    (setf (fli:foreign-slot-value params 'activation-type)
          +activation-type-process-loopback+
          (fli:foreign-slot-value params 'target-pid) pid
          (fli:foreign-slot-value params 'loopback-mode)
          +loopback-mode-include-tree+)
    (setf (fli:foreign-slot-value prop 'vt) +vt-blob+
          (fli:foreign-slot-value prop 'reserved1) 0
          (fli:foreign-slot-value prop 'reserved2) 0
          (fli:foreign-slot-value prop 'reserved3) 0
          (fli:foreign-slot-value prop 'blob-size)
          (fli:size-of '(:struct activation-params))
          (fli:foreign-slot-value prop 'blob-data)
          (fli:copy-pointer params :type '(:unsigned :byte)))
    (fill-float-format format +audio-rate+ +audio-channels+)
    (multiple-value-bind (hresult operation)
        (%activate-audio-interface-async "VAD\\Process_Loopback"
                                         *iid-iaudioclient*
                                         prop *completion-handler* 0)
      (when (hresult-failed-p hresult)
        (error "ActivateAudioInterfaceAsync failed (#x~x)" hresult))
      (unwind-protect
          (progn
            (unless (= +wait-object-0+
                       (%wait-for-single-object *activation-event* 3000))
              (error "audio activation timed out"))
            (multiple-value-bind (hresult2 activate-hresult client)
                (com-call-get-activate-result (com-method operation 3)
                                              operation 0 0)
              (when (or (hresult-failed-p hresult2)
                        (hresult-failed-p activate-hresult))
                (error "audio activation failed (#x~x / #x~x)"
                       hresult2 activate-hresult))
              (handler-case
                  (values client
                          (setup-capture-client
                           client format
                           (logior +audclnt-streamflags-loopback+
                                   +audclnt-streamflags-eventcallback+)
                           event))
                (error (condition)
                  (com-release client)
                  (error condition)))))
        (com-release operation)))))

(defun mix-format-sample-format (format-pointer)
  "ffmpeg -f name for an endpoint mix format, or NIL if unsupported."
  (let* ((wf (fli:copy-pointer format-pointer
                               :type '(:struct waveformatex)))
         (tag (fli:foreign-slot-value wf 'format-tag))
         (bits (fli:foreign-slot-value wf 'bits-per-sample)))
    (cond ((and (= bits 32)
                (or (= tag +wave-format-ieee-float+)
                    (= tag +wave-format-extensible+)))
           "f32le")
          ((and (= bits 16)
                (or (= tag +wave-format-pcm+)
                    (= tag +wave-format-extensible+)))
           "s16le")
          (t nil))))

(defun activate-endpoint-loopback ()
  "Fallback: IAudioClient in loopback mode on the default render
endpoint (all system audio), polled, in the endpoint's own mix format.
Returns (values client capture-client rate channels frame-bytes
sample-format) or signals. Caller must have COM initialized."
  (multiple-value-bind (hr enumerator)
      (%co-create-instance *clsid-mmdeviceenumerator* fli:*null-pointer*
                           +clsctx-all+ *iid-immdeviceenumerator* 0)
    (when (hresult-failed-p hr)
      (error "CoCreateInstance(MMDeviceEnumerator) failed (#x~x)" hr))
    (let (device client capture-client format
          rate channels frame-bytes sample-format)
      (handler-case
          (progn
            (multiple-value-bind (hr2 dev)
                (com-call-get-default-audio-endpoint
                 (com-method enumerator 4) enumerator
                 +edataflow-render+ +erole-multimedia+ 0)
              (when (hresult-failed-p hr2)
                (error "no default render device (#x~x)" hr2))
              (setf device dev))
            (multiple-value-bind (hr2 ac)
                (com-call-device-activate (com-method device 3) device
                                          *iid-iaudioclient* +clsctx-all+
                                          fli:*null-pointer* 0)
              (when (hresult-failed-p hr2)
                (error "IMMDevice::Activate(IAudioClient) failed (#x~x)" hr2))
              (setf client ac))
            (multiple-value-bind (hr2 fmt)
                (com-call-get-mix-format (com-method client 8) client 0)
              (when (hresult-failed-p hr2)
                (error "GetMixFormat failed (#x~x)" hr2))
              (setf format fmt))
            (let ((wf (fli:copy-pointer format :type '(:struct waveformatex))))
              (setf rate (fli:foreign-slot-value wf 'samples-per-sec)
                    channels (fli:foreign-slot-value wf 'channels)
                    frame-bytes (fli:foreign-slot-value wf 'block-align)
                    sample-format (mix-format-sample-format format))
              (unless sample-format
                (error "unsupported mix format (tag ~a, ~a bits)"
                       (fli:foreign-slot-value wf 'format-tag)
                       (fli:foreign-slot-value wf 'bits-per-sample))))
            (setf capture-client
                  (setup-capture-client client format
                                        +audclnt-streamflags-loopback+ nil)))
        (error (condition)
          (com-release client)
          (com-release device)
          (com-release enumerator)
          (when format (%co-task-mem-free format))
          (error condition)))
      (com-release device)
      (com-release enumerator)
      (%co-task-mem-free format)
      (values client capture-client rate channels frame-bytes sample-format))))

;;; The capture session: a thread that owns the pipe server end and
;;; feeds ffmpeg. Wall-clock pacing tops the stream up with silence so
;;; the audio track always matches the video timeline, including when
;;; WASAPI packets stop or every activation failed.

(defstruct audio-session
  pipe-name
  pipe-handle
  thread
  start-time    ; pacing epoch: set at creation, just before ffmpeg spawns
  ;; capture objects and format; activation happens synchronously in
  ;; START-AUDIO-SESSION so ffmpeg's argv can match the real format
  client capture-client event
  (scope :none) ; :game | :desktop | :none - what the capture hears
  (rate 48000)
  (channels 2)
  (frame-bytes 8)
  (sample-format "f32le")
  (stop-flag nil)
  ;; diagnostics
  connect-ms    ; how long ffmpeg took to open the pipe
  (frames-written 0)
  (packets 0)      ; WASAPI packets received
  (data-frames 0)  ; frames received without the SILENT flag
  error)

(defun audio-pipe-write (session pointer bytes)
  "WriteFile to the pipe; NIL once the reader (ffmpeg) is gone."
  (multiple-value-bind (ok written)
      (%write-file (audio-session-pipe-handle session) pointer bytes 0
                   fli:*null-pointer*)
    (and ok (= written bytes))))

(defun write-silence (session frames zeros zeros-frames)
  (loop :while (plusp frames)
        :do (let ((chunk (min frames zeros-frames)))
              (unless (audio-pipe-write
                       session zeros
                       (* chunk (audio-session-frame-bytes session)))
                (return nil))
              (decf frames chunk))
        :finally (return t)))

(defun drain-packets (session zeros zeros-frames)
  "Forward every pending WASAPI packet to the pipe. Returns (values
ok-p frames-written)."
  (let ((capture-client (audio-session-capture-client session))
        (total 0))
    (loop
      (multiple-value-bind (hr pending)
          (com-call-get-next-packet-size (com-method capture-client 5)
                                         capture-client 0)
        (when (or (hresult-failed-p hr) (zerop pending))
          (return (values t total))))
      (multiple-value-bind (hr data frames flags)
          (com-call-get-buffer (com-method capture-client 3) capture-client
                               0 0 0 fli:*null-pointer* fli:*null-pointer*)
        (when (hresult-failed-p hr)
          (return (values t total)))
        (incf (audio-session-packets session))
        (unless (logtest flags +audclnt-bufferflags-silent+)
          (incf (audio-session-data-frames session) frames))
        (let ((ok (if (logtest flags +audclnt-bufferflags-silent+)
                      (write-silence session frames zeros zeros-frames)
                      (audio-pipe-write
                       session data
                       (* frames (audio-session-frame-bytes session))))))
          (com-call-release-buffer (com-method capture-client 4)
                                   capture-client frames)
          (unless ok (return (values nil total)))
          (incf total frames))))))

(defun audio-capture-loop (session)
  "Body of the capture thread: connect the pipe, then stream captured
audio (or silence when WASAPI is unavailable) until stopped."
  (%co-initialize-ex fli:*null-pointer* +coinit-multithreaded+)
  (unwind-protect
      (progn
        ;; ffmpeg may already have opened the pipe: 535 means connected.
        (unless (or (%connect-named-pipe (audio-session-pipe-handle session)
                                         fli:*null-pointer*)
                    (= (%get-last-error) +error-pipe-connected+))
          (error "ffmpeg never opened the audio pipe"))
        (setf (audio-session-connect-ms session)
              (round (* 1000 (- (get-internal-real-time)
                                (audio-session-start-time session)))
                     internal-time-units-per-second))
        (unless (audio-session-stop-flag session)
          (fli:with-dynamic-foreign-objects ()
            (let* ((rate (audio-session-rate session))
                   (zeros-frames (floor rate 10)) ; 100 ms
                   (zeros (fli:allocate-dynamic-foreign-object
                           :type '(:unsigned :byte)
                           :nelems (* zeros-frames
                                      (audio-session-frame-bytes session))
                           :initial-element 0))
                   ;; The pacing clock started when the session was
                   ;; created, just before ffmpeg spawned: ffmpeg
                   ;; normalizes every input to start at 0, so the time
                   ;; it spent opening its inputs becomes leading
                   ;; silence, keeping audio aligned with video.
                   (start (audio-session-start-time session))
                   (event (audio-session-event session))
                   (written 0))
              (loop :until (audio-session-stop-flag session)
                    ;; Process loopback delivers via its event;
                    ;; endpoint loopback events are unreliable without
                    ;; a render stream of our own, so that path polls.
                    :do (if event
                            (%wait-for-single-object event 50)
                            (mp:process-wait-with-timeout
                             "audio capture poll" 0.02
                             (lambda () (audio-session-stop-flag session))))
                        (when (audio-session-capture-client session)
                          (multiple-value-bind (ok frames)
                              (drain-packets session zeros zeros-frames)
                            (unless ok (return))
                            (incf written frames)))
                        ;; Top up with silence when the source goes
                        ;; quiet, so audio time tracks video time.
                        (let* ((elapsed (- (get-internal-real-time) start))
                               (expected (floor (* elapsed rate)
                                                internal-time-units-per-second))
                               (behind (- expected written)))
                          (when (> behind (floor rate 10))
                            (unless (write-silence session behind
                                                   zeros zeros-frames)
                              (return))
                            (incf written behind)))
                        (setf (audio-session-frames-written session)
                              written))))))
    (let ((client (audio-session-client session)))
      (when client
        (ignore-errors (com-call-no-args (com-method client 11) client)))) ; Stop
    (com-release (audio-session-capture-client session))
    (com-release (audio-session-client session))
    (when (audio-session-event session)
      (%close-handle (audio-session-event session)))
    ;; Closing a named pipe DISCARDS anything the reader has not
    ;; consumed yet; flush first so ffmpeg gets every buffered sample
    ;; before the EOF (returns immediately once the pipe is broken).
    (ignore-errors (%flush-file-buffers (audio-session-pipe-handle session)))
    (ignore-errors (%close-handle (audio-session-pipe-handle session)))
    (%co-uninitialize)))

(defun activate-session-capture (session pid)
  "Try game-only process loopback first, then the endpoint-mix
fallback; on total failure the session stays :none and serves silence."
  (handler-case
      (mp:with-lock (*audio-activation-lock*)
        (let ((event (%create-event fli:*null-pointer* nil nil
                                    fli:*null-pointer*)))
          (handler-case
              (multiple-value-bind (client capture-client)
                  (activate-process-loopback pid event)
                (setf (audio-session-client session) client
                      (audio-session-capture-client session) capture-client
                      (audio-session-event session) event
                      (audio-session-scope session) :game
                      (audio-session-rate session) +audio-rate+
                      (audio-session-channels session) +audio-channels+
                      (audio-session-frame-bytes session)
                      (* +audio-channels+ 4)
                      (audio-session-sample-format session) "f32le"))
            (error (condition)
              (%close-handle event)
              (error condition)))))
    (error (condition)
      (setf (audio-session-error session) (format nil "~a" condition))
      (handler-case
          (multiple-value-bind (client capture-client
                                rate channels frame-bytes sample-format)
              (activate-endpoint-loopback)
            (setf (audio-session-client session) client
                  (audio-session-capture-client session) capture-client
                  (audio-session-scope session) :desktop
                  (audio-session-rate session) rate
                  (audio-session-channels session) channels
                  (audio-session-frame-bytes session) frame-bytes
                  (audio-session-sample-format session) sample-format))
        (error (condition2)
          (setf (audio-session-error session)
                (format nil "~a; fallback: ~a" condition condition2)))))))

(defun start-audio-session (pipe-name pid)
  "Create the pipe server end, activate capture for PID's game audio
(synchronously, so the capture format is known before ffmpeg's argv is
finalized) and start the capture thread. Returns the session, or NIL
when the pipe cannot be created. WASAPI failure still returns a
session - it serves silence - so the video never depends on the audio
path."
  (let ((pipe (%create-named-pipe pipe-name +pipe-access-outbound+
                                  0        ; byte mode, blocking
                                  1 (* 1024 1024) 0 0 fli:*null-pointer*)))
    (unless (invalid-handle-p pipe)
      (let ((session (make-audio-session :pipe-name pipe-name
                                         :pipe-handle pipe
                                         :start-time (get-internal-real-time))))
        (ensure-audio-guids)
        ;; MTA COM objects may be used from the capture thread as well.
        (%co-initialize-ex fli:*null-pointer* +coinit-multithreaded+)
        (when pid
          (activate-session-capture session pid))
        (setf (audio-session-thread session)
              (mp:process-run-function
               "eta-audio-capture" '()
               (lambda ()
                 (handler-case (audio-capture-loop session)
                   (error (condition)
                     (setf (audio-session-error session)
                           (format nil "~a" condition))
                     (ignore-errors
                       (%close-handle (audio-session-pipe-handle session))))))))
        session))))

(defun stop-audio-session (session)
  "Ask the capture thread to finish; closing the pipe gives ffmpeg the
audio EOF it needs to finalize. Unblocks a thread still stuck waiting
for a reader by briefly connecting to the pipe ourselves."
  (when session
    (setf (audio-session-stop-flag session) t)
    (let ((client (%create-file (audio-session-pipe-name session)
                                +generic-read+ 0 fli:*null-pointer*
                                +open-existing+ 0 fli:*null-pointer*)))
      (unless (invalid-handle-p client)
        (%close-handle client)))))
