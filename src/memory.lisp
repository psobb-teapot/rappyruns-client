(in-package :ephinea-ta-client)

;;; Process memory access protocol. The live implementation (win32.lisp,
;;; LispWorks only) reads the PSOBB process; MOCK-READER serves tests and
;;; demo mode. All multi-byte values are little-endian (x86).

(defgeneric read-block (reader address size)
  (:documentation
   "Read SIZE bytes at ADDRESS. Returns an (unsigned-byte 8) vector,
or NIL when the memory is unreadable."))

(defgeneric reader-window-title (reader)
  (:documentation
   "Title of the game window this reader is attached to, or NIL. The
recorder needs it for ffmpeg's gdigrab title= input.")
  (:method (reader)
    (declare (ignore reader))
    nil))

(defclass mock-reader ()
  ((regions :initarg :regions :accessor mock-reader-regions
            :documentation "List of (address . byte-vector).")))

(defun make-mock-reader (&rest regions)
  "REGIONS are (address . byte-vector) conses."
  (make-instance 'mock-reader :regions regions))

(defmethod read-block ((reader mock-reader) address size)
  (loop :for (base . bytes) :in (mock-reader-regions reader)
        :when (and (>= address base)
                   (<= (+ address size) (+ base (length bytes))))
          :return (subseq bytes (- address base) (+ (- address base) size))))

;;; Decoding helpers on top of READ-BLOCK

(defun bytes-u16 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun bytes-u32 (bytes offset)
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun u32-float (bits)
  "Decode an IEEE 754 single float from its bit pattern.
Infinities and NaNs are clamped; precision suffices for comparisons."
  (let ((sign (if (logbitp 31 bits) -1 1))
        (expo (ldb (byte 8 23) bits))
        (mant (ldb (byte 23 0) bits)))
    (cond ((= expo 255) (* sign 3.4e38))
          ((zerop expo) (float (* sign mant (expt 2f0 -149)) 1f0))
          (t (float (* sign (1+ (/ mant (expt 2 23))) (expt 2f0 (- expo 127)))
                    1f0)))))

(defun read-u8 (reader address)
  (let ((bytes (read-block reader address 1)))
    (and bytes (aref bytes 0))))

(defun read-u16 (reader address)
  (let ((bytes (read-block reader address 2)))
    (and bytes (bytes-u16 bytes 0))))

(defun read-u32 (reader address)
  (let ((bytes (read-block reader address 4)))
    (and bytes (bytes-u32 bytes 0))))

(defun read-f32 (reader address)
  (let ((bits (read-u32 reader address)))
    (and bits (u32-float bits))))

(defun u64-double (bits)
  "Decode an IEEE 754 double float from its bit pattern.
Infinities and NaNs are clamped, like U32-FLOAT."
  (let ((sign (if (logbitp 63 bits) -1 1))
        (expo (ldb (byte 11 52) bits))
        (mant (ldb (byte 52 0) bits)))
    (cond ((= expo 2047) (* sign 1.7d308))
          ((zerop expo) (float (* sign mant (expt 2d0 -1074)) 1d0))
          (t (float (* sign (1+ (/ mant (expt 2 52))) (expt 2d0 (- expo 1023)))
                    1d0)))))

(defun read-f64 (reader address)
  (let ((bytes (read-block reader address 8)))
    (and bytes (u64-double (logior (bytes-u32 bytes 0)
                                   (ash (bytes-u32 bytes 4) 32))))))

(defun decode-utf16-z (bytes)
  "Decode a NUL-terminated UTF-16LE string from BYTES (BMP only)."
  (with-output-to-string (out)
    (loop :for i :from 0 :below (1- (length bytes)) :by 2
          :for code := (bytes-u16 bytes i)
          :until (zerop code)
          :do (write-char (code-char code) out))))

(defun read-utf16-string (reader address max-bytes)
  (let ((bytes (read-block reader address max-bytes)))
    (and bytes (decode-utf16-z bytes))))
