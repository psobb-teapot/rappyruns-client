;;; Pure-CL tests for the ephinea-ta client: memory decoding, snapshot
;;; parsing and the quest detection state machine. Runs on SBCL or
;;; LispWorks; no game and no server required.
;;;
;;; Load :ephinea-ta-client first, then this file; it defines and runs
;;; RUN-CLIENT-TESTS, incrementing *CLIENT-TEST-FAILURES* on failure.

(defpackage :ephinea-ta-client-tests
  (:use :cl :ephinea-ta-client))
(in-package :ephinea-ta-client-tests)

(defvar *failures* 0)

(defmacro check (label form)
  `(handler-case
       (if ,form
           (format t "~&PASS ~a~%" ,label)
           (progn (incf *failures*) (format t "~&FAIL ~a~%" ,label)))
     (error (e)
       (incf *failures*)
       (format t "~&FAIL ~a (error: ~a)~%" ,label e))))

;;; ------------------------------------------------------------------
;;; Mock memory image builders
;;; ------------------------------------------------------------------

(defun put-u16 (bytes offset value)
  (setf (aref bytes offset) (ldb (byte 8 0) value)
        (aref bytes (+ offset 1)) (ldb (byte 8 8) value)))

(defun put-u32 (bytes offset value)
  (put-u16 bytes offset (ldb (byte 16 0) value))
  (put-u16 bytes (+ offset 2) (ldb (byte 16 16) value)))

(defun put-utf16 (bytes offset string)
  (loop :for char :across string
        :for i :from offset :by 2
        :do (put-u16 bytes i (char-code char))))

(defun put-f32 (bytes offset float)
  "Encode a small non-negative float (enough for PB gauge values)."
  (put-u32 bytes offset
           (if (zerop float)
               0
               (let* ((expo (floor (log float 2)))
                      (mant (round (* (1- (/ float (expt 2 expo))) (expt 2 23)))))
                 (logior (ash (+ expo 127) 23) mant)))))

(defun put-f64 (bytes offset value)
  "Encode a positive normal double (enough for the Ephinea HP scale)."
  (multiple-value-bind (mant expo sign)
      (integer-decode-float (float value 1d0))
    (declare (ignore sign))
    (let ((bits (logior (ash (+ expo 52 1023) 52) (ldb (byte 52 0) mant))))
      (loop :for i :below 8
            :do (setf (aref bytes (+ offset i)) (ldb (byte 8 (* 8 i)) bits))))))

(defconstant +player0-base+ #x00500000)
(defconstant +player1-base+ #x00510000)
(defconstant +quest-base+ #x00700000)
(defconstant +quest-data-base+ #x00710000)
(defconstant +register-base+ #x00720000)

(defun make-player-block (&key name (class-id 0) (floor 0) (warping nil) (pb 0.0)
                               (section-id 0) (level-raw 0) (room 0) (state 1)
                               (hp 0) (max-hp 0) (tp 0) (max-tp 0) (meseta 0)
                               guild-card)
  (let ((bytes (make-array #xE60 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (put-utf16 bytes #x428 (format nil "~aE~a" #\Tab name))
    (put-u16 bytes #x960 (logior (ash class-id 8) section-id))
    (put-u16 bytes #x3F0 floor)
    (put-u16 bytes #x33E (if warping #x04 0))
    (put-f32 bytes #x520 pb)
    (put-u16 bytes #x028 room)
    (put-u16 bytes #x348 state)
    (put-u16 bytes #x2BC max-hp)
    (put-u16 bytes #x2BE max-tp)
    (put-u16 bytes #x334 hp)
    (put-u16 bytes #x336 tp)
    (put-u16 bytes #xE44 level-raw)
    (put-u32 bytes #xE4C meseta)
    (when guild-card
      (loop :for char :across guild-card
            :for i :from #x930
            :do (setf (aref bytes i) (char-code char))))
    bytes))

(defun make-game-regions (&key (episode-raw 0) players quest-name quest-number
                               register-values (difficulty 0) (map 0) hp-scale)
  "Full mock memory image. PLAYERS is a list of player block byte vectors;
REGISTER-VALUES an alist of (register-id . value). HP-SCALE, when given,
publishes the Ephinea HP table pointer + scale double."
  (let ((globals (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (episode (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0))
        (player-array (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0))
        (quest-ptr (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (regions '()))
    (put-u16 episode 0 episode-raw)
    (loop :for player :in players
          :for i :from 0
          :for base := (+ #x00500000 (* i #x10000))
          :do (put-u32 player-array (* 4 i) base)
              (push (cons base player) regions))
    (when quest-name
      (let ((quest (make-array #x200 :element-type '(unsigned-byte 8) :initial-element 0))
            (data (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
            (registers (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0)))
        (put-u32 quest-ptr 0 +quest-base+)
        (put-u32 quest #x19C +quest-data-base+)
        (put-u32 quest #x2C +register-base+)
        (put-u16 data #x10 (or quest-number 0))
        (put-utf16 data #x18 quest-name)
        (loop :for (id . value) :in register-values
              :do (put-u16 registers (* 4 id) value))
        (push (cons +quest-base+ quest) regions)
        (push (cons +quest-data-base+ data) regions)
        (push (cons +register-base+ registers) regions)))
    (let ((difficulty-bytes (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-element 0))
          (map-bytes (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
      (put-u16 difficulty-bytes 0 difficulty)
      (put-u16 map-bytes 0 map)
      (push (cons #x00A9CD68 difficulty-bytes) regions)
      (push (cons #x00AAFC9C map-bytes) regions))
    (when hp-scale
      (let ((ephinea (make-array 12 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
        (put-u32 ephinea 0 #x00CAFE00)  ; non-zero table pointer
        (put-f64 ephinea 4 hp-scale)
        (push (cons #x00B5F800 ephinea) regions)))
    (push (cons #x00A9C4F4 globals) regions)          ; my player index = 0
    (push (cons #x00A9B1C8 episode) regions)
    (push (cons #x00A94254 player-array) regions)
    (push (cons #x00A95AA8 quest-ptr) regions)
    (push (cons #x00AC9FA0 (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                                 :initial-element 0))
          regions)
    (apply #'make-mock-reader regions)))

;;; The suites live in sibling files (split from the original single
;;; file, T21), loaded here in the original order; each starts with
;;; IN-PACKAGE :EPHINEA-TA-CLIENT-TESTS.
(dolist (part '("tests-memory" "tests-detect" "tests-quests"
                "tests-recorder" "tests-helpers" "tests-misc"
                "tests-ghost"))
  (load (merge-pathnames (concatenate 'string part ".lisp")
                         (or *load-truename* *default-pathname-defaults*))))

(defun run-client-tests ()
  (setf *failures* 0)
  (load-quest-defs)
  (run-i18n-tests)
  (run-credentials-tests)
  (run-signature-policy-tests)
  (run-memory-tests)
  (run-inventory-tests)
  (run-extended-player-tests)
  (run-telemetry-tests)
  (run-monster-read-tests)
  (run-psostats-telemetry-tests)
  (run-payload-tests)
  (run-detect-tests)
  (run-anguish-tests)
  (run-monster-clear-tests)
  (run-detect-telemetry-tests)
  (run-server-defs-tests)
  (run-gdv-segment-test)
  (run-trigger-log-tests)
  (run-quest-rule-tests)
  (run-rule-form-tests)
  (run-pure-helper-tests)
  (run-room-picker-tests)
  (run-recorder-tests)
  (run-diagnostics-tests)
  (run-video-flow-tests)
  (run-upload-queue-tests)
  (run-retention-tests)
  (run-ux-helper-tests)
  (run-updater-tests)
  (run-config-migration-tests)
  (run-ghost-tests)
  (format t "~&=== client tests: ~d failure~:p ===~%" *failures*)
  *failures*)
