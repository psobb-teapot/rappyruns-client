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

(defconstant +player0-base+ #x00500000)
(defconstant +player1-base+ #x00510000)
(defconstant +quest-base+ #x00700000)
(defconstant +quest-data-base+ #x00710000)
(defconstant +register-base+ #x00720000)

(defun make-player-block (&key name (class-id 0) (floor 0) (warping nil) (pb 0.0))
  (let ((bytes (make-array #xE60 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (put-utf16 bytes #x428 (format nil "~aE~a" #\Tab name))
    (put-u16 bytes #x960 (ash class-id 8))
    (put-u16 bytes #x3F0 floor)
    (put-u16 bytes #x33E (if warping #x04 0))
    (put-f32 bytes #x520 pb)
    bytes))

(defun make-game-regions (&key (episode-raw 0) players quest-name quest-number
                               register-values)
  "Full mock memory image. PLAYERS is a list of player block byte vectors;
REGISTER-VALUES an alist of (register-id . value)."
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
    (push (cons #x00A9C4F4 globals) regions)          ; my player index = 0
    (push (cons #x00A9B1C8 episode) regions)
    (push (cons #x00A94254 player-array) regions)
    (push (cons #x00A95AA8 quest-ptr) regions)
    (push (cons #x00AC9FA0 (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                                 :initial-element 0))
          regions)
    (apply #'make-mock-reader regions)))

;;; ------------------------------------------------------------------
;;; Memory decoding + snapshot parsing
;;; ------------------------------------------------------------------

(defun run-memory-tests ()
  (format t "~&--- memory/snapshot ---~%")
  (let ((reader (make-mock-reader
                 (cons 100 (coerce #(1 2 3 4 #x78 #x56 #x34 #x12) 'vector)))))
    (check "read-u8" (= 1 (read-u8 reader 100)))
    (check "read-u16 little-endian" (= #x0201 (read-u16 reader 100)))
    (check "read-u32 little-endian" (= #x12345678 (read-u32 reader 104)))
    (check "read out of range -> NIL" (null (read-u16 reader 200))))
  (check "f32 decode 12.5"
         (< (abs (- 12.5 (ephinea-ta-client::u32-float #x41480000))) 0.001))
  (let* ((reader (make-game-regions
                  :episode-raw 0
                  :players (list (make-player-block :name "Ryu" :class-id 2 :floor 5)
                                 (make-player-block :name "Elly" :class-id 8 :floor 5))
                  :quest-name "Towards the Future"
                  :quest-number 118
                  :register-values '((12 . 1))))
         (snapshot (read-snapshot reader)))
    (check "snapshot episode" (= 1 (getf snapshot :episode)))
    (check "snapshot quest name"
           (equal "Towards the Future" (getf snapshot :quest-name)))
    (check "snapshot quest number" (= 118 (getf snapshot :quest-number)))
    (check "snapshot two players" (= 2 (length (getf snapshot :players))))
    (check "player name decoded (\\tE stripped)"
           (equal "Ryu" (getf (first (getf snapshot :players)) :name)))
    (check "player classes decoded"
           (equal '("HUcast" "FOnewearl")
                  (mapcar (lambda (p) (getf p :class)) (getf snapshot :players))))
    (check "register 12 set" (snapshot-register-set-p snapshot 12))
    (check "register 254 clear" (not (snapshot-register-set-p snapshot 254)))
    (check "floor switch clear" (not (snapshot-floor-switch-set-p snapshot 4 99)))))

;;; ------------------------------------------------------------------
;;; Detection state machine (driven through mock memory images)
;;; ------------------------------------------------------------------

(defun ttf-reader (&key (start 0) (end 0) (seg 0) (pb 0.0))
  (make-game-regions
   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1 :pb pb)
                  (make-player-block :name "Elly" :class-id 8 :floor 1))
   :quest-name "Towards the Future" :quest-number 118
   :register-values (list (cons 12 start) (cons 254 end) (cons 100 seg))))

(defun lobby-reader ()
  (make-game-regions
   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 0))))

(defun step-with (detector reader)
  (detector-step detector (read-snapshot reader)))

(defun run-detect-tests ()
  (format t "~&--- detect ---~%")
  ;; Full TTF flow: lobby -> quest loaded -> start -> finish.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (check "idle after lobby" (eq :idle (detector-state detector)))
    (step-with detector (ttf-reader))
    (check "still idle before start register" (eq :idle (detector-state detector)))
    (step-with detector (ttf-reader :start 1))
    (check "in-quest after start register" (eq :in-quest (detector-state detector)))
    (sleep 0.05)
    (check "no run before end register"
           (null (step-with detector (ttf-reader :start 1))))
    (sleep 0.05)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "run emitted on end register" (not (null run)))
      (check "run slug" (equal "ep1-towards-the-future" (getf run :quest-slug)))
      (check "run time >= 100ms" (>= (getf run :time-ms) 100))
      (check "run party" (= 2 (getf run :party-size)))
      (check "run players"
             (equal '(("Ryu" . "HUcast") ("Elly" . "FOnewearl"))
                    (mapcar (lambda (p) (cons (getf p :name) (getf p :class)))
                            (getf run :players))))
      (check "run not PB" (not (getf run :pb)))
      (check "detector reset after run" (eq :idle (detector-state detector))))
    ;; Triggers stay set after completion while the quest is loaded; the
    ;; detector must not re-start (and re-submit) the same run.
    (step-with detector (ttf-reader :start 1 :end 1))
    (check "no restart while completed quest stays loaded"
           (eq :idle (detector-state detector)))
    ;; Back through the lobby re-arms; a fresh take of the quest starts.
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "re-armed after lobby visit"
           (eq :in-quest (detector-state detector))))
  ;; Attaching mid-quest must not start a run (not armed).
  (let ((detector (make-detector)))
    (step-with detector (ttf-reader :start 1))
    (check "mid-quest attach stays idle" (eq :idle (detector-state detector))))
  ;; A charged gauge / cast Shifta at the start is NOT enough for PB - a
  ;; normal No-PB run often starts that way. It must be No PB.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1 :pb 80.0))
    (sleep 0.02)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1 :pb 80.0)))))
      (check "charged PB at start alone stays No PB" (null (getf run :pb)))))
  ;; Actually discharging a Photon Blast mid-run (gauge drop > 50) is PB.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1 :pb 90.0))
    (step-with detector (ttf-reader :start 1 :pb 2.0))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "PB discharge -> PB category" (eq t (getf run :pb)))))
  ;; A quest run that never discharges is No PB, even with Shifta up
  ;; (the segment tests below and this one cover the common case).
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "no discharge -> No PB" (null (getf run :pb)))))
  ;; A segment definition sharing the quest is tracked in parallel with
  ;; the full clear: one run through the quest yields both records.
  (let* ((segment (ephinea-ta-client::make-quest-def
                   :slug "ep1-towards-the-future-2-rooms" :episode 1
                   :names '("Towards the Future") :number 118
                   :start '(:register 12) :end '(:register 100)))
         (ephinea-ta-client::*quest-defs*
           (cons segment ephinea-ta-client::*quest-defs*))
         (detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "segment: both trackers active"
           (= 2 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (ttf-reader :start 1 :seg 1))))
      (check "segment run emitted first"
             (equal '("ep1-towards-the-future-2-rooms")
                    (mapcar (lambda (run) (getf run :quest-slug)) runs))))
    (check "full clear still running"
           (= 1 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (ttf-reader :start 1 :seg 1 :end 1))))
      (check "full clear emitted second"
             (equal '("ep1-towards-the-future")
                    (mapcar (lambda (run) (getf run :quest-slug)) runs)))
      (check "full clear time > segment possible"
             (>= (getf (first runs) :time-ms) 100)))
    (check "segment does not restart while quest loaded"
           (null (step-with detector (ttf-reader :start 1 :seg 1 :end 1)))))
  ;; Warp-in quests start when a player leaves Pioneer 2.
  (let ((detector (make-detector))
        (en1-p2 (make-game-regions
                 :players (list (make-player-block :name "Ryu" :class-id 2 :floor 0))
                 :quest-name "Endless Nightmare #1" :quest-number 108))
        (en1-in (make-game-regions
                 :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1))
                 :quest-name "Endless Nightmare #1" :quest-number 108
                 :register-values '((30 . 1)))))
    (step-with detector (lobby-reader))
    (step-with detector en1-p2)
    (check "warp-in: idle on Pioneer 2" (eq :idle (detector-state detector)))
    ;; Same frame reaches the end register here; start frame first:
    (step-with detector en1-in)
    (check "warp-in: started once on the field"
           (eq :in-quest (detector-state detector)))
    (let ((run (first (step-with detector en1-in))))
      (check "warp-in quest completes" (equal "ep1-endless-nightmare-1"
                                              (getf run :quest-slug)))))
  ;; Unknown quests never start.
  (let ((detector (make-detector))
        (unknown (make-game-regions
                  :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1))
                  :quest-name "Gallon's Shop" :quest-number 9999
                  :register-values '((12 . 1)))))
    (step-with detector (lobby-reader))
    (step-with detector unknown)
    (check "unknown quest stays idle" (eq :idle (detector-state detector))))
  ;; Losing the game or reloading the quest voids the run.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (detector-step detector nil)
    (check "game gone -> idle" (eq :idle (detector-state detector)))))

;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; Server-defined detection categories (GET /api/quests -> quest-def)
;;; ------------------------------------------------------------------

(defun api-quest (&rest keys-and-values)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on keys-and-values :by #'cddr
          :do (setf (gethash key table) value))
    table))

(defun floor-switch-json (floor switch)
  (api-quest "type" "floor-switch" "floor" floor "switch" switch))

(defun run-server-defs-tests ()
  (format t "~&--- server-defined categories ---~%")
  (load-quest-defs)
  (let ((builtin-count (length ephinea-ta-client::*builtin-quest-defs*)))
    ;; A moderator-created "GDV reset": ep2 quest 944, ends at floor 5 sw 2.
    (let ((quests (vector
                   (api-quest "slug" "ep2-gdv-reset" "episode" 2
                              "game_number" 944
                              "start" (floor-switch-json 5 0)
                              "end" (floor-switch-json 5 2))
                   ;; A display-only entry (no triggers) is ignored.
                   (api-quest "slug" "ep1-some-catalog-quest" "episode" 1))))
      (check "set-server-quest-defs counts only timeable entries"
             (= 1 (set-server-quest-defs quests)))
      (check "server def merged into active defs"
             (find "ep2-gdv-reset" ephinea-ta-client::*quest-defs*
                   :key #'quest-def-slug :test #'equal))
      (check "builtin defs still present after merge"
             (= (1+ builtin-count) (length ephinea-ta-client::*quest-defs*)))
      (let ((def (find "ep2-gdv-reset" ephinea-ta-client::*quest-defs*
                       :key #'quest-def-slug :test #'equal)))
        (check "server def start trigger converted"
               (equal '(:floor-switch 5 0) (quest-def-start def)))
        (check "server def end trigger converted"
               (equal '(:floor-switch 5 2) (quest-def-end def)))
        (check "server def keeps game number" (eql 944 (quest-def-number def)))))
    ;; Re-fetching replaces server defs without duplicating.
    (set-server-quest-defs (vector))
    (check "empty refetch drops server defs, keeps builtin"
           (= builtin-count (length ephinea-ta-client::*quest-defs*)))))

;;; A GDV reset (server-defined) tracked alongside the full GDV clear.
(defun gdv-reader (&key (start 0) (room2 0) (full 0))
  "GDV = Maximum Attack E: Gal Da Val, ep2 quest 944. Start = floor 5
switch 0; room 2 cleared = floor 5 switch 2; full clear = register 254."
  (let ((switches (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
    ;; floor 5 switch 0 and switch 2 live in the floor-5 block.
    (flet ((set-switch (floor switch)
             (let ((offset (+ (* 32 floor) (floor switch 8)))
                   (mask (ash #x80 (- (mod switch 8)))))
               (setf (aref switches offset)
                     (logior (aref switches offset) mask)))))
      (when (plusp start) (set-switch 5 0))
      (when (plusp room2) (set-switch 5 2)))
    (let ((reader (make-game-regions
                   :episode-raw 1  ; raw 1 -> episode 2
                   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 5))
                   :quest-name "Maximum Attack E: Gal Da Val" :quest-number 944
                   :register-values (list (cons 254 full)))))
      ;; Overlay our crafted floor-switch block.
      (push (cons #x00AC9FA0 switches)
            (ephinea-ta-client::mock-reader-regions reader))
      reader)))

(defun run-gdv-segment-test ()
  (format t "~&--- GDV reset alongside full clear ---~%")
  (load-quest-defs)
  (set-server-quest-defs
   (vector (api-quest "slug" "ep2-gdv-reset" "episode" 2 "game_number" 944
                      "start" (floor-switch-json 5 0)
                      "end" (floor-switch-json 5 2))))
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (gdv-reader))                ; loaded, not started
    (step-with detector (gdv-reader :start 1))       ; start switch set
    (check "GDV: both full-clear and reset tracked"
           (= 2 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (gdv-reader :start 1 :room2 1))))
      (check "GDV reset emitted at room 2"
             (equal '("ep2-gdv-reset")
                    (mapcar (lambda (r) (getf r :quest-slug)) runs))))
    (check "GDV full clear still running"
           (= 1 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (gdv-reader :start 1 :room2 1 :full 1))))
      (check "GDV full clear emitted at register 254"
             (equal '("ep2-maximum-attack-e-gal-da-val")
                    (mapcar (lambda (r) (getf r :quest-slug)) runs)))))
  (set-server-quest-defs (vector)))

(defun run-trigger-log-tests ()
  (format t "~&--- trigger log ---~%")
  (ephinea-ta-client::close-trigger-log)
  (let ((path (ephinea-ta-client::trigger-log-path)))
    (ignore-errors (delete-file path))
    ;; Enabling logging must create the file immediately (before any
    ;; trigger changes), so the user can see it is working.
    (start-trigger-log)
    (check "start-trigger-log creates the file at once" (probe-file path))
    ;; A floor switch flipping between two frames is recorded.
    (let* ((clear (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0))
           (set-2 (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
      ;; floor 5 switch 2 on in the second frame.
      (setf (aref set-2 (+ (* 32 5) 0)) (ash #x80 (- 2)))
      (let ((prev (list :quest-name "GDV" :quest-ptr 1 :floor-switches clear
                        :registers (make-array 1024 :element-type '(unsigned-byte 8)
                                                     :initial-element 0)))
            (next (list :quest-name "GDV" :quest-ptr 1 :floor-switches set-2
                        :registers (make-array 1024 :element-type '(unsigned-byte 8)
                                                     :initial-element 0))))
        (check "diff records the flipped switch"
               (= 1 (log-trigger-changes prev next)))))
    (ephinea-ta-client::close-trigger-log)
    (let ((text (with-output-to-string (out)
                  (with-open-file (in path :external-format :utf-8)
                    (loop :for line := (read-line in nil nil)
                          :while line :do (format out "~a~%" line))))))
      (check "log names floor 5 switch 2"
             (search "floor 5 switch 2" text)))
    (ignore-errors (delete-file path))))

(defun run-client-tests ()
  (setf *failures* 0)
  (load-quest-defs)
  (run-memory-tests)
  (run-detect-tests)
  (run-server-defs-tests)
  (run-gdv-segment-test)
  (run-trigger-log-tests)
  (format t "~&=== client tests: ~d failure~:p ===~%" *failures*)
  *failures*)
