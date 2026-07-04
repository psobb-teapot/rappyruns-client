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
                               register-values (difficulty 0) (map 0))
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
    (let ((difficulty-bytes (make-array 2 :element-type '(unsigned-byte 8)
                                          :initial-element 0))
          (map-bytes (make-array 2 :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
      (put-u16 difficulty-bytes 0 difficulty)
      (put-u16 map-bytes 0 map)
      (push (cons #x00A9CD68 difficulty-bytes) regions)
      (push (cons #x00AAFC9C map-bytes) regions))
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
;;; Extended player stats (psostats parity fields)
;;; ------------------------------------------------------------------

(defun run-extended-player-tests ()
  (format t "~&--- extended player stats ---~%")
  (let* ((reader (make-game-regions
                  :difficulty 3
                  :map 5
                  :players (list (make-player-block
                                  :name "Ryu" :class-id 2 :floor 5
                                  :section-id 2 :level-raw 41 :room 7
                                  :state 4 :hp 945 :max-hp 1200
                                  :tp 300 :max-tp 400 :meseta 123456
                                  :guild-card "42001234"))))
         (snapshot (read-snapshot reader))
         (me (snapshot-my-player snapshot)))
    (check "difficulty in snapshot" (= 3 (getf snapshot :difficulty)))
    (check "difficulty name" (equal "Ultimate" (difficulty-name 3)))
    (check "map in snapshot" (= 5 (getf snapshot :map)))
    (check "section id decoded" (equal "Skyly" (getf me :section-id)))
    (check "level decoded (+1)" (= 42 (getf me :level)))
    (check "guild card decoded" (equal "42001234" (getf me :guild-card)))
    (check "room decoded" (= 7 (getf me :room)))
    (check "action state decoded" (= 4 (getf me :state)))
    (check "hp decoded" (= 945 (getf me :hp)))
    (check "max hp decoded" (= 1200 (getf me :max-hp)))
    (check "tp decoded" (= 300 (getf me :tp)))
    (check "meseta decoded" (= 123456 (getf me :meseta)))
    (check "no shifta -> level 0" (= 0 (getf me :shifta))))
  (check "shifta multiplier -> level"
         ;; level 20 multiplier on Ephinea is 10% + 19 * 1.3% = 0.347
         (= 20 (shifta-level 0.347)))
  (check "tech name lookup" (equal "Resta" (tech-name #x0F))))

;;; ------------------------------------------------------------------
;;; Telemetry accumulation
;;; ------------------------------------------------------------------

(defun tele-snapshot (&key (hp 100) (tp 50) (state 1) (pb 0.0) (meseta 1000)
                           (floor 1) (map 1) (tech 0) inventory monsters)
  (list :my-index 0
        :map map
        :quest-ptr 1
        :players (list (list :index 0 :name "Ryu" :class "HUcast"
                             :hp hp :max-hp 100 :tp tp :max-tp 50
                             :state state :pb pb :meseta meseta
                             :floor floor :room 2 :x 10.04 :z -3.06
                             :shifta 0 :deband 0 :invincible nil
                             :current-tech tech
                             :damage-traps 0 :freeze-traps 0 :confuse-traps 0))
        :inventory inventory
        :monsters monsters))

(defun run-telemetry-tests ()
  (format t "~&--- telemetry ---~%")
  (let* ((tick internal-time-units-per-second)
         (start (get-internal-real-time))
         (tele (make-telemetry :start-time start)))
    ;; Second 0: baseline frame, two monsters alive, 3 monomates.
    (telemetry-step tele (tele-snapshot
                          :inventory '(:consumables (:monomate 3) :equipment ()
                                       :weapon nil)
                          :monsters '((:id 1 :hp 50) (:id 2 :hp 30)))
                    :now start)
    ;; Second 1: cast Resta, one monster killed, one monomate used,
    ;; 100 meseta charged.
    (telemetry-step tele (tele-snapshot
                          :state 8 :tech #x0F :meseta 900
                          :inventory '(:consumables (:monomate 2) :equipment ()
                                       :weapon nil)
                          :monsters '((:id 1 :hp 0) (:id 2 :hp 30)))
                    :now (+ start tick))
    ;; Second 2: died.
    (telemetry-step tele (tele-snapshot :hp 0 :state 15 :meseta 900)
                    :now (+ start (* 2 tick)))
    (let ((data (telemetry-run-data tele)))
      (check "one frame per second" (= 3 (length (getf data :frames))))
      (check "frame layout matches +frame-keys+"
             (= (length ephinea-ta-client::+frame-keys+)
                (length (first (getf data :frames)))))
      (check "death counted" (= 1 (getf data :death-count)))
      (check "kill counted" (= 1 (getf data :kills)))
      (check "meseta charged" (= 100 (getf data :meseta-charged)))
      (check "monomate use counted"
             (equal '((:monomate . 1)) (getf data :items-used)))
      (check "resta cast counted"
             (equal '(("Resta" . 1)) (getf data :techs-cast)))
      (check "bare-handed accrues seconds"
             (let ((weapon (find "Bare Handed" (getf data :weapons)
                                 :key (lambda (entry) (getf entry :id))
                                 :test #'equal)))
               (and weapon (= 2 (getf weapon :seconds)))))
      (check "time-by-state covers the dead second"
             (let ((cell (assoc 15 (getf data :time-by-state))))
               (and cell (plusp (cdr cell)))))
      (check "run data is printable"
             (stringp (with-standard-io-syntax
                        (write-to-string data :readably t)))))))

;;; ------------------------------------------------------------------
;;; Run JSON payload
;;; ------------------------------------------------------------------

(defun run-payload-tests ()
  (format t "~&--- run payload ---~%")
  (let* ((run (list :quest-slug "ep1-towards-the-future"
                    :quest-name "Towards the Future"
                    :episode 1 :time-ms 754321 :party-size 1 :pb t
                    :difficulty "Ultimate" :death-count 2
                    :players (list (list :name "Ryu" :class "HUcast"
                                         :level 142 :section-id "Skyly"
                                         :guild-card "42001234"))
                    :telemetry (list :frames '((0 945 300 0 0 1 2 10.0 -3.1
                                                0 0 0 1 12 0))
                                     :events '((:t 12 :type "death"))
                                     :death-count 2 :meseta-charged 400
                                     :kills 55 :tp-used 120
                                     :traps-used '(:dt 0 :ft 2 :ct 0)
                                     :items-used '((:monomate . 1))
                                     :techs-cast '(("Resta" . 3))
                                     :time-by-state '((1 . 60000))
                                     :weapons (list
                                               (list :id "00010000"
                                                     :display "Charge Vulcan +9"
                                                     :type :weapon :seconds 700
                                                     :attacks 512 :techs 0)))))
         (parsed (com.inuoe.jzon:parse (ephinea-ta-client::run-json run))))
    (check "payload difficulty" (equal "Ultimate" (gethash "difficulty" parsed)))
    (check "payload death count" (eql 2 (gethash "death_count" parsed)))
    (check "payload episode" (eql 1 (gethash "episode" parsed)))
    (let ((player (aref (gethash "players" parsed) 0)))
      (check "payload player level" (eql 142 (gethash "level" player)))
      (check "payload player section" (equal "Skyly" (gethash "section_id" player)))
      (check "payload player guild card"
             (equal "42001234" (gethash "guild_card" player))))
    (let ((telemetry (gethash "telemetry" parsed)))
      (check "payload telemetry present" (hash-table-p telemetry))
      (check "payload frame keys"
             (equalp (coerce ephinea-ta-client::+frame-keys+ 'list)
                     (coerce (gethash "frame_keys" telemetry) 'list)))
      (check "payload one frame"
             (= 1 (length (gethash "frames" telemetry))))
      (check "payload items snake_cased"
             (eql 1 (gethash "monomate" (gethash "items_used" telemetry))))
      (check "payload traps skip zeroes"
             (and (eql 2 (gethash "ft" (gethash "traps_used" telemetry)))
                  (null (gethash "dt" (gethash "traps_used" telemetry)))))
      (check "payload weapon display"
             (equal "Charge Vulcan +9"
                    (gethash "display" (aref (gethash "weapons" telemetry) 0))))
      (check "payload event"
             (equal "death" (gethash "type"
                                     (aref (gethash "events" telemetry) 0)))))))

;;; ------------------------------------------------------------------
;;; Detector integration: telemetry rides along with completed runs
;;; ------------------------------------------------------------------

(defun run-detect-telemetry-tests ()
  (format t "~&--- detect + telemetry ---~%")
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader))
    (step-with detector (ttf-reader :start 1))
    (sleep 0.05)
    (step-with detector (ttf-reader :start 1))
    (sleep 0.05)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "run has difficulty" (equal "Normal" (getf run :difficulty)))
      (check "run has death count" (eql 0 (getf run :death-count)))
      (check "run has telemetry" (listp (getf run :telemetry)))
      (check "telemetry recorded a frame"
             (plusp (length (getf (getf run :telemetry) :frames))))
      (check "player carries level"
             (= 1 (getf (first (getf run :players)) :level))))))

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

;;; ------------------------------------------------------------------
;;; Recorder: capture-backend mock and state machine tests
;;; ------------------------------------------------------------------

(defclass mock-backend ()
  ((events :initform '() :accessor mock-events
           :documentation "Chronological list of side-effect events.")
   (alive :initform t :accessor mock-alive)
   (start-result :initform :ok :accessor mock-start-result)
   (stale :initform '() :accessor mock-stale)))

(defun record-event (backend &rest event)
  (setf (mock-events backend)
        (append (mock-events backend) (list event))))

(defun events-of (backend kind)
  (remove kind (mock-events backend) :key #'first :test-not #'eq))

(defmethod backend-start-capture ((backend mock-backend) ffmpeg-path args
                                  output-path &key audio-pipe audio-pid)
  (record-event backend :start ffmpeg-path args output-path
                audio-pipe audio-pid)
  (if (eq (mock-start-result backend) :ok)
      :mock-capture
      (values nil "mock start failure")))

(defmethod backend-capture-alive-p ((backend mock-backend) capture)
  (declare (ignore capture))
  (mock-alive backend))

(defmethod backend-request-stop ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :stop))

(defmethod backend-kill-capture ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :kill))

(defmethod backend-close-capture ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :close))

(defmethod backend-rename-file ((backend mock-backend) from to)
  (record-event backend :rename from to))

(defmethod backend-delete-file ((backend mock-backend) path)
  (record-event backend :delete path))

(defmethod backend-list-stale-files ((backend mock-backend) dir)
  (declare (ignore dir))
  (mock-stale backend))

(defmacro with-recording-config ((&rest overrides) &body body)
  "Run BODY with an in-memory config; OVERRIDES are plist entries laid
over the defaults. Restores the global config afterwards (it is bound)."
  `(let ((ephinea-ta-client::*config*
           (append (list ,@overrides)
                   (copy-list ephinea-ta-client::*default-config*))))
     ,@body))

(defun make-test-run (&key (slug "ep1-test-quest") (time-ms 599123))
  (list :quest-slug slug
        :time-ms time-ms
        :finished-at (encode-universal-time 0 30 21 4 7 2026)))

(defun make-test-recorder ()
  (let ((backend (make-instance 'mock-backend)))
    (values (make-recorder :backend backend) backend)))

(defun run-recorder-tests ()
  (format t "~&--- recorder ---~%")
  (with-recording-config (:record-enabled t)
    ;; Happy path: quest completes, video kept under the run's name.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "recording starts on :idle -> :in-quest"
             (eq (recorder-state rec) :recording))
      (let ((start (first (events-of backend :start))))
        (check "ffmpeg args capture the window title"
               (member "title=Ephinea PSOBB" (third start) :test #'equal))
        (check "ffmpeg writes to a rec-tmp file"
               (search "rec-tmp-" (fourth start))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      ;; The full clear completes and the detector flips to :idle on the
      ;; same frame; the run must still be credited to this capture.
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (check "stop is requested when the detector goes idle"
             (and (eq (recorder-state rec) :stopping)
                  (= 1 (length (events-of backend :stop)))))
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (let ((rename (first (events-of backend :rename))))
        (check "completed run's video is renamed"
               (and rename (search "rec-tmp-" (second rename))))
        (check "final name has quest, time and date"
               (and rename
                    (search "ep1-test-quest 9'59.123 (2026-07-04 2130).mp4"
                            (third rename)))))
      (check "recorder returns to idle after finalize"
             (and (eq (recorder-state rec) :idle)
                  (null (events-of backend :delete))
                  (= 1 (length (events-of backend :close))))))
    ;; Abandoned quest: no completed runs, file deleted.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "abandoned quest video is deleted"
             (and (= 1 (length (events-of backend :delete)))
                  (null (events-of backend :rename))
                  (eq (recorder-state rec) :idle))))
    ;; Segment completed, then the player leaves before the full clear.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep1-seg" :time-ms 120500))
                     "Ephinea PSOBB")
      (check "segment completion does not stop the capture"
             (eq (recorder-state rec) :recording))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "segment-only capture is kept under the segment name"
             (search "ep1-seg 2'00.500"
                     (third (first (events-of backend :rename))))))
    ;; Full clear + segment: the longest run names the file.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep1-seg" :time-ms 120500))
                     "Ephinea PSOBB")
      (recorder-step rec :idle
                     (list (make-test-run :slug "ep1-full" :time-ms 599123))
                     "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "full clear (longest run) names the video"
             (search "ep1-full 9'59.123"
                     (third (first (events-of backend :rename))))))
    ;; ffmpeg fails to start: error surfaced, retried on the NEXT quest.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-start-result backend) :fail)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start leaves the recorder idle with an error"
             (and (eq (recorder-state rec) :idle)
                  (recorder-last-error rec)))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start is not retried mid-quest"
             (= 1 (length (events-of backend :start))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start is retried on the next quest"
             (= 2 (length (events-of backend :start)))))
    ;; ffmpeg dies mid-recording: cleanup + error, detection unaffected.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "ffmpeg dying mid-quest deletes the file and reports"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :delete)))
                  (recorder-last-error rec))))
    ;; "q" ignored: killed after the grace period, file still kept.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (ephinea-ta-client::recorder-stop-deadline rec)
            (1- (get-internal-real-time)))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "unresponsive ffmpeg is killed after the grace period"
             (= 1 (length (events-of backend :kill))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "kill happens only once" (= 1 (length (events-of backend :kill))))
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "killed capture is still kept (fragmented mp4)"
             (= 1 (length (events-of backend :rename)))))
    ;; Shutdown mid-recording finishes the capture synchronously.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest (list (make-test-run)) "Ephinea PSOBB")
      (recorder-shutdown rec :timeout 0)
      (check "shutdown mid-recording kills and keeps the completed run"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :kill)))
                  (= 1 (length (events-of backend :rename))))))
    ;; No window title (mock reader / not attached): no capture.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() nil)
      (check "no window title means no capture"
             (and (eq (recorder-state rec) :idle)
                  (null (mock-events backend)))))
    ;; Stale tmp files from a crashed session are removed at startup.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-stale backend) '("a/rec-tmp-1.mp4" "a/rec-tmp-2.mp4"))
      (cleanup-stale-recordings rec)
      (check "stale recordings are deleted at startup"
             (equal '("a/rec-tmp-1.mp4" "a/rec-tmp-2.mp4")
                    (mapcar #'second (events-of backend :delete))))))
  ;; Recording disabled: the poll loop feeds frames but nothing happens.
  (with-recording-config (:record-enabled nil)
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "disabled recorder does nothing"
             (and (eq (recorder-state rec) :idle)
                  (null (mock-events backend))))))
  ;; Pure helpers.
  (check "sanitize-filename strips reserved characters"
         (string= "a-b-c-d" (sanitize-filename "a:b/c\"d")))
  (check "video filename prefers the in-game quest name"
         (search "Towards the Future 9'59.123"
                 (run-video-filename
                  (list :quest-slug "ep1-towards-the-future"
                        :quest-name "Towards the Future"
                        :time-ms 599123
                        :finished-at (encode-universal-time 0 30 21 4 7 2026)))))
  (check "best-session-run picks the longest run"
         (string= "long"
                  (getf (best-session-run
                         (list (list :quest-slug "short" :time-ms 10)
                               (list :quest-slug "long" :time-ms 20)))
                        :quest-slug)))
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4")))
    (check "ffmpeg args use fragmented mp4"
           (member "+frag_keyframe+empty_moov" args :test #'equal))
    (check "ffmpeg args set the poll framerate"
           (member "30" args :test #'equal))
    (check "ffmpeg output path is the last argument"
           (equal "out.mp4" (first (last args))))
    (check "video-only args carry no audio input"
           (not (member "s16le" args :test #'equal))))
  ;; Audio arguments and their video-only fallback.
  (let* ((pipe (ephinea-ta-client::audio-pipe-name))
         (with-audio (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :audio-pipe pipe)))
    (check "audio args add the pipe input and aac"
           (and (member pipe with-audio :test #'equal)
                (member "aac" with-audio :test #'equal)))
    (check "stripping audio args restores the video-only argv"
           (equal (build-ffmpeg-args :window-title "T" :output-path "out.mp4")
                  (ephinea-ta-client::strip-audio-args with-audio pipe)))
    (let ((retargeted (ephinea-ta-client::retarget-audio-args
                       with-audio :sample-format "f32le"
                       :rate 44100 :channels 2)))
      (check "retargeting rewrites the audio format tokens"
             (and (member "f32le" retargeted :test #'equal)
                  (member "44100" retargeted :test #'equal)
                  (not (member "s16le" retargeted :test #'equal))
                  (not (member "48000" retargeted :test #'equal))))
      (check "retargeting keeps the video tokens intact"
             (and (member "gdigrab" retargeted :test #'equal)
                  (member "30" retargeted :test #'equal)))))
  ;; The recorder passes the audio pipe and target pid to the backend.
  (with-recording-config (:record-enabled t :record-audio t)
    (let ((ephinea-ta-client::*audio-target-pid* 1234))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (let ((start (first (events-of backend :start))))
          (check "recorder hands the backend the audio pipe and pid"
                 (and (equal (fifth start) (ephinea-ta-client::audio-pipe-name))
                      (eql (sixth start) 1234))))))
    (let ((ephinea-ta-client::*audio-target-pid* nil))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (check "no attached game pid means no audio pipe"
               (null (fifth (first (events-of backend :start))))))))
  (with-recording-config (:record-enabled t :record-audio nil)
    (let ((ephinea-ta-client::*audio-target-pid* 1234))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (check "audio can be disabled in config"
               (null (fifth (first (events-of backend :start)))))))))

;;; ------------------------------------------------------------------
;;; Video attach flow: recordings linked to queue entries, clipboard
;;; URL recognition and target resolution
;;; ------------------------------------------------------------------

(defmacro with-test-store ((&rest initial-runs) &body body)
  "Run BODY against a private *RUNS* list and a throwaway queue file, so
store functions that persist never touch the real %APPDATA% queue."
  `(let ((ephinea-ta-client::*runs* (list ,@initial-runs))
         (ephinea-ta-client::*queue-path*
           (merge-pathnames (format nil "eta-test-queue-~d.sexp"
                                    (get-internal-real-time))
                            (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ephinea-ta-client::*queue-path*)))))

(defun run-video-flow-tests ()
  (format t "~&--- video attach flow ---~%")
  ;; Recorder: ON-KEEP fires exactly when a video is saved.
  (with-recording-config (:record-enabled t)
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "on-keep is called once with the final path and best run"
             (and (= 1 (length kept))
                  (search "9'59.123" (first (first kept)))
                  (equal "ep1-test-quest"
                         (getf (second (first kept)) :quest-slug)))))
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB") ; abandoned
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "on-keep is not called for abandoned captures" (null kept)))
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB") ; ffmpeg died
      (check "on-keep is not called when the capture aborts" (null kept)))
    (let* ((backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (declare (ignore path run))
                                          (error "callback boom")))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "an erroring on-keep neither sticks nor reports"
             (and (eq (recorder-state rec) :idle)
                  (null (recorder-last-error rec))))))
  ;; Submission updates carry the server id for later video attachment.
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "id" payload) 42
          (gethash "url" payload) "https://x/runs/42")
    (check "created runs remember their server id"
           (equal '(:status :submitted :url "https://x/runs/42" :server-id 42)
                  (ephinea-ta-client::submission-updates :created payload)))
    (check "duplicate runs remember their server id too"
           (eql 42 (getf (ephinea-ta-client::submission-updates
                          :duplicate payload)
                         :server-id))))
  (check "rejected runs carry a reason, not a server id"
         (let ((payload (make-hash-table :test 'equal)))
           (setf (gethash "message" payload) "nope")
           (let ((updates (ephinea-ta-client::submission-updates
                           :rejected payload)))
             (and (null (getf updates :server-id))
                  (search "nope" (getf updates :reason))))))
  ;; Linking a saved video to its (copy-replaced) queue entry.
  (with-test-store ()
    (let* ((run (make-test-run))
           (entry (enqueue-run! run)))
      (ephinea-ta-client::update-run! entry :status :submitted :server-id 7)
      (let ((linked (ephinea-ta-client::link-video-file! run "C:/v/run.mp4")))
        (check "link-video-file! matches by natural key after updates"
               (and linked (search "run.mp4" (getf linked :video-path))))
        (check "linked entry still carries its server id"
               (eql 7 (getf linked :server-id))))
      (check "link-video-file! returns NIL for unknown runs"
             (null (ephinea-ta-client::link-video-file!
                    (make-test-run :slug "ep1-other") "C:/v/x.mp4")))))
  ;; Active entries survive trimming and restarts; attached ones do not.
  (let* ((unattached (list :status :submitted :server-id 1 :video-path "v.mp4"))
         (attached (list :status :submitted :server-id 2 :video-path "w.mp4"
                         :video-attached t))
         ;; Newest first; the attached entry is the oldest of 61 finished.
         (runs (cons unattached
                     (append (loop :for i :below 60
                                   :collect (list :status :submitted :n i))
                             (list attached))))
         (trimmed (ephinea-ta-client::trim-finished-runs runs 50)))
    (check "unattached video survives the finished-run cap"
           (member unattached trimmed))
    (check "attached video counts as finished and trims away"
           (not (member attached trimmed))))
  (check "attached entries are not active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 2 :video-path "w.mp4"
                     :video-attached t))))
  (check "rejected runs without a server id are not kept for video"
         (not (ephinea-ta-client::entry-active-p
               (list :status :rejected :video-path "v.mp4"))))
  (with-test-store ((list :status :submitted :server-id 1 :video-path "v.mp4"
                          :telemetry '(:frames ()))
                    (list :status :queued :telemetry '(:frames ())))
    (ephinea-ta-client::save-queue!)
    (let ((saved (ephinea-ta-client::read-sexp-file
                  ephinea-ta-client::*queue-path*)))
      (check "queue file keeps both active entries" (= 2 (length saved)))
      (check "persisted video entry drops its telemetry"
             (null (getf (first saved) :telemetry)))
      (check "persisted queued entry keeps its telemetry"
             (getf (second saved) :telemetry))))
  ;; Which run does a copied URL belong to?
  (let ((a (list :quest-slug "a" :time-ms 1 :finished-at 1 :server-id 1))
        (b (list :quest-slug "b" :time-ms 2 :finished-at 2 :server-id 2)))
    (check "no candidates -> NIL"
           (null (ephinea-ta-client::resolve-video-target '() nil)))
    (check "a single candidate needs no preference"
           (eq a (ephinea-ta-client::resolve-video-target (list a) b)))
    (check "the preferred run wins among several"
           (eq b (ephinea-ta-client::resolve-video-target
                  (list a b) (copy-list b))))
    (check "several candidates without a preference -> :choose"
           (eq :choose (ephinea-ta-client::resolve-video-target (list a b) nil)))
    (check "a stale preference falls back to :choose"
           (eq :choose (ephinea-ta-client::resolve-video-target
                        (list a b)
                        (list :quest-slug "gone" :time-ms 9 :finished-at 9)))))
  (with-test-store ((list :status :submitted :server-id 1)
                    (list :status :queued)
                    (list :status :submitted :server-id 3 :video-attached t))
    (check "video candidates need a server id and no attached video"
           (equal '(1)
                  (mapcar (lambda (entry) (getf entry :server-id))
                          (ephinea-ta-client::video-candidates)))))
  ;; Labels for the new Video column and statuses.
  (check "video label: saved recording"
         (equal "saved" (ephinea-ta-client::run-video-label
                         (list :video-path "v.mp4"))))
  (check "video label: attached"
         (equal "attached" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t))))
  (check "video label: no recording"
         (equal "" (ephinea-ta-client::run-video-label (list :status :queued))))
  (check "status label: saved video points at the Upload button"
         (search "Upload" (ephinea-ta-client::run-status-label
                           (list :status :submitted :video-path "v.mp4"))))
  (check "status label: attached video says awaiting review"
         (search "awaiting review"
                 (ephinea-ta-client::run-status-label
                  (list :status :submitted :video-attached t))))
  ;; Clipboard URL recognition.
  (check "watch URLs are recognized"
         (ephinea-ta-client::youtube-video-url
          "https://www.youtube.com/watch?v=dQw4w9WgXcQ"))
  (check "watch URLs with extra query params are recognized"
         (ephinea-ta-client::youtube-video-url
          "https://www.youtube.com/watch?app=desktop&v=dQw4w9WgXcQ&t=10s"))
  (check "youtu.be share URLs are recognized"
         (ephinea-ta-client::youtube-video-url
          "https://youtu.be/dQw4w9WgXcQ?si=abc"))
  (check "shorts and live URLs are recognized"
         (and (ephinea-ta-client::youtube-video-url
               "https://www.youtube.com/shorts/dQw4w9WgXcQ")
              (ephinea-ta-client::youtube-video-url
               "https://m.youtube.com/live/dQw4w9WgXcQ")))
  (check "surrounding whitespace is trimmed"
         (equal "https://youtu.be/dQw4w9WgXcQ"
                (ephinea-ta-client::youtube-video-url
                 " https://youtu.be/dQw4w9WgXcQ
")))
  (check "non-video YouTube pages are not recognized"
         (notany #'ephinea-ta-client::youtube-video-url
                 (list "https://www.youtube.com/"
                       "https://www.youtube.com/@somechannel"
                       "https://www.youtube.com/playlist?list=PLx"
                       "https://www.youtube.com/watch?v=tooshort"
                       "https://www.youtube.com/watch?v=muchtoolongid")))
  (check "non-YouTube text is not recognized"
         (notany #'ephinea-ta-client::youtube-video-url
                 (list "https://example.com/watch?v=dQw4w9WgXcQ"
                       "https://twitch.tv/videos/123456"
                       "watch?v=dQw4w9WgXcQ"
                       "just some words"
                       nil
                       ""))))

;;; ------------------------------------------------------------------
;;; UX helpers: status labels, list trimming, URL and error text
;;; ------------------------------------------------------------------

(defun run-ux-helper-tests ()
  (format t "~&--- ux helpers ---~%")
  ;; Status labels (shown in the runs list).
  (let ((label (ephinea-ta-client::run-status-label
                (list :status :submitted
                      :url "https://example.com/runs/42"))))
    (check "submitted label does not leak the URL"
           (not (search "http" label)))
    (check "submitted label says draft and hints at the video step"
           (and (search "draft" label) (search "video" label))))
  (check "rejected label carries the reason"
         (search "too fast"
                 (ephinea-ta-client::run-status-label
                  (list :status :rejected :reason "too fast"))))
  (check "format-run-time formats minutes:seconds.millis"
         (string= "9:59.123" (ephinea-ta-client::format-run-time 599123)))
  ;; Trimming: unfinished entries survive, finished ones are capped.
  (let* ((runs (loop :for i :from 0 :below 70
                     :collect (list :status (case (mod i 7)
                                              (3 :queued)
                                              (5 :failed)
                                              (t :submitted))
                                    :n i)))
         (trimmed (ephinea-ta-client::trim-finished-runs runs 50)))
    (check "trim keeps every queued/failed entry"
           (= (count-if (lambda (entry)
                          (member (getf entry :status) '(:queued :failed)))
                        runs)
              (count-if (lambda (entry)
                          (member (getf entry :status) '(:queued :failed)))
                        trimmed)))
    (check "trim caps finished entries at the limit"
           (= 50 (count :submitted trimmed :key (lambda (e) (getf e :status)))))
    (check "trim keeps the newest finished entries in order"
           (equal (subseq (mapcar (lambda (e) (getf e :n)) runs) 0 10)
                  (subseq (mapcar (lambda (e) (getf e :n)) trimmed) 0 10))))
  ;; Browser URL guard.
  (check "http and https URLs are openable"
         (and (ephinea-ta-client::valid-http-url-p "http://x/y")
              (ephinea-ta-client::valid-http-url-p "https://x/y")))
  (check "non-web strings are rejected"
         (notany #'ephinea-ta-client::valid-http-url-p
                 (list "C:\\evil.exe" "file:///c:/x" "https://x/a b"
                       "javascript:alert(1)" nil 42 "")))
  ;; Human-readable server errors.
  (check "windows error code is extracted"
         (= 12029 (ephinea-ta-client::windows-error-code
                   "WinHttpConnect failed (Windows error 12029)")))
  (check "windows error code absent -> nil"
         (null (ephinea-ta-client::windows-error-code "plain message")))
  (flet ((text-for (message)
           (ephinea-ta-client::server-status-error-text
            (make-condition 'ephinea-ta-client::api-error :message message))))
    (check "connection failure reads like a sentence, not a condition"
           (search "could not connect"
                   (text-for "WinHttpConnect failed (Windows error 12029)")))
    (check "bad URL points at the settings fix"
           (search "Save settings" (text-for "Bad URL: nonsense")))
    (check "unexpected HTTP status mentions the response"
           (search "unexpected response"
                   (text-for "GET /api/quests -> 500"))))
  (check "non-api conditions still say what happened"
         (search "check failed"
                 (ephinea-ta-client::server-status-error-text
                  (make-condition 'simple-error
                                  :format-control "boom")))))

(defun run-client-tests ()
  (setf *failures* 0)
  (load-quest-defs)
  (run-memory-tests)
  (run-extended-player-tests)
  (run-telemetry-tests)
  (run-payload-tests)
  (run-detect-tests)
  (run-detect-telemetry-tests)
  (run-server-defs-tests)
  (run-gdv-segment-test)
  (run-trigger-log-tests)
  (run-recorder-tests)
  (run-video-flow-tests)
  (run-ux-helper-tests)
  (format t "~&=== client tests: ~d failure~:p ===~%" *failures*)
  *failures*)
