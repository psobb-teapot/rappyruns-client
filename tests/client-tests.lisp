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
;;; Inventory reading
;;; ------------------------------------------------------------------

(defun make-item-block (addr &key (owner 0) (type 0) (group 0) (index 0)
                                  equipped (id-high 0) (id-low 0) tool-count)
  "Item struct bytes at ADDR; ADDR feeds the tool-count XOR obfuscation."
  (let ((bytes (make-array #x200 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (put-u16 bytes #xD8 id-low)
    (put-u16 bytes #xDA id-high)
    (setf (aref bytes #xE4) owner
          (aref bytes #xF2) type
          (aref bytes #xF3) group
          (aref bytes #xF4) index)
    (when equipped (setf (aref bytes #x190) 1))
    (when tool-count
      (setf (aref bytes #x104)
            (logxor tool-count (ldb (byte 8 0) (+ addr #x104)))))
    (cons addr bytes)))

(defun run-inventory-tests ()
  (format t "~&--- inventory ---~%")
  ;; The item array covers the whole game world, so a multiplayer count
  ;; runs well past one 30-slot inventory. Regression: a 60-item cap
  ;; made READ-INVENTORY bail out in multiplayer, losing all weapon and
  ;; equipment telemetry (runs showed only "Bare Handed 0:00").
  (let* ((array-base #x00610000)
         (item-count 70)
         (globals (make-array 8 :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (pointers (make-array (* 4 item-count)
                               :element-type '(unsigned-byte 8)
                               :initial-element 0))
         (my-weapon #x00600000)
         (my-mate #x00600400)
         (their-weapon #x00600800)
         (their-mate #x00600C00))
    (put-u32 globals 0 array-base)      ; +item-array-pointer+
    (put-u16 globals 4 item-count)      ; +item-array-count+
    (put-u32 pointers (* 4 0) my-weapon)
    (put-u32 pointers (* 4 63) their-weapon)
    (put-u32 pointers (* 4 65) my-mate)
    (put-u32 pointers (* 4 69) their-mate)
    (let* ((reader (make-mock-reader
                    (cons #x00A8D81C globals)
                    (cons array-base pointers)
                    (make-item-block my-weapon :owner 0 :type 0 :group 2
                                               :index 5 :equipped t
                                               :id-high #x0001 :id-low #x0203)
                    (make-item-block my-mate :owner 0 :type 3 :group 0
                                             :index 0 :tool-count 5)
                    (make-item-block their-weapon :owner 1 :type 0 :group 2
                                                  :index 5 :equipped t)
                    (make-item-block their-mate :owner 1 :type 3 :group 0
                                                :index 0 :tool-count 99)))
           (inventory (read-inventory reader 0)))
      (check "inventory read past 60 items" (not (null inventory)))
      (check "only my equipment listed"
             (= 1 (length (getf inventory :equipment))))
      (check "equipped weapon resolved"
             (equal "00010203" (getf (getf inventory :weapon) :id)))
      (check "equipped weapon typed"
             (eq :weapon (getf (getf inventory :weapon) :type)))
      (check "my consumables counted"
             (= 5 (getf (getf inventory :consumables) :monomate)))))
  ;; Garbage counts still bail out instead of reading megabytes.
  (let ((globals (make-array 8 :element-type '(unsigned-byte 8)
                               :initial-element 0)))
    (put-u32 globals 0 #x00610000)
    (put-u16 globals 4 #xFFFF)
    (check "garbage item count -> NIL"
           (null (read-inventory (make-mock-reader (cons #x00A8D81C globals))
                                 0)))))

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
                           (floor 1) (map 1) (tech 0) (shifta 0)
                           inventory monsters fast-burst extra-players)
  (list :my-index 0
        :map map
        :quest-ptr 1
        :fast-burst fast-burst
        :players (cons (list :index 0 :name "Ryu" :class "HUcast"
                             :hp hp :max-hp 100 :tp tp :max-tp 50
                             :state state :pb pb :meseta meseta
                             :floor floor :room 2 :x 10.04 :z -3.06
                             :shifta shifta :deband 0 :invincible nil
                             :current-tech tech
                             :damage-traps 0 :freeze-traps 0 :confuse-traps 0)
                       extra-players)
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
;;; Monster reading (psostats GetMonsterList parity)
;;; ------------------------------------------------------------------

(defconstant +npc-array-base+ #x00630000)
(defconstant +hp-table-base+ #x00640000)
(defconstant +monster-a-base+ #x00650000)
(defconstant +monster-b-base+ #x00660000)

(defun make-monster-block (&key (id 0) (unitxt 0) (x 0.0) (y 0.0) (z 0.0)
                                (facing 0) (status 0) (paralyzed 0)
                                (attacker 0) (zu-y 0.0))
  (let ((bytes (make-array #x420 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (put-u16 bytes #x1C id)
    (put-u32 bytes #x378 unitxt)
    (put-f32 bytes #x38 x)
    (put-f32 bytes #x3C y)
    (put-f32 bytes #x40 z)
    (put-u16 bytes #x60 facing)
    (put-u16 bytes #x268 status)
    (put-u16 bytes #x25C paralyzed)
    (put-u16 bytes #x2D8 attacker)
    (put-f32 bytes #x418 zu-y)
    bytes))

(defun make-monster-reader (&key (hp-7 50) (hp-8 #xFFFF))
  "Mock image with one player slot and three npc slots: a normal Booma
\(id 7), an empty pointer and a Zu (id 8) whose hp underflowed."
  (let ((npc-ptr (make-array 4 :element-type '(unsigned-byte 8)
                               :initial-element 0))
        (counts (make-array 8 :element-type '(unsigned-byte 8)
                              :initial-element 0))
        (hp-ptr (make-array 4 :element-type '(unsigned-byte 8)
                              :initial-element 0))
        (hp-table (make-array 512 :element-type '(unsigned-byte 8)
                                  :initial-element 0))
        (pointers (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
        (unitxt-ptr (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-element 0))
        (unitxt (make-array 20 :element-type '(unsigned-byte 8)
                               :initial-element 0))
        (names (make-array 512 :element-type '(unsigned-byte 8)
                               :initial-element 0))
        (booma (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-element 0))
        (zu (make-array 32 :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (put-u32 npc-ptr 0 +npc-array-base+)
    (put-u32 counts 0 3)                ; npc count
    (put-u32 counts 4 1)                ; player count
    (put-u32 hp-ptr 0 +hp-table-base+)
    (put-u16 hp-table (+ 4 (* 32 7)) hp-7)
    (put-u16 hp-table (+ 4 (* 32 8)) hp-8)
    ;; Entity slots: slot 0 is the player; monsters fill 1-3.
    (put-u32 pointers (* 4 1) +monster-a-base+)
    (put-u32 pointers (* 4 2) 0)
    (put-u32 pointers (* 4 3) +monster-b-base+)
    (put-u32 unitxt-ptr 0 #x00670000)
    (put-u32 unitxt 16 #x00680000)
    (put-u32 names (* 4 5) #x00690000)
    (put-u32 names (* 4 94) #x00690040)
    (put-utf16 booma 0 "Booma")
    (put-utf16 zu 0 "Zu")
    (make-mock-reader
     (cons #x007B4BA2 npc-ptr)
     (cons #x00AAE164 counts)
     (cons #x00B5F800 hp-ptr)
     (cons +hp-table-base+ hp-table)
     ;; READ-MONSTERS starts its block read past the player slots.
     (cons +npc-array-base+ pointers)
     (cons +monster-a-base+ (make-monster-block
                             :id 7 :unitxt 5 :x 10.5 :y 2.0 :z 3.25
                             :facing 100 :status #x02 :attacker 1))
     (cons +monster-b-base+ (make-monster-block
                             :id 8 :unitxt 94 :y 2.0 :zu-y 3.0
                             :paralyzed #x10))
     (cons #x00A9CD50 unitxt-ptr)
     (cons #x00670000 unitxt)
     (cons #x00680000 names)
     (cons #x00690000 booma)
     (cons #x00690040 zu))))

(defun run-monster-read-tests ()
  (format t "~&--- monster reading ---~%")
  (clrhash ephinea-ta-client::*monster-name-cache*)
  (let* ((monsters (read-monsters (make-monster-reader)))
         (booma (find 7 monsters :key (lambda (m) (getf m :id))))
         (zu (find 8 monsters :key (lambda (m) (getf m :id)))))
    (check "two monsters read (empty slot skipped)" (= 2 (length monsters)))
    (check "monster id and unitxt" (and booma (= 5 (getf booma :unitxt))))
    (check "monster name resolved" (equal "Booma" (getf booma :name)))
    (check "monster hp from the Ephinea table" (= 50 (getf booma :hp)))
    (check "monster position"
           (and (< (abs (- 10.5 (getf booma :x))) 0.01)
                (< (abs (- 3.25 (getf booma :z))) 0.01)))
    (check "monster facing" (= 100 (getf booma :facing)))
    (check "monster frozen" (and (getf booma :frozen)
                                 (not (getf booma :paralyzed))
                                 (not (getf booma :confused))))
    (check "monster last attacker" (= 1 (getf booma :last-attacker)))
    (check "monster index counts player slots" (= 1 (getf booma :index)))
    (check "hp underflow clamps to zero" (= 0 (getf zu :hp)))
    (check "monster paralyzed" (and (getf zu :paralyzed)
                                    (not (getf zu :frozen))))
    (check "Zu height adds the extra field"
           (< (abs (- 5.0 (getf zu :y))) 0.01)))
  ;; Fast burst: pointer chain to a zero u16 means fast burst is on.
  (flet ((fast-burst-reader (flag)
           (let ((base (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-element 0))
                 (link (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-element 0))
                 (value (make-array 2 :element-type '(unsigned-byte 8)
                                      :initial-element 0)))
             (put-u32 base 0 #x100)
             (put-u32 link 0 #x00620000)
             (put-u16 value 0 flag)
             (make-mock-reader (cons #x5B92DA base)
                               (cons (+ #x100 #x5B92DF) link)
                               (cons #x00620000 value)))))
    (check "fast burst detected"
           (eq t (ephinea-ta-client::fast-burst-enabled-p
                  (fast-burst-reader 0))))
    (check "slow burst detected"
           (not (ephinea-ta-client::fast-burst-enabled-p
                 (fast-burst-reader 1)))))
  ;; Boss identification and party shifta ceilings (pure tables).
  (check "boss: Sil Dragon" (equal "Sil Dragon"
                                   (ephinea-ta-client::boss-name 44 5)))
  (check "boss: Dal Ra Lie only at index 0"
         (and (equal "Dal Ra Lie" (ephinea-ta-client::boss-name 45 0))
              (null (ephinea-ta-client::boss-name 45 3))))
  (check "boss: Saint-Million head numbering"
         (equal "Saint-Million Head (2)" (ephinea-ta-client::boss-name 106 6)))
  (check "non-boss unitxt -> NIL" (null (ephinea-ta-client::boss-name 5 0)))
  (check "solo HUcast pb ceiling"
         (= 21 (ephinea-ta-client::max-party-pb-shifta
                '((:class "HUcast")))))
  (check "solo FOnewearl can out-shifta the pb ceiling"
         (= 30 (ephinea-ta-client::max-party-pb-shifta
                '((:class "FOnewearl")))))
  (check "two-player pb ceiling"
         (= 41 (ephinea-ta-client::max-party-pb-shifta
                '((:class "HUcast") (:class "RAmar"))))))

;;; ------------------------------------------------------------------
;;; psostats-parity telemetry: monster tracking, damage attribution,
;;; bosses, cheat heuristics and their JSON encoding
;;; ------------------------------------------------------------------

(defun run-psostats-telemetry-tests ()
  (format t "~&--- psostats parity telemetry ---~%")
  (let* ((tick internal-time-units-per-second)
         (ms (/ tick 1000))
         (start (get-internal-real-time))
         (tele (make-telemetry :start-time start :max-party-pb-shifta 21)))
    ;; t=0: a Booma and the Sil Dragon boss are up.
    (telemetry-step
     tele (tele-snapshot
           :monsters '((:id 1 :hp 100 :unitxt 5 :index 1 :name "Booma"
                        :last-attacker 0)
                       (:id 2 :hp 500 :unitxt 44 :index 2 :name "Sil Dragon"
                        :last-attacker 0)))
     :now start)
    ;; t=30ms: player 1 damages the Booma; a second Booma spawns.
    (telemetry-step
     tele (tele-snapshot
           :monsters '((:id 1 :hp 40 :unitxt 5 :index 1 :last-attacker 1)
                       (:id 2 :hp 500 :unitxt 44 :index 2 :last-attacker 0)
                       (:id 3 :hp 30 :unitxt 5 :index 3 :last-attacker 0)))
     :now (+ start (* 30 ms)))
    ;; t=50ms: the fresh Booma dies 20ms after spawning (frame-1 kill).
    (telemetry-step
     tele (tele-snapshot
           :monsters '((:id 1 :hp 40 :unitxt 5 :index 1 :last-attacker 1)
                       (:id 2 :hp 500 :unitxt 44 :index 2 :last-attacker 0)
                       (:id 3 :hp 0 :unitxt 5 :index 3 :last-attacker 0)))
     :now (+ start (* 50 ms)))
    ;; t=1s: player 1 finishes the first Booma (last hit, 40 hp credit).
    (telemetry-step
     tele (tele-snapshot
           :monsters '((:id 1 :hp 0 :unitxt 5 :index 1 :last-attacker 1)
                       (:id 2 :hp 500 :unitxt 44 :index 2 :last-attacker 0)))
     :now (+ start tick))
    ;; t=2s: own shifta above the party ceiling; a party member warps
    ;; while fast burst is on.
    (telemetry-step
     tele (tele-snapshot
           :shifta 25 :fast-burst t
           :extra-players '((:index 1 :name "Elly" :class "FOnewearl"
                             :guild-card "42009999" :floor 1 :room 3
                             :x 1.0 :y 0.0 :z 2.0 :facing 7 :warping t)))
     :now (+ start (* 2 tick)))
    (let ((data (telemetry-run-data tele)))
      (check "monsters dead counted" (= 2 (getf data :kills)))
      (check "damage attributed per player"
             (and (= 30 (cdr (assoc 0 (getf data :player-damage))))
                  (= 100 (cdr (assoc 1 (getf data :player-damage))))))
      (check "last hits attributed per player"
             (and (= 1 (cdr (assoc 0 (getf data :last-hits))))
                  (= 1 (cdr (assoc 1 (getf data :last-hits))))))
      (check "all monsters recorded" (= 3 (length (getf data :monsters))))
      (let ((fast (find 3 (getf data :monsters)
                        :key (lambda (m) (getf m :id))))
            (slow (find 1 (getf data :monsters)
                        :key (lambda (m) (getf m :id)))))
        (check "frame-1 kill flagged" (eq t (getf fast :frame1)))
        (check "normal kill not frame-1"
               (and (not (getf slow :frame1))
                    (<= 990 (getf slow :killed-ms) 1010))))
      (let ((boss (first (getf data :bosses))))
        (check "boss recorded" (equal "Sil Dragon" (getf boss :name)))
        (check "boss hp history follows the frames"
               (equal '(500 500 500) (getf boss :hp))))
      (check "monster hp pool follows the frames"
             (equal '(600 500 500) (getf data :monster-hp-pool)))
      (check "illegal shifta flagged" (eq t (getf data :illegal-shifta)))
      (check "fast warp flagged" (eq t (getf data :fast-warps)))
      (check "pb ceiling kept" (= 21 (getf data :max-party-pb-shifta)))
      (check "frames carry the damage column"
             (let ((frame (second (getf data :frames))))
               (= 30 (nth (position "damage" ephinea-ta-client::+frame-keys+
                                    :test #'string=)
                          frame))))
      (check "parity run data is printable"
             (stringp (with-standard-io-syntax
                        (write-to-string data :readably t))))
      ;; JSON encoding of the parity fields.
      (let* ((json (com.inuoe.jzon:parse
                    (com.inuoe.jzon:stringify
                     (ephinea-ta-client::telemetry-json data))))
             (monsters (gethash "monsters" json)))
        (check "json monsters array" (= 3 (length monsters)))
        (check "json frame1 only on the fast kill"
               (equal '(3)
                      (loop :for monster :across monsters
                            :when (eq t (gethash "frame1" monster))
                              :collect (gethash "id" monster))))
        (check "json bosses keyed by monster id"
               (equal "Sil Dragon"
                      (gethash "name" (gethash "2" (gethash "bosses" json)))))
        (check "json player damage keyed by player index"
               (eql 100 (gethash "1" (gethash "player_damage" json))))
        (check "json monster hp pool" (= 3 (length (gethash "monster_hp_pool"
                                                            json))))
        (check "json cheat flags"
               (and (eq t (gethash "illegal_shifta" json))
                    (eq t (gethash "fast_warps" json))))
        (let* ((frame (aref (gethash "frames" json) 2))
               (locs (aref frame (position "player_locs"
                                           ephinea-ta-client::+frame-keys+
                                           :test #'string=))))
          (check "json frame player locations keyed by guild card"
                 (and (hash-table-p locs)
                      (gethash "Ryu" locs)
                      (= 7 (length (gethash "42009999" locs)))))
          (check "json frame warp flag rides in the location row"
                 (eql 1 (aref (gethash "42009999" locs) 6))))))))

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
    (check "game gone -> idle" (eq :idle (detector-state detector))))
  ;; Abandoning a quest mid-run: attempts shorter than +abort-min-ms+
  ;; are noise and emit nothing.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "abort: quick lobby return emits nothing"
           (null (step-with detector (lobby-reader)))))
  ;; Past the threshold, returning to the lobby emits an aborted run
  ;; built from state captured at the start (no quest snapshot left).
  (flet ((age-trackers (detector seconds)
           (dolist (tracker (ephinea-ta-client::detector-trackers detector))
             (decf (ephinea-ta-client::tracker-start-time tracker)
                   (* seconds internal-time-units-per-second)))))
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (let ((run (first (step-with detector (lobby-reader)))))
        (check "abort: lobby return emits aborted run" (not (null run)))
        (check "abort: marked aborted" (eq t (getf run :aborted)))
        (check "abort: slug" (equal "ep1-towards-the-future"
                                    (getf run :quest-slug)))
        (check "abort: time >= 20s" (>= (getf run :time-ms) 20000))
        (check "abort: quest name captured at start"
               (equal "Towards the Future" (getf run :quest-name)))
        (check "abort: party captured" (= 2 (getf run :party-size)))
        (check "abort: telemetry attached" (not (null (getf run :telemetry))))
        (check "abort: detector reset" (eq :idle (detector-state detector)))))
    ;; Game exit mid-run aborts the same way.
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (let ((run (first (detector-step detector nil))))
        (check "abort: game exit emits aborted run"
               (eq t (getf run :aborted)))))
    ;; A completed run must never be re-emitted as aborted afterwards.
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (step-with detector (ttf-reader :start 1 :end 1))
      (check "abort: nothing re-emitted after completion"
             (null (step-with detector (lobby-reader)))))))

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
   (remux-alive :initform nil :accessor mock-remux-alive
                :documentation "NIL: the remux exits as soon as spawned.")
   (remux-ok :initform t :accessor mock-remux-ok)
   (remux-start-result :initform :ok :accessor mock-remux-start-result)
   (stale :initform '() :accessor mock-stale)
   (recordings :initform '() :accessor mock-recordings
               :documentation "(namestring size write-date) triples.")
   (fullscreen-monitor :initform nil :accessor mock-fullscreen-monitor)))

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
  (if (eq capture :mock-remux)
      (mock-remux-alive backend)
      (mock-alive backend)))

(defmethod backend-start-remux ((backend mock-backend) ffmpeg-path args)
  (record-event backend :remux ffmpeg-path args)
  (if (eq (mock-remux-start-result backend) :ok)
      :mock-remux
      (values nil "mock remux failure")))

(defmethod backend-capture-succeeded-p ((backend mock-backend) capture)
  (declare (ignore capture))
  (mock-remux-ok backend))

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

(defmethod backend-list-recordings ((backend mock-backend) dir)
  (declare (ignore dir))
  (mock-recordings backend))

(defmethod backend-fullscreen-monitor ((backend mock-backend))
  (mock-fullscreen-monitor backend))

(defmacro with-recording-config ((&rest overrides) &body body)
  "Run BODY with an in-memory config; OVERRIDES are plist entries laid
over the defaults. Restores the global config afterwards (it is bound)."
  `(let ((ephinea-ta-client::*config*
           (append (list ,@overrides)
                   (copy-list ephinea-ta-client::*default-config*))))
     ,@body))

(defun make-test-run (&key (slug "ep1-test-quest") (time-ms 599123) aborted)
  (append (list :quest-slug slug
                :time-ms time-ms
                :finished-at (encode-universal-time 0 30 21 4 7 2026))
          (when aborted (list :aborted t))))

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
      (let ((remux (first (events-of backend :remux))))
        (check "kept capture is remuxed with the moov up front"
               (and (eq (recorder-state rec) :remuxing)
                    remux
                    (member "+faststart" (third remux) :test #'equal)))
        (check "remux reads the tmp file"
               (and remux (search "rec-tmp-" (nth 4 (third remux)))))
        (check "remux writes the final name with quest, time and date"
               (and remux
                    (search "ep1-test-quest 9'59.123 (2026-07-04 2130).mp4"
                            (first (last (third remux)))))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "successful remux deletes the tmp and skips the rename"
             (let ((delete (first (events-of backend :delete))))
               (and delete
                    (search "rec-tmp-" (second delete))
                    (null (events-of backend :rename)))))
      (check "recorder returns to idle after finalize"
             (and (eq (recorder-state rec) :idle)
                  (= 2 (length (events-of backend :close))))))
    ;; Completed runs are stamped with the video offset (capture start
    ;; -> run start); the uploader forwards it as video_offset_ms so
    ;; telemetry seeks land on the right video time.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (declare (ignorable backend))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      ;; Pretend the capture has been running for 700 s.
      (setf (ephinea-ta-client::recorder-capture-start-real rec)
            (- (get-internal-real-time)
               (* 700 internal-time-units-per-second)))
      (let ((run (make-test-run :time-ms 599123)))
        (recorder-step rec :in-quest (list run) "Ephinea PSOBB")
        (check "completed runs carry the video offset of their start"
               (let ((offset (getf run :video-offset-ms)))
                 ;; 700s elapsed - 599.123s run = ~100.9s into the video
                 (and (integerp offset) (<= 100000 offset 102000)))))
      (let ((run (append (make-test-run :time-ms 1)
                         (list :video-offset-ms 42))))
        (recorder-step rec :in-quest (list run) "Ephinea PSOBB")
        (check "an existing video offset is left alone"
               (eql 42 (getf run :video-offset-ms)))))
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
                     (first (last (third (first (events-of backend :remux))))))))
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
                     (first (last (third (first (events-of backend :remux))))))))
    ;; Completed segment inside an aborted quest (a GDV reset): the
    ;; segment is the only run that can take the video on the site, so
    ;; it must outrank the slightly longer aborted stay.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep2-gdv-reset"
                                          :time-ms 145160))
                     "Ephinea PSOBB")
      (recorder-step rec :idle
                     (list (make-test-run :slug "ep2-gdv" :time-ms 148594
                                          :aborted t))
                     "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "a completed segment outranks a longer aborted run"
             (search "ep2-gdv-reset 2'25.160"
                     (first (last (third (first (events-of backend :remux))))))))
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
      (check "killed capture is still kept (remuxed from the fragments)"
             (= 1 (length (events-of backend :remux)))))
    ;; Remux fails: partial output dropped, fragmented original renamed.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-ok backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (let ((rename (first (events-of backend :rename))))
        (check "failed remux falls back to the fragmented original"
               (and rename
                    (search "rec-tmp-" (second rename))
                    (eq (recorder-state rec) :idle)))
        (check "failed remux drops its partial output first"
               (equal (third rename)
                      (second (first (events-of backend :delete)))))))
    ;; Remux cannot even start (ffmpeg gone): immediate rename fallback.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-start-result backend) :fail)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "unstartable remux falls back to a rename at once"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :rename))))))
    ;; Remux hangs: killed after the grace period, fallback still kept.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-alive backend) t
            (mock-remux-ok backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "healthy remux is not killed"
             (null (events-of backend :kill)))
      (setf (ephinea-ta-client::recorder-remux-deadline rec)
            (1- (get-internal-real-time)))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "hung remux is killed after the grace period"
             (= 1 (length (events-of backend :kill))))
      (setf (mock-remux-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "killed remux still keeps the fragmented original"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :rename))))))
    ;; Shutdown mid-recording finishes capture and remux synchronously.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest (list (make-test-run)) "Ephinea PSOBB")
      (recorder-shutdown rec :timeout 0)
      (check "shutdown mid-recording kills and keeps the completed run"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :kill)))
                  (= 1 (length (events-of backend :remux)))
                  (null (events-of backend :rename)))))
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
  (check "best-session-run prefers a completed run over a longer aborted one"
         (string= "seg"
                  (getf (best-session-run
                         (list (list :quest-slug "seg" :time-ms 10)
                               (list :quest-slug "abort" :time-ms 20
                                     :aborted t)))
                        :quest-slug)))
  (check "best-session-run falls back to the longest aborted run"
         (string= "ab2"
                  (getf (best-session-run
                         (list (list :quest-slug "ab1" :time-ms 10 :aborted t)
                               (list :quest-slug "ab2" :time-ms 20 :aborted t)))
                        :quest-slug)))
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4")))
    (check "ffmpeg args use fragmented mp4"
           (member "+frag_keyframe+empty_moov" args :test #'equal))
    (check "ffmpeg args set the poll framerate"
           (member "30" args :test #'equal))
    (check "video input probes minimally (A/V sync anchor)"
           (let ((probe (position "-probesize" args :test #'equal))
                 (grab (position "gdigrab" args :test #'equal)))
             (and probe grab
                  (equal "32" (nth (1+ probe) args))
                  (< probe grab))))
    (check "ffmpeg args encode at crf 29"
           (let ((crf (position "-crf" args :test #'equal)))
             (and crf (equal "29" (nth (1+ crf) args)))))
    (check "ffmpeg args disable B-frames (zero-based video timestamps)"
           (let ((bf (position "-bf" args :test #'equal)))
             (and bf (equal "0" (nth (1+ bf) args)))))
    (check "ffmpeg args cap the height at 1080 without upscaling"
           (let ((vf (position "-vf" args :test #'equal)))
             (and vf
                  (search "scale=-2" (nth (1+ vf) args))
                  (search "min(1080" (nth (1+ vf) args)))))
    (check "ffmpeg output path is the last argument"
           (equal "out.mp4" (first (last args))))
    (check "video-only args carry no audio input"
           (not (member "s16le" args :test #'equal))))
  (let ((args (build-remux-args "in.mp4" "out.mp4")))
    (check "remux args stream-copy the video with faststart"
           (and (member "copy" args :test #'equal)
                (member "+faststart" args :test #'equal)
                (not (member "libx264" args :test #'equal))))
    (check "remux args loudness-normalize the audio"
           (let ((af (position "-af" args :test #'equal)))
             (and af
                  (search "loudnorm" (nth (1+ af) args))
                  (member "aac" args :test #'equal))))
    (check "remux loudness target is -20 LUFS (-16 was too loud, issue 84)"
           (let ((af (position "-af" args :test #'equal)))
             (and af (search "loudnorm=I=-20:" (nth (1+ af) args)))))
    (check "remux applies no timestamp correction (sync fixed at the source)"
           (let ((af (position "-af" args :test #'equal)))
             (and af
                  (not (search "atrim" (nth (1+ af) args)))
                  (not (member "-itsoffset" args :test #'equal)))))
    (check "remux reads the input and writes the output last"
           (and (member "in.mp4" args :test #'equal)
                (equal "out.mp4" (first (last args))))))
  ;; Audio arguments and their video-only fallback.
  (let* ((pipe (ephinea-ta-client::audio-pipe-name))
         (with-audio (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :audio-pipe pipe)))
    (check "audio args add the pipe input and aac"
           (and (member pipe with-audio :test #'equal)
                (member "aac" with-audio :test #'equal)))
    (check "live capture args carry no loudnorm (it throttles the video)"
           (notany (lambda (arg) (search "loudnorm" arg)) with-audio))
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
  ;; Fullscreen capture: a game window covering its whole monitor is
  ;; grabbed with ddagrab (gdigrab records black over exclusive
  ;; fullscreen Direct3D).
  (check "a window spanning its monitor exactly is fullscreen"
         (ephinea-ta-client::rect-covers-p '(0 0 1920 1080)
                                           '(0 0 1920 1080)))
  (check "a fullscreen window on a secondary monitor is fullscreen"
         (ephinea-ta-client::rect-covers-p '(1920 0 3840 1080)
                                           '(1920 0 3840 1080)))
  (check "a maximized window (work area, above the taskbar) is not"
         (not (ephinea-ta-client::rect-covers-p '(0 0 1920 1032)
                                                '(0 0 1920 1080))))
  (check "an ordinary window is not fullscreen"
         (not (ephinea-ta-client::rect-covers-p '(100 100 1124 868)
                                                '(0 0 1920 1080))))
  (check "GDI display names map to 0-based output indices"
         (and (eql 0 (ephinea-ta-client::display-device-output-index
                      "\\\\.\\DISPLAY1"))
              (eql 11 (ephinea-ta-client::display-device-output-index
                       "\\\\.\\DISPLAY12"))
              (null (ephinea-ta-client::display-device-output-index
                     "\\\\.\\DISPLAY"))
              (null (ephinea-ta-client::display-device-output-index ""))))
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                 :fullscreen-monitor 1)))
    (check "fullscreen args capture via ddagrab, not gdigrab"
           (and (not (member "gdigrab" args :test #'equal))
                (member "lavfi" args :test #'equal)
                (find-if (lambda (arg)
                           (search "ddagrab=output_idx=1" arg))
                         args)))
    (check "fullscreen args keep the framerate and hide the mouse"
           (find-if (lambda (arg)
                      (and (search "framerate=30" arg)
                           (search "draw_mouse=0" arg)))
                    args))
    (check "fullscreen args download GPU frames before the scale cap"
           (let ((vf (position "-vf" args :test #'equal)))
             (and vf
                  (let ((filter (nth (1+ vf) args)))
                    (and (search "hwdownload" filter)
                         (search "min(1080" filter)
                         (< (search "hwdownload" filter)
                            (search "scale" filter)))))))
    (check "stripping audio args restores the fullscreen video-only argv"
           (let ((pipe (ephinea-ta-client::audio-pipe-name)))
             (equal args
                    (ephinea-ta-client::strip-audio-args
                     (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :audio-pipe pipe
                                        :fullscreen-monitor 1)
                     pipe)))))
  ;; The recorder asks the backend about fullscreen at capture start.
  (with-recording-config (:record-enabled t)
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-fullscreen-monitor backend) 0)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (let ((args (third (first (events-of backend :start)))))
        (check "recorder records a fullscreen game via ddagrab"
               (find-if (lambda (arg) (search "ddagrab=output_idx=0" arg))
                        args))))
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (let ((args (third (first (events-of backend :start)))))
        (check "recorder keeps gdigrab for a windowed game"
               (member "gdigrab" args :test #'equal)))))
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
      (check "on-keep waits for the remux" (null kept))
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
  ;; Clearing the list keeps only unsent runs - the one thing that
  ;; exists nowhere else. Drafts with a pending video go too (their
  ;; recording and server draft survive elsewhere), so recordings the
  ;; user never means to upload cannot haunt the list forever.
  (with-test-store ((list :status :submitted :server-id 1)
                    (list :status :queued)
                    (list :status :failed :reason "boom")
                    (list :status :submitted :server-id 2 :video-path "v.mp4")
                    (list :status :submitted :server-id 3 :video-path "w.mp4"
                          :video-attached t)
                    (list :status :duplicate :server-id 4)
                    (list :status :rejected :reason "nope"))
    (check "clear-runs! reports the removed count"
           (= 5 (ephinea-ta-client::clear-runs!)))
    (check "clear keeps only queued and failed entries, in order"
           (equal '(:queued :failed)
                  (mapcar (lambda (entry) (getf entry :status))
                          (queued-runs))))
    (check "clear persists the surviving queue"
           (= 2 (length (ephinea-ta-client::read-sexp-file
                         ephinea-ta-client::*queue-path*))))
    (check "cleared pending-video draft is no longer a video candidate"
           (null (ephinea-ta-client::video-candidates))))
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
  ;; The auto-uploaded hosted copy may still be swapped for the
  ;; player's own link, so those entries stay candidates until an
  ;; external URL is on file (issue #35).
  (with-test-store ((list :status :submitted :server-id 4 :video-attached t
                          :video-uploaded t)
                    (list :status :submitted :server-id 5 :video-attached t
                          :video-uploaded t :video-url "https://youtu.be/x")
                    (list :status :submitted :server-id 6 :video-attached t))
    (check "an auto-uploaded video can still take an external URL"
           (equal '(4)
                  (mapcar (lambda (entry) (getf entry :server-id))
                          (ephinea-ta-client::video-candidates)))))
  ;; An aborted run never auto-uploads, but its recording can still be
  ;; put on YouTube by hand, so it is a candidate for a copied link.
  (with-test-store ((list :status :submitted :server-id 9 :aborted t
                          :video-path "v.mp4"))
    (check "an aborted run is a candidate for a copied YouTube URL"
           (equal '(9)
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
  (check "status label: saved video announces the automatic upload"
         (with-recording-config (:video-upload t)
           (search "automatically"
                   (ephinea-ta-client::run-status-label
                    (list :status :submitted :video-path "v.mp4")))))
  (check "status label: saved video points at the Upload button when auto-upload is off"
         (with-recording-config (:video-upload nil)
           (search "Upload" (ephinea-ta-client::run-status-label
                             (list :status :submitted :video-path "v.mp4")))))
  (check "status label: a given-up upload points back at the Upload button"
         (with-recording-config (:video-upload t)
           (search "Upload" (ephinea-ta-client::run-status-label
                             (list :status :submitted :video-path "v.mp4"
                                   :upload-given-up t)))))
  (check "status label: attached video says awaiting review"
         (search "awaiting review"
                 (ephinea-ta-client::run-status-label
                  (list :status :submitted :video-attached t))))
  ;; An aborted run's link never enters review, so its label must not
  ;; promise one - it just reports the attached, private video.
  (check "status label: an aborted attached video is not awaiting review"
         (let ((label (ephinea-ta-client::run-status-label
                       (list :status :submitted :video-attached t :aborted t))))
           (and (search "aborted" label)
                (not (search "awaiting review" label)))))
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
  ;; Debug mode gates developer-only settings (the Server URL field).
  (check "debug mode is off by default"
         (with-recording-config ()
           (not (ephinea-ta-client::debug-mode-p))))
  (check "debug mode can be enabled in config"
         (with-recording-config (:debug t)
           (ephinea-ta-client::debug-mode-p)))
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
           (search "Save & verify" (text-for "Bad URL: nonsense")))
    (check "unexpected HTTP status mentions the response"
           (search "unexpected response"
                   (text-for "GET /api/quests -> 500"))))
  (check "non-api conditions still say what happened"
         (search "check failed"
                 (ephinea-ta-client::server-status-error-text
                  (make-condition 'simple-error
                                  :format-control "boom"))))
  ;; Token paste normalization (browser copies drag whitespace along).
  (check "normalize-token trims spaces and CRLF"
         (string= "eta_abc123"
                  (normalize-token (format nil "  eta_abc123~c~c" #\Return #\Linefeed))))
  (check "normalize-token trims tabs"
         (string= "eta_abc123"
                  (normalize-token (format nil "~ceta_abc123~c" #\Tab #\Tab))))
  (check "normalize-token maps nil to empty"
         (string= "" (normalize-token nil)))
  (check "normalize-token keeps empty empty"
         (string= "" (normalize-token "   ")))
  ;; Token check errors read like sentences too.
  (check "token transport failure reads like a sentence"
         (search "could not connect"
                 (ephinea-ta-client::token-status-error-text
                  (make-condition 'ephinea-ta-client::api-error
                                  :message "WinHttpConnect failed (Windows error 12029)")))))

;;; ------------------------------------------------------------------
;;; Self-update: version compare, release JSON, zip check, helper script
;;; ------------------------------------------------------------------

(defparameter *release-json-sample*
  "{\"tag_name\": \"v0.6.0\",
    \"prerelease\": false,
    \"assets\": [
      {\"name\": \"notes.txt\",
       \"size\": 12,
       \"browser_download_url\": \"https://example.com/notes.txt\"},
      {\"name\": \"EphineaTAClient.zip\",
       \"size\": 12345678,
       \"browser_download_url\": \"https://github.com/x/y/releases/download/v0.6.0/EphineaTAClient.zip\"},
      {\"name\": \"RappyRunsClient.zip\",
       \"size\": 12345678,
       \"browser_download_url\": \"https://github.com/x/y/releases/download/v0.6.0/RappyRunsClient.zip\"}]}")

(defun run-updater-tests ()
  (format t "~&--- updater ---~%")
  ;; Version parsing: strict X.Y.Z, malformed tags never update.
  (check "parse-version reads v-prefixed tags"
         (equal '(1 2 3) (parse-version "v1.2.3")))
  (check "parse-version reads bare versions"
         (equal '(0 10 0) (parse-version "0.10.0")))
  (check "parse-version rejects two components"
         (null (parse-version "v1.2")))
  (check "parse-version rejects four components"
         (null (parse-version "v1.2.3.4")))
  (check "parse-version rejects suffixes"
         (null (parse-version "v1.2.3-rc1")))
  (check "parse-version rejects words and empties"
         (notany #'parse-version (list "abc" "" "v" "v.." nil 42)))
  ;; Comparison is numeric, not textual.
  (check "version< compares components as numbers"
         (and (version< '(0 9 9) '(0 10 0))
              (not (version< '(0 10 0) '(0 9 9)))))
  (check "version< is false on equal versions"
         (not (version< '(1 2 3) '(1 2 3))))
  (check "version< orders on the major first"
         (version< '(1 9 9) '(2 0 0)))
  ;; update-available-p falls on the do-not-update side.
  (check "update available for a newer tag"
         (update-available-p "0.5.0" "v0.6.0"))
  (check "no update for the same tag"
         (not (update-available-p "0.6.0" "v0.6.0")))
  (check "no update when the current version is nil (dev)"
         (not (update-available-p nil "v0.6.0")))
  (check "no update when the latest tag is malformed"
         (not (update-available-p "0.5.0" "release-2")))
  ;; The pre-GUI startup pass: only :apply downloads before the main
  ;; window shows; everything else starts the client normally.
  (let ((release (parse-release-json *release-json-sample*)))
    (check "startup decision applies a newer release"
           (eq :apply (startup-update-decision release "0.5.0" t)))
    (check "startup decision defers to the manual page when not writable"
           (eq :not-writable (startup-update-decision release "0.5.0" nil)))
    (check "startup decision is up-to-date on the same version"
           (eq :up-to-date (startup-update-decision release "0.6.0" t)))
    (check "startup decision never updates a dev build"
           (eq :up-to-date (startup-update-decision release nil t))))
  (check "startup decision reports a failed release check"
         (eq :check-failed (startup-update-decision nil "0.5.0" t)))
  ;; Release JSON -> plist.
  (let ((release (parse-release-json *release-json-sample*)))
    (check "release json yields the tag"
           (equal "v0.6.0" (getf release :tag)))
    (check "release json picks the client zip asset, not other assets"
           (search "download/v0.6.0/RappyRunsClient.zip"
                   (getf release :asset-url)))
    (check "release json carries the asset size"
           (eql 12345678 (getf release :asset-size))))
  (check "release without the zip asset is ignored"
         (null (parse-release-json
                "{\"tag_name\": \"v0.6.0\", \"assets\": [{\"name\": \"other.zip\", \"browser_download_url\": \"https://x/o.zip\"}]}")))
  (check "empty and malformed responses are ignored"
         (notany #'parse-release-json
                 (list "{}" "" "not json" "{\"assets\": []}")))
  ;; Downloaded zip verification: size and PK magic.
  (let ((path (merge-pathnames "eta-test-update.zip"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
             (write-sequence #(80 75 3 4 9 9 9 9) out))
           (check "zip check passes on matching size and magic"
                  (valid-update-zip-p path 8))
           (check "zip check passes without an expected size"
                  (valid-update-zip-p path nil))
           (check "zip check fails on a size mismatch"
                  (not (valid-update-zip-p path 7)))
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
             (write-sequence #(60 104 116 109 108 62 10 10) out))
           (check "zip check fails on an html error page"
                  (not (valid-update-zip-p path 8))))
      (ignore-errors (delete-file path))))
  (check "zip check fails on a missing file"
         (not (valid-update-zip-p
               (merge-pathnames "eta-no-such-file.zip"
                                (uiop:temporary-directory))
               nil)))
  ;; The helper script: staging, verification, rollback, quoting. The
  ;; exe paths mimic a pre-rename install so the rename-migration side
  ;; (running EphineaTAClient.exe, installing RappyRunsClient.exe) is
  ;; pinned too.
  (let ((script (updater-script-text
                 :pid 4242
                 :exe-path "C:\\Program Files\\Ephinea TA\\EphineaTAClient.exe"
                 :target-exe-path "C:\\Program Files\\Ephinea TA\\RappyRunsClient.exe"
                 :install-dir "C:\\Program Files\\Ephinea TA\\"
                 :zip-path "C:\\Temp\\RappyRunsClient-update.zip"
                 :stage-dir "C:\\Temp\\rappyruns-update-stage\\"
                 :log-path "C:\\Temp\\it's a log.txt")))
    (check "script waits for the old process"
           (and (search "Wait-Process -Id 4242" script)
                (search "Get-Process -Id 4242" script)))
    (check "script stages the zip before touching the install"
           (let ((expand (search "Expand-Archive" script))
                 (move (search "Move-Item -Force $exe $old" script)))
             (and expand move (< expand move))))
    (check "script verifies the staged exe"
           (search "no RappyRunsClient.exe in the update zip" script))
    (check "script installs under the canonical exe name"
           (and (search "$target = 'C:\\Program Files\\Ephinea TA\\RappyRunsClient.exe'"
                        script)
                (search "Copy-Item $newExe $target -Force" script)))
    (check "script rolls the .old exe back on failure"
           (search "Move-Item $old $exe" script))
    (check "a failed rename migration drops the half-installed new exe"
           (let ((remove (search "Remove-Item -Force $target" script))
                 (rollback (search "Move-Item $old $exe" script)))
             (and remove rollback (< remove rollback))))
    (check "script restarts the new exe"
           (search "Start-Process -FilePath $target" script))
    (check "a failed update restarts the old exe"
           (search "Start-Process -FilePath $exe" script))
    (check "script updates the data folder"
           (search "Join-Path $stage 'data'" script))
    (check "script treats ffmpeg as best effort"
           (search "ffmpeg update skipped" script))
    (check "paths with spaces are single-quoted"
           (search "'C:\\Program Files\\Ephinea TA\\EphineaTAClient.exe'"
                   script))
    (check "embedded quotes in paths are doubled"
           (search "'C:\\Temp\\it''s a log.txt'" script))
    (check "the running exe is never deleted, only moved"
           (not (search "Remove-Item -Force $exe" script)))))

;;; ------------------------------------------------------------------
;;; Config migration (the RappyRuns rename retargets the old default
;;; server URL; custom URLs and everything else pass through)
;;; ------------------------------------------------------------------

(defun run-config-migration-tests ()
  (format t "~&--- config migration ---~%")
  (let ((migrated (ephinea-ta-client::migrate-config
                   (list :server-url ephinea-ta-client::+old-default-server-url+
                         :api-token "eta_x"))))
    (check "old default server URL is retargeted"
           (equal (getf ephinea-ta-client::*default-config* :server-url)
                  (getf migrated :server-url)))
    (check "other keys survive the migration"
           (equal "eta_x" (getf migrated :api-token))))
  (check "a custom server URL is never touched"
         (equal "http://localhost:8080"
                (getf (ephinea-ta-client::migrate-config
                       (list :server-url "http://localhost:8080"))
                      :server-url)))
  (check "the dropped token-prompt-shown key is scrubbed"
         (let ((migrated (ephinea-ta-client::migrate-config
                          (list :token-prompt-shown t :auto-submit t))))
           (and (null (getf migrated :token-prompt-shown))
                (getf migrated :auto-submit))))
  ;; The default recordings folder rename (Videos/EphineaTA -> RappyRuns).
  (check "a fresh install uses the new recordings folder"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice nil nil)))
  (check "only the pre-rename folder present triggers the migration"
         (eq :migrate (ephinea-ta-client::default-record-dir-choice t nil)))
  (check "an already-migrated install stays on the new folder"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice nil t)))
  (check "both folders present never renames onto the existing one"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice t t))))

;;; ------------------------------------------------------------------
;;; Authenticode trust policy (the Win32 verification itself is
;;; LispWorks-only; the decision and the GUI label are pure)
;;; ------------------------------------------------------------------

(defun run-signature-policy-tests ()
  (format t "~&--- signature policy ---~%")
  (check "the official signer is accepted"
         (psobb-signature-trusted-p :valid "Terry Chatman"))
  (check "a valid signature from an unknown signer is refused"
         (not (psobb-signature-trusted-p :valid "Mallory")))
  (check "an unsigned exe is refused"
         (not (psobb-signature-trusted-p :unsigned nil)))
  (check "a broken signature is refused even with the right name"
         (not (psobb-signature-trusted-p :invalid "Terry Chatman")))
  (check "a missing signer name is refused"
         (not (psobb-signature-trusted-p :valid nil)))
  (check "the rejection label names an untrusted signer"
         (let ((ephinea-ta-client::*language* :en))
           (search "Mallory"
                   (ephinea-ta-client::signature-status-label
                    '(:pid 1 :status :valid :signer "Mallory")))))
  (check "the rejection label explains an unsigned exe"
         (let ((ephinea-ta-client::*language* :en))
           (string= "no signature"
                    (ephinea-ta-client::signature-status-label
                     '(:pid 1 :status :unsigned)))))
  (check "any other status reads as unverifiable"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "署名を検証できません"
                    (ephinea-ta-client::signature-status-label
                     '(:pid 1 :status :invalid))))))

;;; ------------------------------------------------------------------
;;; i18n: the UI string table and TR
;;; ------------------------------------------------------------------

(defun run-i18n-tests ()
  (format t "~&--- i18n ---~%")
  (check "every key has an English and a Japanese string"
         (loop :for (key entry) :on ephinea-ta-client::*strings* :by #'cddr
               :always (and (keywordp key)
                            (= 2 (length entry))
                            (stringp (first entry))
                            (stringp (second entry)))))
  (check "default language is English"
         (string= "Settings" (ephinea-ta-client::tr :tab-settings)))
  (check "tr switches to Japanese"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "設定" (ephinea-ta-client::tr :tab-settings))))
  (check "tr formats arguments"
         (search "teapot" (ephinea-ta-client::tr :token-ok "teapot")))
  (check "labels follow the language too"
         (let ((ephinea-ta-client::*language* :ja))
           (string= "保存済み" (ephinea-ta-client::run-video-label
                                (list :video-path "v.mp4")))))
  (check "an invalid configured language falls back to English"
         (and (eq :en (ephinea-ta-client::valid-language "nonsense"))
              (eq :ja (ephinea-ta-client::valid-language :ja))))
  ;; Directive mismatches between the two columns would error at
  ;; runtime in one language only; format every entry in both.
  (check "every entry formats cleanly in both languages"
         (loop :for language :in ephinea-ta-client::*languages*
               :always (let ((ephinea-ta-client::*language* language))
                         (loop :for (key entry) :on ephinea-ta-client::*strings*
                               :by #'cddr
                               :always (stringp
                                        (ignore-errors
                                          (ephinea-ta-client::tr key 1 2 3))))))))

;;; ------------------------------------------------------------------
;;; Automatic upload queue: candidate selection, backoff and labels
;;; ------------------------------------------------------------------

(defun run-upload-queue-tests ()
  (format t "~&--- automatic upload queue ---~%")
  (let ((video (merge-pathnames (format nil "eta-test-video-~d.mp4"
                                        (get-internal-real-time))
                                (uiop:temporary-directory)))
        (now (get-universal-time)))
    (with-open-file (out video :direction :output :if-exists :supersede)
      (write-string "not really mp4" out))
    (unwind-protect
         (let ((path (namestring video)))
           ;; *RUNS* is newest first; the candidate scan wants the oldest.
           (with-test-store ((list :status :submitted :server-id 3
                                   :video-path path)
                             (list :status :submitted :server-id 2
                                   :video-path path :video-attached t)
                             (list :status :submitted :server-id 1
                                   :video-path path))
             (check "the oldest unattached recording uploads first"
                    (eql 1 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path path
                                   :next-upload-at (+ now 900)))
             (check "a backing-off entry is skipped"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id)))
             (check "the backoff expires with time"
                    (eql 1 (getf (ephinea-ta-client::upload-candidate
                                  :now (+ now 1000))
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 1
                                   :video-path path :upload-given-up t))
             (check "a given-up entry is never a candidate"
                    (null (ephinea-ta-client::upload-candidate :now now))))
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path "C:/nowhere/gone.mp4"))
             (check "a vanished recording gives up and the scan moves on"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id)))
             (check "the vanished entry is marked given up"
                    (getf (find 1 (queued-runs)
                                :key (lambda (entry) (getf entry :server-id)))
                          :upload-given-up)))
           (with-test-store ((list :status :queued :video-path path))
             (check "entries without a server draft cannot upload yet"
                    (null (ephinea-ta-client::upload-candidate :now now))))
           ;; Aborted runs keep their recording locally but never
           ;; auto-upload it (reset-farming would flood hosted storage).
           (with-test-store ((list :status :submitted :server-id 2
                                   :video-path path)
                             (list :status :submitted :server-id 1
                                   :video-path path :aborted t))
             (check "an aborted run's recording never uploads"
                    (eql 2 (getf (ephinea-ta-client::upload-candidate
                                  :now now)
                                 :server-id))))
           (with-test-store ((list :status :submitted :server-id 1
                                   :video-path path :aborted t))
             (check "an aborted-only queue has no upload candidate"
                    (null (ephinea-ta-client::upload-candidate :now now)))))
      (ignore-errors (delete-file video))))
  ;; The upload URL carries the recorder's video offset when known.
  (check "video-file url carries the offset when known"
         (equal "/api/runs/7/video-file?offset_ms=1234"
                (ephinea-ta-client::video-file-path 7 1234)))
  (check "video-file url omits the offset when unknown"
         (equal "/api/runs/7/video-file"
                (ephinea-ta-client::video-file-path 7 nil)))
  ;; Given-up entries are finished: trimmed, not persisted.
  (check "a given-up upload is no longer active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 1 :video-path "v.mp4"
                     :upload-given-up t))))
  (check "an aborted run with a pending video is not active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 1 :video-path "v.mp4"
                     :aborted t))))
  (check "status label: aborted drafts say the recording stays local"
         (search "aborted" (ephinea-ta-client::run-status-label
                            (list :status :submitted :aborted t
                                  :video-path "v.mp4"))))
  ;; Labels around the upload lifecycle.
  (let ((ephinea-ta-client::*upload-progress* (list 7 50 200)))
    (check "video label: in-flight upload shows its percent"
           (search "25%" (ephinea-ta-client::run-video-label
                          (list :server-id 7 :video-path "v.mp4"))))
    (check "video label: other entries are not uploading"
           (equal "saved" (ephinea-ta-client::run-video-label
                           (list :server-id 8 :video-path "v.mp4")))))
  (check "video label: uploaded"
         (equal "uploaded" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t
                                  :video-uploaded t))))
  (check "video label: manual attach still reads attached"
         (equal "attached" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t))))
  (check "video label: given up"
         (equal "upload failed" (ephinea-ta-client::run-video-label
                                 (list :video-path "v.mp4"
                                       :upload-given-up t)))))

;;; ------------------------------------------------------------------
;;; local recordings storage budget (recording.lisp + store.lisp)
;;; ------------------------------------------------------------------

(defun run-retention-tests ()
  (format t "~&--- local storage budget ---~%")
  (flet ((evict (files cap &rest kw)
           (apply #'ephinea-ta-client::recordings-to-evict files cap kw)))
    ;; (namestring size-bytes write-date)
    (let ((files '(("a.mp4" 500 100) ("b.mp4" 500 200) ("c.mp4" 500 300))))
      (check "no cap set: nothing is evicted"
             (null (evict files nil)))
      (check "under the cap: nothing is evicted"
             (null (evict files 2000)))
      (check "over the cap: the oldest go until back under"
             (equal '("a.mp4") (evict files 1200)))
      (check "eviction keeps going until the total fits"
             (equal '("a.mp4" "b.mp4") (evict files 600)))
      (check "protected files are never evicted"
             (equal '("b.mp4" "c.mp4")
                    (evict files 100 :protected '("a.mp4"))))
      (check "when only protected files remain, eviction stops short"
             (equal '("c.mp4")
                    (evict files 100 :protected '("a.mp4" "b.mp4"))))
      (check "uploaded files are reclaimed before the rest"
             ;; c is newest but uploaded, so it goes before older a/b.
             (equal '("c.mp4") (evict files 1200 :uploaded '("c.mp4"))))
      (check "uploaded first, then oldest-first among the rest"
             (equal '("c.mp4" "a.mp4")
                    (evict files 600 :uploaded '("c.mp4"))))))
  ;; The queue drives which on-disk files are protected vs reclaimable.
  (with-test-store ((list :status :submitted :server-id 3
                          :video-path "up.mp4" :video-attached t)
                    (list :status :submitted :server-id 2
                          :video-path "pending.mp4")
                    (list :status :submitted :server-id 1
                          :video-path "aborted.mp4" :aborted t))
    (multiple-value-bind (protected uploaded)
        (ephinea-ta-client::video-path-retention-sets)
      (check "a file awaiting upload is protected"
             (member "pending.mp4" protected :test #'equal))
      (check "an uploaded file is reclaimable first"
             (member "up.mp4" uploaded :test #'equal))
      (check "an aborted run's video is neither protected nor uploaded"
             (and (not (member "aborted.mp4" protected :test #'equal))
                  (not (member "aborted.mp4" uploaded :test #'equal))))))
  ;; End to end: the sweep reaps the uploaded file first, spares the
  ;; pending one, and reaches the unmatched file only when still over.
  (let* ((backend (make-instance 'mock-backend))
         (recorder (make-recorder :backend backend))
         (byte-cap (lambda (bytes) (/ bytes (* 1024 1024 1024)))))
    (setf (mock-recordings backend)
          '(("up.mp4" 500 100) ("pending.mp4" 500 200) ("orphan.mp4" 500 300)))
    (with-recording-config (:record-max-total-gb (funcall byte-cap 400))
      (with-test-store ((list :status :submitted :server-id 2
                              :video-path "up.mp4" :video-attached t)
                        (list :status :submitted :server-id 1
                              :video-path "pending.mp4"))
        (ephinea-ta-client::apply-recording-retention recorder)
        (let ((deleted (mapcar #'second (events-of backend :delete))))
          (check "uploaded and orphan files are reaped, pending is spared"
                 (equal '("up.mp4" "orphan.mp4") deleted))))))
  ;; A capture in progress means the disk is busy; the sweep waits.
  (let* ((backend (make-instance 'mock-backend))
         (recorder (make-recorder :backend backend :state :recording)))
    (setf (mock-recordings backend) '(("a.mp4" 500 100)))
    (with-recording-config (:record-max-total-gb (/ 1 (* 1024 1024 1024)))
      (with-test-store ()
        (ephinea-ta-client::apply-recording-retention recorder)
        (check "no sweep runs while a capture is in progress"
               (null (events-of backend :delete)))))))

;;; ------------------------------------------------------------------
;;; login.txt credentials parsing
;;; ------------------------------------------------------------------

(defun run-credentials-tests ()
  (format t "~&--- login.txt credentials ---~%")
  (multiple-value-bind (username password)
      (parse-credentials (format nil "username=Teapot~%password=secret123~%"))
    (check "plain username= and password= lines parse"
           (and (equal "Teapot" username) (equal "secret123" password))))
  (multiple-value-bind (username password)
      (parse-credentials
       (format nil "~a# comment~a~%  username = Teapot ~a~%password=a=b=c~a~%"
               (code-char #xFEFF) #\Return #\Return #\Return))
    (check "BOM, CRLF, comments and spaces around keys are tolerated"
           (and (equal "Teapot" username) (equal "a=b=c" password))))
  (check "a missing password yields NIL NIL"
         (null (parse-credentials (format nil "username=Teapot~%"))))
  (check "empty values yield NIL NIL"
         (null (parse-credentials
                (format nil "username=Teapot~%password=~%"))))
  (check "empty text yields NIL NIL"
         (null (parse-credentials "")))
  (check "NIL text yields NIL NIL"
         (null (parse-credentials nil)))
  (let ((path (merge-pathnames (format nil "eta-test-login-~d.txt"
                                       (get-internal-real-time))
                               (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out "username=Teapot~%password=secret123~%"))
    (unwind-protect
         (multiple-value-bind (username password) (read-credentials path)
           (check "read-credentials reads a real file"
                  (and (equal "Teapot" username)
                       (equal "secret123" password))))
      (ignore-errors (delete-file path)))
    (check "read-credentials on a missing file yields NIL NIL"
           (null (read-credentials path)))))

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
  (run-detect-telemetry-tests)
  (run-server-defs-tests)
  (run-gdv-segment-test)
  (run-trigger-log-tests)
  (run-recorder-tests)
  (run-video-flow-tests)
  (run-upload-queue-tests)
  (run-retention-tests)
  (run-ux-helper-tests)
  (run-updater-tests)
  (run-config-migration-tests)
  (format t "~&=== client tests: ~d failure~:p ===~%" *failures*)
  *failures*)
