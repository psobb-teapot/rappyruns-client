(in-package :ephinea-ta-client-tests)

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
  ;; The batched HP-table read behind READ-MONSTERS.
  (let ((reader (make-monster-reader)))
    (check "hp block covers the live id span in one read"
           (multiple-value-bind (bytes low)
               (ephinea-ta-client::monster-hp-block reader +hp-table-base+
                                                    '(7 8))
             (and bytes (= 7 low) (= 64 (length bytes)))))
    (check "hp block refuses a garbage id span"
           (null (ephinea-ta-client::monster-hp-block reader +hp-table-base+
                                                      '(0 60000))))
    (check "hp block absent without a table"
           (null (ephinea-ta-client::monster-hp-block reader 0 '(7 8)))))
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

