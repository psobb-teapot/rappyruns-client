(in-package :ephinea-ta-client)

;;; PSOBB memory layout, transcribed from psostats-client (MIT,
;;; https://github.com/phelix-/psostats-client) client/internal/pso/.
;;; All addresses are for the 32-bit Ephinea PSOBB client and are
;;; deliberately collected in this one file.

;; Global addresses
(defconstant +base-player-array+ #x00A94254)  ; 12 pointers, 4 bytes each
(defconstant +my-player-index+ #x00A9C4F4)    ; u8
(defconstant +episode-address+ #x00A9B1C8)    ; u16, 0-based; 2 means ep4
(defconstant +quest-pointer+ #x00A95AA8)      ; u32 -> quest struct
(defconstant +floor-switches+ #x00AC9FA0)     ; 32 bytes per floor
(defconstant +difficulty-address+ #x00A9CD68) ; u16, 0=Normal .. 3=Ultimate
(defconstant +current-map-address+ #x00AAFC9C) ; u16, map number (floor names)

;; Inventory (item array is the local player's inventory)
(defconstant +item-array-pointer+ #x00A8D81C) ; u32 -> array of item pointers
(defconstant +item-array-count+ #x00A8D820)   ; u32
(defconstant +pmt-pointer+ #x00A8DC94)        ; u32 -> ItemPMT tables
(defconstant +unitxt-pointer+ #x00A9CD50)     ; u32 -> unitxt string tables
(defconstant +item-id-offset+ #xD8)           ; 2 x u16, printed high:low
(defconstant +item-owner-offset+ #xE4)        ; u8, player index
(defconstant +item-type-offset+ #xF2)         ; u8
(defconstant +item-group-offset+ #xF3)        ; u8
(defconstant +item-index-offset+ #xF4)        ; u8, index within group
(defconstant +item-tool-count-offset+ #x104)  ; u8 XOR low byte of its address
(defconstant +item-equipped-offset+ #x190)    ; u8, bit 0
(defconstant +item-mag-stats-offset+ #x1C0)   ; 4 x u16 (def/pow/dex/mind x100)
(defconstant +item-wep-stats-offset+ #x1C8)   ; 3 x (u8 area, i8 percent)
(defconstant +item-arm-slots-offset+ #x1B8)   ; u8
(defconstant +item-frame-dfp-offset+ #x1B9)   ; u8
(defconstant +item-frame-evp-offset+ #x1BA)   ; u8
(defconstant +item-barrier-dfp-offset+ #x1E4) ; u8
(defconstant +item-barrier-evp-offset+ #x1E5) ; u8
(defconstant +item-wep-grind-offset+ #x1F5)   ; u8
(defconstant +item-wep-special-offset+ #x1F6) ; u8

;; Monsters (entity array shared with players)
(defconstant +npc-array-pointer+ #x007B4BA2)  ; u32 -> entity pointer array
(defconstant +npc-count-address+ #x00AAE164)  ; u32
(defconstant +player-count-address+ #x00AAE168) ; u32
(defconstant +ephinea-monster-hp-table+ #x00B5F800) ; u32 -> hp table, stride 32
(defconstant +monster-id-offset+ #x1C)        ; u16
(defconstant +monster-unitxt-offset+ #x378)   ; u32, 0 = not a monster
(defconstant +monster-hp-offset+ #x334)       ; u16 (fallback, non-Ephinea)

;; Offsets within the quest struct / quest data block
(defconstant +quest-data-offset+ #x19C)       ; u32 -> quest data block
(defconstant +quest-register-offset+ #x2C)    ; u32 -> u16 registers, stride 4
(defconstant +quest-number-offset+ #x10)      ; u16 within data block
(defconstant +quest-name-offset+ #x18)        ; UTF-16, 32 chars max

;; Offsets within a player struct. READ-PLAYER fetches the whole block
;; [+player-block-start+, +player-block-end+) in one ReadProcessMemory
;; call and decodes from the bytes.
(defconstant +player-block-start+ #x028)
(defconstant +player-block-end+ #xE50)
(defconstant +player-room-offset+ #x028)      ; u16
(defconstant +player-x-offset+ #x038)         ; f32
(defconstant +player-y-offset+ #x03C)         ; f32
(defconstant +player-z-offset+ #x040)         ; f32
(defconstant +player-facing-offset+ #x060)    ; u16
(defconstant +player-shifta-offset+ #x278)    ; f32 multiplier, 0 = none
(defconstant +player-deband-offset+ #x284)    ; f32 multiplier, 0 = none
(defconstant +player-max-hp-offset+ #x2BC)    ; u16
(defconstant +player-max-tp-offset+ #x2BE)    ; u16
(defconstant +player-hp-offset+ #x334)        ; u16
(defconstant +player-tp-offset+ #x336)        ; u16
(defconstant +player-state-offset+ #x33E)     ; u16 bitfield, #x04 = warping
(defconstant +player-action-state-offset+ #x348) ; u16, see +action-states+
(defconstant +player-floor-offset+ #x3F0)     ; u16
(defconstant +player-name-offset+ #x428)      ; UTF-16, 12 chars
(defconstant +player-current-tech-offset+ #x464) ; u16, see +tech-names+
(defconstant +player-pb-offset+ #x520)        ; f32, photon blast charge
(defconstant +player-invincibility-offset+ #x720) ; u32, frames remaining
(defconstant +player-damage-traps-offset+ #x89C)  ; u8
(defconstant +player-freeze-traps-offset+ #x89D)  ; u8
(defconstant +player-confuse-traps-offset+ #x89F) ; u8
(defconstant +player-guild-card-offset+ #x930)    ; 8 ASCII bytes (Ephinea)
(defconstant +player-class-offset+ #x960)     ; u16, class in bits 8-11,
                                              ; section id in bits 0-7
(defconstant +player-level-offset+ #xE44)     ; u16, 0-based
(defconstant +player-meseta-offset+ #xE4C)    ; u32

(defconstant +floor-count+ 18)                ; floors 0-17 (tower)
(defconstant +register-count+ 256)

(defparameter +class-ids+
  '((#x00 . "HUmar") (#x01 . "HUnewearl") (#x02 . "HUcast") (#x09 . "HUcaseal")
    (#x03 . "RAmar") (#x0B . "RAmarl") (#x04 . "RAcast") (#x05 . "RAcaseal")
    (#x0A . "FOmar") (#x06 . "FOmarl") (#x07 . "FOnewm") (#x08 . "FOnewearl")))

(defun class-name-for-id (id)
  (cdr (assoc id +class-ids+)))

(defparameter +section-ids+
  #("Viridia" "Greenill" "Skyly" "Bluefull" "Purplenum"
    "Pinkal" "Redria" "Oran" "Yellowboze" "Whitill"))

(defun section-name-for-id (id)
  (when (and (integerp id) (< -1 id (length +section-ids+)))
    (aref +section-ids+ id)))

(defparameter +difficulty-names+
  #("Normal" "Hard" "Very Hard" "Ultimate"))

(defun difficulty-name (id)
  (when (and (integerp id) (< -1 id (length +difficulty-names+)))
    (aref +difficulty-names+ id)))

(defparameter +tech-names+
  '((#x0000 . "Foie") (#x0001 . "Gifoie") (#x0002 . "Rafoie")
    (#x0003 . "Barta") (#x0004 . "Gibarta") (#x0005 . "Rabarta")
    (#x0006 . "Zonde") (#x0007 . "Gizonde") (#x0008 . "Razonde")
    (#x0009 . "Grants") (#x0012 . "Megid")
    (#x000A . "Deband") (#x000B . "Jellen") (#x000C . "Zalure")
    (#x000D . "Shifta") (#x000E . "Ryuker") (#x000F . "Resta")
    (#x0010 . "Anti") (#x0011 . "Reverser")))

(defun tech-name (id)
  (cdr (assoc id +tech-names+)))

(defun shifta-level (multiplier)
  "Shifta/Deband multiplier -> level, matching psostats
getSDLvlFromMultiplier (0 when the buff is down, negative for
Jellen/Zalure)."
  (if (or (null multiplier) (zerop multiplier))
      0
      (let ((level (1+ (floor (+ (/ (- (* (abs multiplier) 100) 10) 1.3)
                                 0.5)))))
        (if (minusp multiplier) (- level) level))))

(defun strip-name-prefix (name)
  ;; Character names are prefixed with a "\tE" language marker.
  (if (and (>= (length name) 2)
           (char= (char name 0) #\Tab))
      (subseq name 2)
      name))

(defun decode-ascii-z (bytes)
  "NUL-terminated printable-ASCII string from BYTES (guild card numbers)."
  (with-output-to-string (out)
    (loop :for byte :across bytes
          :until (zerop byte)
          :when (<= 32 byte 126)
            :do (write-char (code-char byte) out))))

(defun read-player (reader address)
  "Decode one player struct from a single block read; NIL when unreadable."
  (let ((block (read-block reader (+ address +player-block-start+)
                           (- +player-block-end+ +player-block-start+))))
    (when block
      (flet ((u8 (offset) (aref block (- offset +player-block-start+)))
             (u16 (offset) (bytes-u16 block (- offset +player-block-start+)))
             (u32 (offset) (bytes-u32 block (- offset +player-block-start+)))
             (f32 (offset) (u32-float
                            (bytes-u32 block (- offset +player-block-start+))))
             (bytes-at (offset length)
               (subseq block (- offset +player-block-start+)
                       (+ (- offset +player-block-start+) length))))
        (let ((class-bits (u16 +player-class-offset+))
              (state-bits (u16 +player-state-offset+))
              (guild-card (string-trim
                           " " (decode-ascii-z
                                (bytes-at +player-guild-card-offset+ 8)))))
          (list :name (strip-name-prefix
                       (decode-utf16-z (bytes-at +player-name-offset+ 24)))
                :class (class-name-for-id (ash (logand class-bits #xF00) -8))
                :section-id (section-name-for-id (logand class-bits #xFF))
                :level (1+ (u16 +player-level-offset+))
                :guild-card (and (string/= guild-card "") guild-card)
                :floor (u16 +player-floor-offset+)
                :room (u16 +player-room-offset+)
                :x (f32 +player-x-offset+)
                :y (f32 +player-y-offset+)
                :z (f32 +player-z-offset+)
                :facing (u16 +player-facing-offset+)
                :warping (plusp (logand state-bits #x04))
                :state (u16 +player-action-state-offset+)
                :current-tech (u16 +player-current-tech-offset+)
                :hp (u16 +player-hp-offset+)
                :max-hp (u16 +player-max-hp-offset+)
                :tp (u16 +player-tp-offset+)
                :max-tp (u16 +player-max-tp-offset+)
                :meseta (u32 +player-meseta-offset+)
                :pb (f32 +player-pb-offset+)
                :shifta (shifta-level (f32 +player-shifta-offset+))
                :deband (shifta-level (f32 +player-deband-offset+))
                :invincible (plusp (u32 +player-invincibility-offset+))
                :damage-traps (u8 +player-damage-traps-offset+)
                :freeze-traps (u8 +player-freeze-traps-offset+)
                :confuse-traps (u8 +player-confuse-traps-offset+)))))))

(defun read-players (reader)
  "Players in the current game with their party slot order preserved."
  (loop :for i :from 0 :below 12
        :for pointer := (read-u32 reader (+ +base-player-array+ (* 4 i)))
        :when (and pointer (plusp pointer))
          :collect (let ((player (read-player reader pointer)))
                     (when player (list* :index i player)))
            :into players
        :finally (return (remove nil players))))

(defun read-episode (reader)
  (let ((raw (read-u16 reader +episode-address+)))
    (case (and raw (1+ raw))
      (3 4)
      ((1 2 4) (1+ raw))
      (t nil))))

(defun read-snapshot (reader)
  "One frame of game state as a plist; NIL if the process is unreadable.
Trigger evaluation happens on the snapshot (see SNAPSHOT-REGISTER-SET-P),
so the detection state machine stays pure and testable."
  (let ((my-index (read-u8 reader +my-player-index+)))
    (when my-index
      (let* ((episode (read-episode reader))
             (players (read-players reader))
             (quest-ptr (read-u32 reader +quest-pointer+))
             (snapshot (list :episode episode
                             :my-index my-index
                             :players players
                             :difficulty (read-u16 reader +difficulty-address+)
                             :map (read-u16 reader +current-map-address+)
                             :quest-ptr (or quest-ptr 0))))
        (when (and quest-ptr (plusp quest-ptr))
          (let* ((data-ptr (read-u32 reader (+ quest-ptr +quest-data-offset+)))
                 (register-ptr (read-u32 reader (+ quest-ptr +quest-register-offset+)))
                 (quest-name (and data-ptr (plusp data-ptr)
                                  (read-utf16-string
                                   reader (+ data-ptr +quest-name-offset+) 64)))
                 (quest-number (and data-ptr (plusp data-ptr)
                                    (read-u16 reader (+ data-ptr +quest-number-offset+)))))
            (setf snapshot
                  (append snapshot
                          (list :quest-name (and quest-name
                                                 (string-trim " " quest-name))
                                :quest-number quest-number
                                :registers (and register-ptr (plusp register-ptr)
                                                (read-block reader register-ptr
                                                            (* 4 +register-count+)))
                                :floor-switches (read-block
                                                 reader +floor-switches+
                                                 (* 32 +floor-count+)))))))
        snapshot))))

(defun snapshot-register-set-p (snapshot register-id)
  "Quest register REGISTER-ID holds a non-zero u16 (psostats IsRegisterSet)."
  (let ((registers (getf snapshot :registers)))
    (and registers
         (< (+ (* 4 register-id) 1) (length registers))
         (plusp (bytes-u16 registers (* 4 register-id))))))

(defun snapshot-floor-switch-set-p (snapshot floor switch-id)
  "Floor switch bit, matching psostats getFloorSwitch bit layout."
  (let ((switches (getf snapshot :floor-switches))
        (offset (+ (* 32 floor) (floor switch-id 8))))
    (and switches
         (< offset (length switches))
         (plusp (logand (aref switches offset)
                        (ash #x80 (- (mod switch-id 8))))))))

(defun snapshot-my-player (snapshot)
  (find (getf snapshot :my-index) (getf snapshot :players)
        :key (lambda (player) (getf player :index))))

;;; Inventory. Ported from psostats inventory.go: the item array holds
;;; the local inventory; equipped gear is resolved to display names via
;;; the game's own ItemPMT and unitxt tables so no item database ships
;;; with the client.

(defvar *item-name-cache* (make-hash-table :test 'eql)
  "unitxt index -> item name; unitxt data is static for a game session.")

(defun read-item-name (reader unitxt-index)
  (or (gethash unitxt-index *item-name-cache*)
      (let* ((unitxt (read-u32 reader +unitxt-pointer+))
             (names (and unitxt (plusp unitxt) (read-u32 reader (+ unitxt 4))))
             (name-address (and names (plusp names)
                                (read-u32 reader (+ names (* 4 unitxt-index)))))
             (name (and name-address (plusp name-address)
                        (read-utf16-string reader name-address 48))))
        (when (and name (string/= name ""))
          (setf (gethash unitxt-index *item-name-cache*) name)))))

(defun item-pmt-index (reader group index type-offset stride)
  "Index into the unitxt name table for an item, via ItemPMT."
  (let* ((pmt (read-u32 reader +pmt-pointer+))
         (table (and pmt (plusp pmt) (read-u32 reader (+ pmt type-offset)))))
    (when (and table (plusp table))
      (let ((entry (read-u32 reader (+ table (* 8 group) 4))))
        (when (and entry (plusp entry))
          (read-u32 reader (+ entry (* stride index))))))))

(defparameter +weapon-specials+
  #("" "Draw" "Drain" "Fill" "Gush" "Heart" "Mind" "Soul" "Geist"
    "Master's" "Lord's" "King's" "Charge" "Spirit" "Berserk"
    "Ice" "Frost" "Freeze" "Blizzard" "Bind" "Hold" "Seize" "Arrest"
    "Heat" "Fire" "Flame" "Burning" "Shock" "Thunder" "Storm" "Tempest"
    "Dim" "Shadow" "Dark" "Hell" "Panic" "Riot" "Havoc" "Chaos"
    "Devil's" "Demon's"))

(defparameter +srank-specials+
  #("" "Jellen" "Zalure" "HP Regeneration" "TP Regeneration" "Burning"
    "Tempest" "Blizzard" "Arrest" "Chaos" "Hell" "Spirit" "Berserk"
    "Demon's" "Gush" "Geist" "King's"))

(defun special-name (table id)
  (if (and id (< -1 id (length table)))
      (aref table id)
      "?"))

(defun signed-i8 (byte)
  (if (> byte 127) (- byte 256) byte))

(defun weapon-display (reader item-addr group index)
  (let ((name (or (let ((pmt-index (item-pmt-index reader group index #x00 44)))
                    (and pmt-index (read-item-name reader pmt-index)))
                  "?"))
        (grind (read-u8 reader (+ item-addr +item-wep-grind-offset+)))
        (srank (or (<= #x70 group #x88) (<= #xA5 group #xA9)))
        (native 0) (abeast 0) (machine 0) (dark 0) (hit 0))
    (loop :for slot :from 0 :below 3
          :for area := (read-u8 reader (+ item-addr +item-wep-stats-offset+
                                          (* 2 slot)))
          :for percent := (read-u8 reader (+ item-addr +item-wep-stats-offset+
                                             (* 2 slot) 1))
          :when (and area percent)
            :do (let ((value (signed-i8 percent)))
                  (case area
                    (1 (setf native value))
                    (2 (setf abeast value))
                    (3 (setf machine value))
                    (4 (setf dark value))
                    (5 (setf hit value)))))
    (let ((special (if srank
                       (special-name +srank-specials+ index)
                       (special-name
                        +weapon-specials+
                        (read-u8 reader (+ item-addr +item-wep-special-offset+))))))
      (format nil "~a~@[ +~d~]~@[ [~a]~] [~d/~d/~d/~d|~d]"
              name
              (and grind (plusp grind) grind)
              (and (string/= special "") special)
              native abeast machine dark hit))))

(defun armor-display (reader item-addr group index kind)
  "KIND is :frame, :barrier or :unit."
  (let* ((pmt-index (ecase kind
                      ((:frame :barrier) (item-pmt-index reader (1- group) index
                                                         #x04 32))
                      (:unit (item-pmt-index reader 0 index #x08 20))))
         (name (or (and pmt-index (read-item-name reader pmt-index)) "?")))
    (ecase kind
      (:frame (format nil "~a [~d|~d] [~ds]" name
                      (or (read-u8 reader (+ item-addr +item-frame-dfp-offset+)) 0)
                      (or (read-u8 reader (+ item-addr +item-frame-evp-offset+)) 0)
                      (or (read-u8 reader (+ item-addr +item-arm-slots-offset+)) 0)))
      (:barrier (format nil "~a [~d|~d]" name
                        (or (read-u8 reader (+ item-addr +item-barrier-dfp-offset+)) 0)
                        (or (read-u8 reader (+ item-addr +item-barrier-evp-offset+)) 0)))
      (:unit name))))

(defun mag-display (reader item-addr group)
  (let ((name (or (let ((pmt-index (item-pmt-index reader 0 group #x10 28)))
                    (and pmt-index (read-item-name reader pmt-index)))
                  "?")))
    (flet ((stat (slot)
             (floor (or (read-u16 reader (+ item-addr +item-mag-stats-offset+
                                            (* 2 slot)))
                        0)
                    100)))
      (format nil "~a [~d/~d/~d/~d]" name (stat 0) (stat 1) (stat 2) (stat 3)))))

(defun read-equipped-item (reader item-addr)
  "Equipment plist (:id :type :display) for an equipped item, or NIL."
  (let ((word0 (read-u16 reader (+ item-addr +item-id-offset+)))
        (word1 (read-u16 reader (+ item-addr +item-id-offset+ 2)))
        (type (read-u8 reader (+ item-addr +item-type-offset+)))
        (group (read-u8 reader (+ item-addr +item-group-offset+)))
        (index (read-u8 reader (+ item-addr +item-index-offset+))))
    (when (and word0 word1 type group index)
      (let ((id (format nil "~4,'0x~4,'0x" word1 word0)))
        (case type
          (0 (list :id id :type :weapon
                   :display (weapon-display reader item-addr group index)))
          (1 (case group
               (1 (list :id id :type :frame
                        :display (armor-display reader item-addr group index :frame)))
               (2 (list :id id :type :barrier
                        :display (armor-display reader item-addr group index :barrier)))
               (3 (list :id id :type :unit
                        :display (armor-display reader item-addr group index :unit)))))
          (2 (list :id id :type :mag
                   :display (mag-display reader item-addr group))))))))

(defparameter +consumable-keys+
  ;; (group index key) per psostats addConsumableToInventory
  '((0 0 :monomate) (0 1 :dimate) (0 2 :trimate)
    (1 0 :monofluid) (1 1 :difluid) (1 2 :trifluid)
    (3 nil :sol-atomizer) (4 nil :moon-atomizer) (5 nil :star-atomizer)
    (7 nil :telepipe)))

(defun consumable-key (group index)
  (loop :for (want-group want-index key) :in +consumable-keys+
        :when (and (eql group want-group)
                   (or (null want-index) (eql index want-index)))
          :return key))

(defun read-inventory (reader my-index)
  "Equipped gear and consumable counts:
\(:equipment ((:id :type :display) ...) :weapon plist-or-nil
 :consumables (:monomate n ... :telepipe n))."
  (let ((count (read-u32 reader +item-array-count+))
        (array (read-u32 reader +item-array-pointer+)))
    (when (and count array (plusp array) (<= 0 count 60))
      (let ((pointers (read-block reader array (* 4 count)))
            (equipment '())
            (weapon nil)
            (consumables '()))
        (when pointers
          (loop :for i :from 0 :below count
                :for item-addr := (bytes-u32 pointers (* 4 i))
                :when (plusp item-addr)
                  :do (let ((type (read-u8 reader (+ item-addr +item-type-offset+)))
                            (group (read-u8 reader (+ item-addr +item-group-offset+)))
                            (index (read-u8 reader (+ item-addr +item-index-offset+)))
                            (owner (read-u8 reader (+ item-addr +item-owner-offset+)))
                            (equipped (read-u8 reader (+ item-addr
                                                         +item-equipped-offset+))))
                        (cond
                          ((and owner equipped
                                (= owner my-index) (oddp equipped))
                           (let ((item (read-equipped-item reader item-addr)))
                             (when item
                               (push item equipment)
                               (when (eq (getf item :type) :weapon)
                                 (setf weapon item)))))
                          ((eql type 3)
                           (let ((key (consumable-key group index))
                                 (raw (read-u8 reader (+ item-addr
                                                         +item-tool-count-offset+))))
                             (when (and key raw)
                               ;; The count byte is XOR-obfuscated with the
                               ;; low byte of its own address.
                               (setf (getf consumables key)
                                     (logxor raw
                                             (logand (+ item-addr
                                                        +item-tool-count-offset+)
                                                     #xFF)))))))))
          (list :equipment (nreverse equipment)
                :weapon weapon
                :consumables consumables))))))

;;; Monsters. Enough of psostats GetMonsterList to count living enemies
;;; and observe alive->dead transitions (kills); names, positions and
;;; boss-specific HP forms are not read.

(defun read-monsters (reader)
  "List of (:id n :hp n) for monster entities, or NIL when unreadable."
  (let ((array (read-u32 reader +npc-array-pointer+))
        (npc-count (read-u32 reader +npc-count-address+))
        (player-count (read-u32 reader +player-count-address+))
        (hp-table (read-u32 reader +ephinea-monster-hp-table+)))
    (when (and array (plusp array) npc-count player-count
               (<= 0 npc-count 512) (<= 0 player-count 12))
      (let ((pointers (read-block reader (+ array (* 4 player-count))
                                  (* 4 npc-count))))
        (when pointers
          (loop :for i :from 0 :below npc-count
                :for monster-addr := (bytes-u32 pointers (* 4 i))
                :when (plusp monster-addr)
                  :append (let ((unitxt (read-u32 reader (+ monster-addr
                                                            +monster-unitxt-offset+)))
                                (id (read-u16 reader (+ monster-addr
                                                        +monster-id-offset+))))
                            (when (and unitxt (plusp unitxt) id)
                              (let ((hp (if (and hp-table (plusp hp-table))
                                            (read-u16 reader (+ hp-table 4 (* 32 id)))
                                            (read-u16 reader (+ monster-addr
                                                                +monster-hp-offset+)))))
                                ;; HP underflows below zero on kill.
                                (when (and hp (> hp #x8000))
                                  (setf hp 0))
                                (list (list :id id :hp (or hp 0))))))))))))
