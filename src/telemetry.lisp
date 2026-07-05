(in-package :ephinea-ta-client)

;;; Per-quest telemetry, mirroring psostats consolidateFrame: one data
;;; frame per second plus running totals (deaths, item/tech usage, time
;;; by action state, weapon usage, kills). Pure like the detector - it
;;; consumes snapshots; inventory and monster lists ride along in the
;;; snapshot under :inventory / :monsters when the poll loop sampled
;;; them this frame (about once per second).
;;;
;;; Everything captured here ends up inside the queued run plist, so it
;;; only uses printable structure (lists, strings, numbers): the queue
;;; is persisted with WRITE-SEXP-FILE.

(defparameter +frame-keys+
  #("t" "hp" "tp" "pb" "meseta" "floor" "room" "x" "z"
    "shifta" "deband" "inv" "state" "monsters" "kills"
    "map_var" "ft" "dt" "ct" "damage" "weapon" "player_locs" "monster_locs")
  "Column names for the compact per-second frame rows, in order.
player_locs rows are (key floor room x y z facing warping01) lists,
monster_locs rows (id x y z facing hp frozen01 paralyzed01 confused01);
everything else is a number or a string.")

(defparameter +attack-states+ '(5 6 7))
(defconstant +casting-state+ 8)

(defparameter +frame1-threshold-ms+ 60
  "A kill this soon after the spawn sample counts as a frame-1 kill
\(psostats consolidateMonsterState).")

(defparameter +frame1-excluded-unitxt+ '(34 45 73 68)
  "De Rol Le, Barba Ray, Dark Gunner/control forms: buggy spawn data in
psostats, excluded from frame-1 detection there too.")

(defstruct telemetry
  start-time                    ; internal real time of the quest start
  last-tick                     ; internal real time of the previous step
  (frames '())                  ; newest first, rows matching +frame-keys+
  (last-frame-second -1)
  (death-count 0)
  last-hp
  (events '())                  ; newest first: (:t sec :type "death"|"floor" ...)
  last-map
  (meseta-charged 0)
  last-meseta
  last-consumables              ; consumables plist from the last sample
  (items-used '())              ; alist (key . count), keys from +consumable-keys+
  last-traps                    ; (damage freeze confuse)
  (traps-used (list :dt 0 :ft 0 :ct 0))
  (tp-used 0)
  last-tp
  (techs-cast '())              ; alist (name . count)
  last-state
  (time-by-state '())           ; alist (state-id . ms)
  (weapons '())                 ; alist (id . (:display :type :seconds :attacks :techs))
  (current-weapon-id "Bare Handed")
  (monsters (make-hash-table :test 'eql)) ; monster id -> state plist
  (monsters-alive 0)
  (monster-hp-pool 0)           ; sum of living monsters' hp, last sample
  (monster-hp-pool-series '())  ; newest first, one entry per frame
  (kills 0)                     ; monsters seen alive, then dead
  (player-damage '())           ; alist (player-index . hp dealt)
  (last-hits '())               ; alist (player-index . kills credited)
  (bosses '())                  ; alist (monster-id . boss plist), newest first
  last-monster-sample           ; most recent :monsters list, for frames
  max-party-pb-shifta           ; highest legitimate shifta, NIL = unknown
  (illegal-shifta nil)          ; own shifta exceeded the party ceiling
  (fast-warps nil))             ; someone warped with fast burst on

(defun telemetry-elapsed-ms (telemetry now)
  (round (* 1000 (- now (telemetry-start-time telemetry)))
         internal-time-units-per-second))

(defun bump-alist (alist key &optional (delta 1) (test #'equal))
  "Increment KEY in ALIST by DELTA, returning the (possibly new) alist."
  (let ((cell (assoc key alist :test test)))
    (if cell
        (progn (incf (cdr cell) delta) alist)
        (cons (cons key delta) alist))))

(defun telemetry-weapon-entry (telemetry id &optional display type)
  (let ((cell (assoc id (telemetry-weapons telemetry) :test #'equal)))
    (unless cell
      (setf cell (cons id (list :display (or display id)
                                :type (or type :weapon)
                                :seconds 0 :attacks 0 :techs 0)))
      (push cell (telemetry-weapons telemetry)))
    (cdr cell)))

(defun round1 (value)
  "One decimal, keeping frame rows compact."
  (/ (round (* 10 value)) 10.0))

(defun update-state-tracking (telemetry me delta-ms)
  (let ((state (getf me :state))
        (previous (telemetry-last-state telemetry)))
    (when state
      (setf (telemetry-time-by-state telemetry)
            (bump-alist (telemetry-time-by-state telemetry) state delta-ms #'eql))
      (let ((weapon (telemetry-weapon-entry
                     telemetry (telemetry-current-weapon-id telemetry))))
        (when (and (member state +attack-states+)
                   (not (member previous +attack-states+)))
          (incf (getf weapon :attacks)))
        (when (and (eql state +casting-state+)
                   (not (eql previous +casting-state+)))
          (incf (getf weapon :techs))
          (let ((tech (tech-name (getf me :current-tech))))
            (when tech
              (setf (telemetry-techs-cast telemetry)
                    (bump-alist (telemetry-techs-cast telemetry) tech))))))
      (setf (telemetry-last-state telemetry) state))))

(defun update-death-tracking (telemetry me second)
  (let ((hp (getf me :hp)))
    (when hp
      (let ((previous (telemetry-last-hp telemetry)))
        (when (and previous (plusp previous) (zerop hp))
          (incf (telemetry-death-count telemetry))
          (push (list :t second :type "death") (telemetry-events telemetry))))
      (setf (telemetry-last-hp telemetry) hp))))

(defun update-map-tracking (telemetry snapshot second)
  (let ((map (getf snapshot :map)))
    (when map
      (let ((previous (telemetry-last-map telemetry)))
        (when (and previous (/= previous map))
          (push (list :t second :type "floor" :floor map)
                (telemetry-events telemetry))))
      (setf (telemetry-last-map telemetry) map))))

(defun update-resource-tracking (telemetry me snapshot)
  ;; Traps and TP only ever decrease through use; refills happen on
  ;; Pioneer 2 where quests are not running.
  (let ((traps (list (getf me :damage-traps) (getf me :freeze-traps)
                     (getf me :confuse-traps)))
        (previous (telemetry-last-traps telemetry)))
    (when (every #'integerp traps)
      (when previous
        (loop :for key :in '(:dt :ft :ct)
              :for old :in previous
              :for new :in traps
              :when (> old new)
                :do (incf (getf (telemetry-traps-used telemetry) key)
                          (- old new))))
      (setf (telemetry-last-traps telemetry) traps)))
  (let ((tp (getf me :tp))
        (previous (telemetry-last-tp telemetry)))
    (when tp
      (when (and previous (> previous tp))
        (incf (telemetry-tp-used telemetry) (- previous tp)))
      (setf (telemetry-last-tp telemetry) tp)))
  ;; Meseta spent while in the field = charged (charge specials, shops
  ;; on Pioneer 2 are floor 0 and excluded), following psostats.
  (let ((meseta (getf me :meseta))
        (previous (telemetry-last-meseta telemetry))
        (floor (getf me :floor 0)))
    (when meseta
      (when (and previous (> previous meseta) (plusp floor))
        (incf (telemetry-meseta-charged telemetry) (- previous meseta)))
      (setf (telemetry-last-meseta telemetry) meseta))))

(defun update-inventory-tracking (telemetry inventory)
  (let ((consumables (getf inventory :consumables))
        (previous (telemetry-last-consumables telemetry)))
    (when previous
      (loop :for (key value) :on consumables :by #'cddr
            :for old := (getf previous key)
            ;; A drop of exactly one between samples is a use; larger
            ;; drops are drops/bank moves, matching psostats.
            :when (and old (= value (1- old)))
              :do (setf (telemetry-items-used telemetry)
                        (bump-alist (telemetry-items-used telemetry) key 1 #'eq))))
    (setf (telemetry-last-consumables telemetry) consumables))
  ;; Equipped gear accrues one second per sample; samples arrive ~1/sec.
  (let ((equipment (getf inventory :equipment)))
    (dolist (item equipment)
      (let ((entry (telemetry-weapon-entry telemetry (getf item :id)
                                           (getf item :display)
                                           (getf item :type))))
        (incf (getf entry :seconds))))
    (unless (getf inventory :weapon)
      (let ((entry (telemetry-weapon-entry telemetry "Bare Handed"
                                           "Bare Handed" :weapon)))
        (incf (getf entry :seconds)))))
  (setf (telemetry-current-weapon-id telemetry)
        (or (getf (getf inventory :weapon) :id) "Bare Handed")))

(defun update-boss-tracking (telemetry state second)
  "Register STATE's monster as a boss the first time it is seen and
keep its kill time current (psostats consolidateMonsterState).
Per-second HP history is appended by PUSH-FRAME."
  (let ((name (boss-name (getf state :unitxt) (getf state :index))))
    (when name
      (let* ((id (getf state :id))
             (cell (assoc id (telemetry-bosses telemetry))))
        (unless cell
          ;; A later form of the same boss gets a (n) suffix.
          (let ((form (count-if (lambda (other)
                                  (eql (getf (cdr other) :unitxt)
                                       (getf state :unitxt)))
                                (telemetry-bosses telemetry))))
            (when (plusp form)
              (setf name (format nil "~a (~d)" name form))))
          (setf cell (cons id (list :name name
                                    :id id
                                    :unitxt (getf state :unitxt)
                                    :spawn-t second
                                    :killed-t nil
                                    :hp '())))
          (push cell (telemetry-bosses telemetry)))
        (let ((killed-ms (getf state :killed-ms)))
          (when (and killed-ms (null (getf (cdr cell) :killed-t)))
            (setf (getf (cdr cell) :killed-t) (floor killed-ms 1000))))))))

(defun update-monster-tracking (telemetry monsters elapsed-ms)
  "One monster sample: spawn/kill bookkeeping, per-player damage and
last-hit attribution (psostats consolidateMonsterState). A monster
first seen with 0 hp still spawns alive there, so its death registers
one sample later; kills are alive -> dead transitions only."
  (let ((table (telemetry-monsters telemetry))
        (second (floor elapsed-ms 1000))
        (alive 0)
        (pool 0))
    (dolist (monster monsters)
      (let* ((id (getf monster :id))
             (hp (getf monster :hp 0))
             (attacker (getf monster :last-attacker 0))
             (state (gethash id table)))
        (when (plusp hp)
          (incf alive))
        (incf pool hp)
        (cond
          ((null state)
           (setf state (list :id id
                             :unitxt (getf monster :unitxt)
                             :index (getf monster :index)
                             :name (getf monster :name)
                             :hp hp
                             :alive t
                             :spawn-ms elapsed-ms
                             :killed-ms nil
                             :frame1 nil)
                 (gethash id table) state))
          ((and (getf state :alive) (zerop hp))
           (setf (getf state :alive) nil
                 (getf state :killed-ms) elapsed-ms)
           (incf (telemetry-kills telemetry))
           (unless (member (getf state :unitxt) +frame1-excluded-unitxt+)
             (setf (getf state :frame1)
                   (< (- elapsed-ms (getf state :spawn-ms))
                      +frame1-threshold-ms+)))
           (setf (telemetry-last-hits telemetry)
                 (bump-alist (telemetry-last-hits telemetry) attacker 1 #'eql))
           ;; The killing blow is credited with the remaining hp.
           (when (plusp (getf state :hp))
             (setf (telemetry-player-damage telemetry)
                   (bump-alist (telemetry-player-damage telemetry)
                               attacker (getf state :hp) #'eql)))
           (setf (getf state :hp) 0))
          ((getf state :alive)
           (when (< hp (getf state :hp))
             (setf (telemetry-player-damage telemetry)
                   (bump-alist (telemetry-player-damage telemetry)
                               attacker (- (getf state :hp) hp) #'eql)))
           (setf (getf state :hp) hp)))
        (update-boss-tracking telemetry state second)))
    (setf (telemetry-monsters-alive telemetry) alive
          (telemetry-monster-hp-pool telemetry) pool
          (telemetry-last-monster-sample telemetry) monsters)))

(defun player-location-key (player)
  "Key for a player's per-frame location: the guild card when known
\(psostats PlayerByGcLocation), the character name otherwise."
  (or (getf player :guild-card) (getf player :name) ""))

(defun frame-player-locs (players)
  (loop :for player :in players
        :collect (list (player-location-key player)
                       (getf player :floor 0)
                       (getf player :room 0)
                       (round1 (getf player :x 0.0))
                       (round1 (getf player :y 0.0))
                       (round1 (getf player :z 0.0))
                       (getf player :facing 0)
                       (if (getf player :warping) 1 0))))

(defun frame-monster-locs (monsters)
  (loop :for monster :in monsters
        :when (plusp (getf monster :hp 0))
          :collect (list (getf monster :id)
                         (round1 (getf monster :x 0.0))
                         (round1 (getf monster :y 0.0))
                         (round1 (getf monster :z 0.0))
                         (getf monster :facing 0)
                         (getf monster :hp 0)
                         (if (getf monster :frozen) 1 0)
                         (if (getf monster :paralyzed) 1 0)
                         (if (getf monster :confused) 1 0))))

(defun my-damage-dealt (telemetry snapshot)
  (or (cdr (assoc (getf snapshot :my-index)
                  (telemetry-player-damage telemetry)))
      0))

(defun push-frame (telemetry me snapshot second)
  (push (list second
              (getf me :hp 0)
              (getf me :tp 0)
              (round (getf me :pb 0))
              (telemetry-meseta-charged telemetry)
              (getf me :floor 0)
              (getf me :room 0)
              (round1 (getf me :x 0.0))
              (round1 (getf me :z 0.0))
              (getf me :shifta 0)
              (getf me :deband 0)
              (if (getf me :invincible) 1 0)
              (getf me :state 0)
              (telemetry-monsters-alive telemetry)
              (telemetry-kills telemetry)
              (or (getf snapshot :map-variation) 0)
              (getf me :freeze-traps 0)
              (getf me :damage-traps 0)
              (getf me :confuse-traps 0)
              (my-damage-dealt telemetry snapshot)
              (telemetry-current-weapon-id telemetry)
              (frame-player-locs (getf snapshot :players))
              (frame-monster-locs (telemetry-last-monster-sample telemetry)))
        (telemetry-frames telemetry))
  ;; The per-second boss HP histories and monster HP pool advance in
  ;; step with the frames.
  (loop :for (id . boss) :in (telemetry-bosses telemetry)
        :for state := (gethash id (telemetry-monsters telemetry))
        :do (push (getf state :hp 0) (getf boss :hp)))
  (push (telemetry-monster-hp-pool telemetry)
        (telemetry-monster-hp-pool-series telemetry))
  (setf (telemetry-last-frame-second telemetry) second))

(defun telemetry-step (telemetry snapshot &key (now (get-internal-real-time)))
  "Feed one frame. Cheap per-frame bookkeeping always runs; heavier
sources (:inventory, :monsters) are used whenever the snapshot carries
them; a data frame is recorded once per second."
  (let* ((me (snapshot-my-player snapshot))
         (elapsed-ms (telemetry-elapsed-ms telemetry now))
         (second (floor elapsed-ms 1000))
         (delta-ms (if (telemetry-last-tick telemetry)
                       (max 0 (round (* 1000 (- now (telemetry-last-tick telemetry)))
                                     internal-time-units-per-second))
                       0)))
    (setf (telemetry-last-tick telemetry) now)
    (let ((monsters (getf snapshot :monsters)))
      (when monsters
        (update-monster-tracking telemetry monsters elapsed-ms)))
    ;; Anyone still warping while fast burst is on (psostats FastWarps).
    (when (and (getf snapshot :fast-burst)
               (some (lambda (player) (getf player :warping))
                     (getf snapshot :players)))
      (setf (telemetry-fast-warps telemetry) t))
    (when me
      ;; Own shifta above the party's legitimate ceiling (IllegalShifta).
      (let ((limit (telemetry-max-party-pb-shifta telemetry)))
        (when (and limit (> (getf me :shifta 0) limit))
          (setf (telemetry-illegal-shifta telemetry) t)))
      (update-state-tracking telemetry me delta-ms)
      (update-death-tracking telemetry me second)
      (update-map-tracking telemetry snapshot second)
      (update-resource-tracking telemetry me snapshot)
      (let ((inventory (getf snapshot :inventory)))
        (when inventory
          (update-inventory-tracking telemetry inventory)))
      (when (> second (telemetry-last-frame-second telemetry))
        (push-frame telemetry me snapshot second)))
    telemetry))

(defun monster-run-data (telemetry)
  "Per-monster records as printable plists, oldest spawn first."
  (let ((monsters '()))
    (maphash (lambda (id state)
               (declare (ignore id))
               (push (list :id (getf state :id)
                           :unitxt (getf state :unitxt)
                           :name (getf state :name)
                           :spawn-ms (getf state :spawn-ms)
                           :killed-ms (getf state :killed-ms)
                           :frame1 (and (getf state :frame1) t))
                     monsters))
             (telemetry-monsters telemetry))
    (sort monsters (lambda (a b) (< (getf a :spawn-ms 0) (getf b :spawn-ms 0))))))

(defun boss-run-data (telemetry)
  "Boss records as printable plists, spawn order preserved."
  (reverse
   (mapcar (lambda (cell)
             (let ((boss (cdr cell)))
               (list :name (getf boss :name)
                     :id (getf boss :id)
                     :unitxt (getf boss :unitxt)
                     :spawn-t (getf boss :spawn-t)
                     :killed-t (getf boss :killed-t)
                     :hp (reverse (getf boss :hp)))))
           (telemetry-bosses telemetry))))

(defun copy-alist-cells (alist)
  (reverse (mapcar (lambda (cell) (cons (car cell) (cdr cell))) alist)))

(defun telemetry-run-data (telemetry)
  "Snapshot of the accumulated telemetry as a printable plist for the
run queue. Taken at the moment a tracker finishes, so running totals
are correct for segment categories that end before the full clear."
  (list :frames (reverse (telemetry-frames telemetry))
        :events (reverse (telemetry-events telemetry))
        :death-count (telemetry-death-count telemetry)
        :meseta-charged (telemetry-meseta-charged telemetry)
        :kills (telemetry-kills telemetry)
        :tp-used (telemetry-tp-used telemetry)
        :traps-used (copy-list (telemetry-traps-used telemetry))
        :items-used (copy-alist-cells (telemetry-items-used telemetry))
        :techs-cast (copy-alist-cells (telemetry-techs-cast telemetry))
        :time-by-state (copy-alist-cells (telemetry-time-by-state telemetry))
        :weapons (reverse
                  (mapcar (lambda (cell)
                            (list* :id (car cell) (copy-list (cdr cell))))
                          (telemetry-weapons telemetry)))
        :monsters (monster-run-data telemetry)
        :bosses (boss-run-data telemetry)
        :player-damage (copy-alist-cells (telemetry-player-damage telemetry))
        :last-hits (copy-alist-cells (telemetry-last-hits telemetry))
        :monster-hp-pool (reverse (telemetry-monster-hp-pool-series telemetry))
        :max-party-pb-shifta (telemetry-max-party-pb-shifta telemetry)
        :illegal-shifta (and (telemetry-illegal-shifta telemetry) t)
        :fast-warps (and (telemetry-fast-warps telemetry) t)))
