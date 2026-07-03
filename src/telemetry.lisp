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
    "shifta" "deband" "inv" "state" "monsters" "kills")
  "Column names for the compact per-second frame rows, in order.")

(defparameter +attack-states+ '(5 6 7))
(defconstant +casting-state+ 8)

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
  (monster-hp (make-hash-table :test 'eql)) ; monster id -> last seen hp
  (monsters-alive 0)
  (kills 0))

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

(defun update-monster-tracking (telemetry monsters)
  (let ((table (telemetry-monster-hp telemetry))
        (alive 0))
    (dolist (monster monsters)
      (let* ((id (getf monster :id))
             (hp (getf monster :hp))
             (previous (gethash id table)))
        (when (plusp hp)
          (incf alive))
        (when (and previous (plusp previous) (zerop hp))
          (incf (telemetry-kills telemetry)))
        (setf (gethash id table) hp)))
    (setf (telemetry-monsters-alive telemetry) alive)))

(defun push-frame (telemetry me second)
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
              (telemetry-kills telemetry))
        (telemetry-frames telemetry))
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
        (update-monster-tracking telemetry monsters)))
    (when me
      (update-state-tracking telemetry me delta-ms)
      (update-death-tracking telemetry me second)
      (update-map-tracking telemetry snapshot second)
      (update-resource-tracking telemetry me snapshot)
      (let ((inventory (getf snapshot :inventory)))
        (when inventory
          (update-inventory-tracking telemetry inventory)))
      (when (> second (telemetry-last-frame-second telemetry))
        (push-frame telemetry me second)))
    telemetry))

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
        :items-used (reverse (mapcar (lambda (cell)
                                       (cons (car cell) (cdr cell)))
                                     (telemetry-items-used telemetry)))
        :techs-cast (reverse (mapcar (lambda (cell)
                                       (cons (car cell) (cdr cell)))
                                     (telemetry-techs-cast telemetry)))
        :time-by-state (reverse (mapcar (lambda (cell)
                                          (cons (car cell) (cdr cell)))
                                        (telemetry-time-by-state telemetry)))
        :weapons (reverse
                  (mapcar (lambda (cell)
                            (list* :id (car cell) (copy-list (cdr cell))))
                          (telemetry-weapons telemetry)))))
