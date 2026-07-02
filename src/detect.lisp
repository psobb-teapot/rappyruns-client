(in-package :ephinea-ta-client)

;;; Quest detection state machine. Pure: consumes snapshots produced by
;;; READ-SNAPSHOT (or synthesized by tests) and emits a run plist when a
;;; quest completes. Mirrors psostats-client's RefreshData flow.

(defstruct detector
  (state :idle)        ; :idle | :in-quest
  (armed nil)          ; T once we've seen "no quest loaded" (lobby);
                       ; guards against starting the client mid-quest
  quest-def            ; matched quest-def while :in-quest
  quest-ptr            ; quest struct pointer at start, to detect resets
  start-time           ; internal real time at quest start
  party                ; ((:name ... :class ...) ...) captured at start
  my-pb                ; own PB gauge on the previous frame
  (pb-flag nil))       ; T when the run belongs in the PB category

(defun elapsed-ms (start-time)
  (round (* 1000 (- (get-internal-real-time) start-time))
         internal-time-units-per-second))

(defun detector-elapsed-ms (detector)
  (when (eq (detector-state detector) :in-quest)
    (elapsed-ms (detector-start-time detector))))

(defun trigger-met-p (trigger snapshot)
  (ecase (first trigger)
    (:register (snapshot-register-set-p snapshot (second trigger)))
    (:floor-switch (snapshot-floor-switch-set-p
                    snapshot (second trigger) (third trigger)))
    (:warp-in (some (lambda (player)
                      (and (plusp (getf player :floor 0))
                           (not (getf player :warping))))
                    (getf snapshot :players)))))

(defun snapshot-quest-def (snapshot)
  (when (getf snapshot :quest-name)
    (find-quest-def :number (getf snapshot :quest-number)
                    :episode (getf snapshot :episode)
                    :name (getf snapshot :quest-name))))

(defun party-of (snapshot)
  (loop :for player :in (getf snapshot :players)
        :when (getf player :class)
          :collect (list :name (getf player :name)
                         :class (getf player :class))))

(defun pb-category-at-start-p (snapshot)
  "Entering the quest with a charged Photon Blast or Shifta already cast
puts the run in the PB category (simplified psostats StartNewQuest check)."
  (let ((me (snapshot-my-player snapshot)))
    (and me
         (or (> (getf me :pb 0.0) 5.0)
             (> (abs (getf me :shifta 0.0)) 0.0)))))

(defun update-pb-tracking (detector snapshot)
  "A large single-frame drop of the PB gauge means a Photon Blast was used."
  (let* ((me (snapshot-my-player snapshot))
         (pb (and me (getf me :pb))))
    (when pb
      (let ((previous (detector-my-pb detector)))
        (when (and previous (> (- previous pb) 50.0))
          (setf (detector-pb-flag detector) t)))
      (setf (detector-my-pb detector) pb))))

(defun reset-detector (detector)
  (setf (detector-state detector) :idle
        (detector-quest-def detector) nil
        (detector-quest-ptr detector) nil
        (detector-start-time detector) nil
        (detector-party detector) nil
        (detector-my-pb detector) nil
        (detector-pb-flag detector) nil))

(defun start-run (detector snapshot quest-def)
  (setf (detector-state detector) :in-quest
        (detector-quest-def detector) quest-def
        (detector-quest-ptr detector) (getf snapshot :quest-ptr)
        (detector-start-time detector) (get-internal-real-time)
        (detector-party detector) (party-of snapshot)
        (detector-my-pb detector) (let ((me (snapshot-my-player snapshot)))
                                    (and me (getf me :pb)))
        (detector-pb-flag detector) (pb-category-at-start-p snapshot)))

(defun finish-run (detector snapshot)
  (let ((def (detector-quest-def detector))
        (time-ms (max 1 (elapsed-ms (detector-start-time detector)))))
    (prog1
        (list :quest-slug (quest-def-slug def)
              :quest-name (getf snapshot :quest-name)
              :episode (quest-def-episode def)
              :time-ms time-ms
              :party-size (length (detector-party detector))
              :pb (and (detector-pb-flag detector) t)
              :players (detector-party detector)
              :finished-at (get-universal-time))
      (reset-detector detector)
      ;; The quest stays loaded with its triggers still set after
      ;; completion; disarm until the player unloads it (lobby) so the
      ;; same run cannot re-start (psostats' AllowQuestStart behaviour).
      (setf (detector-armed detector) nil))))

(defun detector-step (detector snapshot)
  "Feed one SNAPSHOT (NIL when the game is unreadable). Returns a run
plist when a quest just completed, otherwise NIL."
  (cond
    ;; Game gone: abandon any run in progress.
    ((null snapshot)
     (reset-detector detector)
     (setf (detector-armed detector) nil)
     nil)
    ;; No quest loaded (lobby / free field): arm and idle.
    ((not (and (getf snapshot :quest-ptr) (plusp (getf snapshot :quest-ptr))))
     (reset-detector detector)
     (setf (detector-armed detector) t)
     nil)
    ((eq (detector-state detector) :idle)
     (let ((def (snapshot-quest-def snapshot)))
       (when (and def
                  (detector-armed detector)
                  (trigger-met-p (quest-def-start def) snapshot))
         (start-run detector snapshot def))
       nil))
    ;; :in-quest
    (t
     (cond
       ;; Quest reloaded or a different quest: the run is void.
       ((/= (getf snapshot :quest-ptr) (detector-quest-ptr detector))
        (reset-detector detector)
        nil)
       ((trigger-met-p (quest-def-end (detector-quest-def detector)) snapshot)
        (finish-run detector snapshot))
       (t
        (update-pb-tracking detector snapshot)
        nil)))))
