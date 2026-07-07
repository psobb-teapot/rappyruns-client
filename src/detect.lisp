(in-package :ephinea-ta-client)

;;; Quest detection state machine. Pure: consumes snapshots produced by
;;; READ-SNAPSHOT (or synthesized by tests) and emits run plists when
;;; quests complete. Mirrors psostats-client's RefreshData flow.
;;;
;;; One loaded quest can match SEVERAL trigger definitions (the full
;;; clear plus segment categories like "(2 Rooms)" that share the start
;;; trigger but end earlier). Each matching definition gets its own
;;; tracker, so a full run also produces the segment records for free.

(defstruct tracker
  def          ; the quest-def this tracker times
  start-time   ; internal real time at its start trigger
  party        ; ((:name ... :class ...) ...) captured at start
  quest-name   ; quest name captured at start (snapshots may be gone
  difficulty   ; by the time an abandoned run is emitted)
  done)        ; T once its run has been emitted

(defstruct detector
  (state :idle)        ; :idle | :in-quest (any tracker still running)
  (armed nil)          ; T once we've seen "no quest loaded" (lobby);
                       ; guards against starting the client mid-quest
  quest-ptr            ; quest struct pointer, to detect quest reloads
  (trackers '())       ; trackers for the loaded quest, oldest first;
                       ; kept (done or not) until the quest unloads so a
                       ; definition can never re-start within one load
  my-pb                ; own PB gauge on the previous frame
  (pb-flag nil)        ; T when the session belongs in the PB category
  (seen-alive '())     ; monster :id values seen alive (hp>0) this load;
                       ; a (:monster-dead ID) end trigger needs to know an
                       ; enemy was actually alive before its kill counts
  (killed-ids '())     ; monster :id values confirmed dead (were alive,
                       ; then observed at 0 hp) this load
  telemetry)           ; per-quest TELEMETRY, created with the first tracker

(defun elapsed-ms (start-time)
  (round (* 1000 (- (get-internal-real-time) start-time))
         internal-time-units-per-second))

(defun active-trackers (detector)
  (remove-if #'tracker-done (detector-trackers detector)))

(defun detector-active-def (detector)
  "Definition of the longest-running unfinished tracker, or NIL."
  (let ((tracker (first (active-trackers detector))))
    (and tracker (tracker-def tracker))))

(defun detector-active-count (detector)
  (length (active-trackers detector)))

(defun detector-elapsed-ms (detector)
  (let ((tracker (first (active-trackers detector))))
    (and tracker (elapsed-ms (tracker-start-time tracker)))))

(defun trigger-met-p (trigger snapshot &optional killed-ids)
  "Is TRIGGER satisfied on SNAPSHOT? KILLED-IDS is the run's confirmed
monster kills (see UPDATE-KILL-TRACKING); only a :monster-dead trigger
consults it, so start-trigger checks may pass it as NIL."
  (ecase (first trigger)
    (:register (snapshot-register-set-p snapshot (second trigger)))
    (:floor-switch (snapshot-floor-switch-set-p
                    snapshot (second trigger) (third trigger)))
    (:warp-in (some (lambda (player)
                      (and (plusp (getf player :floor 0))
                           (not (getf player :warping))))
                    (getf snapshot :players)))
    ;; A specific enemy (by entity :id) has died this run. The room is
    ;; implicit in the id, which is unique across the loaded quest.
    (:monster-dead (and (member (second trigger) killed-ids) t))))

(defun update-kill-tracking (detector snapshot)
  "Fold this frame's :monsters into the detector's per-id life/death
state. A monster counts as dead only once it has been seen alive (hp>0)
and is then observed at 0 hp - the same alive->dead rule telemetry uses,
so an enemy that merely spawns at 0 hp never false-fires a clear."
  (dolist (monster (getf snapshot :monsters))
    (let ((id (getf monster :id))
          (hp (getf monster :hp 0)))
      (cond ((plusp hp)
             (pushnew id (detector-seen-alive detector)))
            ((and (member id (detector-seen-alive detector))
                  (not (member id (detector-killed-ids detector))))
             (push id (detector-killed-ids detector)))))))

(defun snapshot-quest-defs (snapshot)
  (if (getf snapshot :quest-name)
      (find-quest-defs :number (getf snapshot :quest-number)
                       :episode (getf snapshot :episode)
                       :name (getf snapshot :quest-name))
      '()))

(defun party-of (snapshot)
  (loop :for player :in (getf snapshot :players)
        :when (getf player :class)
          :collect (list :name (getf player :name)
                         :class (getf player :class)
                         :level (getf player :level)
                         :section-id (getf player :section-id)
                         :guild-card (getf player :guild-card))))

(defun pb-category-at-start-p (snapshot)
  "The PB category is decided solely by an actual Photon Blast discharge
during the run (a large PB-gauge drop; see UPDATE-PB-TRACKING). Treating
a charged gauge or an already-cast Shifta at the start frame as PB
produced false positives - a normal No-PB party run routinely starts
with Shifta up - so a run is No PB until a discharge is seen."
  (declare (ignore snapshot))
  nil)

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
        (detector-trackers detector) '()
        (detector-quest-ptr detector) nil
        (detector-my-pb detector) nil
        (detector-pb-flag detector) nil
        (detector-seen-alive detector) '()
        (detector-killed-ids detector) '()
        (detector-telemetry detector) nil))

(defun start-tracker (detector def snapshot)
  ;; The PB session state and telemetry belong to the quest, not the
  ;; tracker; take them when the first tracker starts.
  (when (null (detector-trackers detector))
    (setf (detector-pb-flag detector) (pb-category-at-start-p snapshot)
          (detector-my-pb detector) (let ((me (snapshot-my-player snapshot)))
                                      (and me (getf me :pb)))
          (detector-telemetry detector)
          (make-telemetry :start-time (get-internal-real-time)
                          :max-party-pb-shifta (max-party-pb-shifta
                                                (party-of snapshot)))))
  (let ((tracker (make-tracker :def def
                               :start-time (get-internal-real-time)
                               :party (party-of snapshot)
                               :quest-name (getf snapshot :quest-name)
                               :difficulty (difficulty-name
                                            (getf snapshot :difficulty)))))
    (setf (detector-trackers detector)
          (append (detector-trackers detector) (list tracker)))
    tracker))

(defun finish-tracker (detector tracker &key aborted)
  (let ((def (tracker-def tracker))
        (time-ms (max 1 (elapsed-ms (tracker-start-time tracker))))
        (telemetry (detector-telemetry detector)))
    (setf (tracker-done tracker) t)
    (append
     (list :quest-slug (quest-def-slug def)
           :quest-name (tracker-quest-name tracker)
           :episode (quest-def-episode def)
           :time-ms time-ms
           :party-size (length (tracker-party tracker))
           :pb (and (detector-pb-flag detector) t)
           :players (tracker-party tracker)
           :difficulty (tracker-difficulty tracker)
           :death-count (and telemetry (telemetry-death-count telemetry))
           :telemetry (and telemetry (telemetry-run-data telemetry))
           :finished-at (get-universal-time))
     (when aborted (list :aborted t)))))

(defparameter +abort-min-ms+ 15000
  "Abandoned quests shorter than this are noise (mis-starts, instant
resets) and are dropped instead of recorded.")

(defun abandon-trackers (detector)
  "Aborted-run plists for the trackers still running at the moment the
quest is abandoned (lobby return, quest reload or game exit)."
  (loop :for tracker :in (active-trackers detector)
        :when (>= (elapsed-ms (tracker-start-time tracker)) +abort-min-ms+)
          :collect (finish-tracker detector tracker :aborted t)))

(defun detector-step (detector snapshot)
  "Feed one SNAPSHOT (NIL when the game is unreadable). Returns the list
of runs that completed this frame (usually empty or one). Abandoning a
quest mid-run emits the unfinished trackers as :aborted runs."
  (cond
    ;; Game gone: abandon everything.
    ((null snapshot)
     (let ((aborted (abandon-trackers detector)))
       (reset-detector detector)
       (setf (detector-armed detector) nil)
       aborted))
    ;; No quest loaded (lobby / free field): reset and arm.
    ((not (and (getf snapshot :quest-ptr) (plusp (getf snapshot :quest-ptr))))
     (let ((aborted (abandon-trackers detector)))
       (reset-detector detector)
       (setf (detector-armed detector) t)
       aborted))
    (t
     (let ((aborted '()))
       (let ((ptr (getf snapshot :quest-ptr)))
         ;; Quest reloaded or a different quest: in-flight runs are void.
         (when (and (detector-quest-ptr detector)
                    (/= ptr (detector-quest-ptr detector)))
           (setf aborted (abandon-trackers detector))
           (reset-detector detector))
         (setf (detector-quest-ptr detector) ptr))
       (let ((started '())
             (completed (reverse aborted)))
         ;; Start a tracker for each definition whose start trigger fired.
         (when (detector-armed detector)
           (dolist (def (snapshot-quest-defs snapshot))
             (unless (find def (detector-trackers detector) :key #'tracker-def)
               (when (trigger-met-p (quest-def-start def) snapshot)
                 (push (start-tracker detector def snapshot) started)))))
         (when (active-trackers detector)
           (update-pb-tracking detector snapshot)
           (update-kill-tracking detector snapshot)
           (when (detector-telemetry detector)
             (telemetry-step (detector-telemetry detector) snapshot)))
         ;; End checks skip trackers started this frame: a real end trigger
         ;; cannot fire on the start frame, only stale data could.
         (dolist (tracker (detector-trackers detector))
           (unless (or (tracker-done tracker) (member tracker started))
             (when (trigger-met-p (quest-def-end (tracker-def tracker)) snapshot
                                  (detector-killed-ids detector))
               (push (finish-tracker detector tracker) completed))))
         (setf (detector-state detector)
               (if (active-trackers detector) :in-quest :idle))
         (nreverse completed))))))
