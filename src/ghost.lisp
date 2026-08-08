(in-package :ephinea-ta-client)

;;; Ghost race: while a quest runs, show the live gap against a
;;; reference run ("the ghost") fetched from the server at quest load.
;;; The server resolves the reference (GET /api/quests/:slug/ghost): the
;;; run the user marked "Race this run" on the site, else their own best
;;; telemetry-bearing time on the same difficulty / party size.
;;;
;;; The gap updates at room entries. The ghost arrives as a list of
;;; (floor, room, nth visit, enter-ms) splits, and the live side re-uses
;;; the same (floor, room, nth) identity the site's /compare page aligns
;;; rooms by, so route differences simply leave rooms unmatched and the
;;; previous gap standing. Ghosts recorded by ghost-era clients carry
;;; ms-precision room events; older telemetry falls back to the
;;; per-second frames, so the gap is only second-accurate there (the
;;; :precision slot says which, and the display rounds accordingly).
;;;
;;; Pure except for the fetch plumbing, so the SBCL tests cover the
;;; matching and formatting.

(defstruct ghost
  quest-slug ; slug of the reference run's quest. Usually the slug the
             ; fetch asked for; a chosen target can be a segment
             ; category of the same in-game quest, in which case the
             ; gap tracks that segment's tracker instead.
  run-id
  time-ms    ; the ghost's final time - the number to beat
  label      ; submitter name, for toasts and run-list notes
  source     ; "target" (chosen on the site) or "pb"
  precision  ; :ms (room events) or :sec (frame-derived)
  rooms)     ; vector of (:floor :room :nth :enter-ms), progression order

(defstruct ghost-race
  ghost
  (visit-counts (make-hash-table :test 'equal)) ; (floor . room) -> visits
  last-key       ; (floor . room) on the previous frame
  delta-ms       ; latest gap (live minus ghost); NIL before the first match
  (matched-rooms 0))

(defvar *ghost* nil
  "The ghost fetched for the currently loaded quest, or NIL. Written by
the fetch thread, read by the poll loop; the single variable swap is
atomic enough.")

(defvar *ghost-race* nil
  "Live race state while a quest tracker runs, or NIL.")

(defvar *ghost-fetch-ptr* nil
  "Quest-ptr a ghost fetch was started (or skipped) for, so one quest
load asks the server exactly once.")

(defparameter +ghost-join-max-ms+ 10000
  "A ghost whose fetch completes later than this into a run is not
raced this run: the room-visit counts would start mid-route and
misalign the nth-visit matching. The fetch starts at quest load,
well before the start trigger, so this almost never bites.")

(defun parse-ghost-splits (payload)
  "GET /api/quests/:slug/ghost payload -> ghost, or NIL when malformed."
  (when (hash-table-p payload)
    (let ((rooms (gethash "rooms" payload))
          (time-ms (gethash "time_ms" payload)))
      (when (and (vectorp rooms) (integerp time-ms))
        (make-ghost
         :quest-slug (gethash "quest" payload)
         :run-id (gethash "run_id" payload)
         :time-ms time-ms
         :label (gethash "submitter" payload)
         :source (gethash "source" payload)
         :precision (if (equal (gethash "precision" payload) "ms") :ms :sec)
         :rooms (map 'vector
                     (lambda (room)
                       (list :floor (gethash "floor" room)
                             :room (gethash "room" room)
                             :nth (gethash "nth" room)
                             :enter-ms (gethash "enter_ms" room)))
                     rooms))))))

(defun ghost-room-enter-ms (ghost floor room nth)
  (loop :for entry :across (ghost-rooms ghost)
        :when (and (eql (getf entry :floor) floor)
                   (eql (getf entry :room) room)
                   (eql (getf entry :nth) nth))
          :return (getf entry :enter-ms)))

(defun ghost-race-note-room (race floor room elapsed-ms)
  "Feed the submitter's current (floor, room) at ELAPSED-MS. On entry
into a room the ghost also visited (same floor, room, nth visit) the
gap updates to live-minus-ghost; rooms the ghost never entered leave
the previous gap standing, mirroring the compare page's one-sided
rows. Returns the race's current gap."
  (let ((key (cons floor room)))
    (unless (equal key (ghost-race-last-key race))
      (setf (ghost-race-last-key race) key)
      (let* ((nth (incf (gethash key (ghost-race-visit-counts race) 0)))
             (enter (ghost-room-enter-ms (ghost-race-ghost race)
                                         floor room nth)))
        (when enter
          (incf (ghost-race-matched-rooms race))
          (setf (ghost-race-delta-ms race) (- elapsed-ms enter))))))
  (ghost-race-delta-ms race))

(defun ghost-matches-detector-p (ghost detector)
  "Does GHOST belong to one of the loaded quest's trackers? Guards a
stale ghost from a previous quest load from ever racing the wrong one."
  (and (find (ghost-quest-slug ghost) (detector-trackers detector)
             :key (lambda (tracker) (quest-def-slug (tracker-def tracker)))
             :test #'equal)
       t))

(defun ghost-race-step (detector snapshot)
  "One poll-loop step: create the live race state when a quest with a
matching ghost starts, feed the submitter's room, drop the state when
the quest ends. Safe to call every frame."
  (if (not (eq (detector-state detector) :in-quest))
      (setf *ghost-race* nil)
      (let ((ghost *ghost*))
        (when (and ghost
                   (or (null *ghost-race*)
                       (not (eq (ghost-race-ghost *ghost-race*) ghost))))
          (setf *ghost-race*
                (and (ghost-matches-detector-p ghost detector)
                     (< (or (detector-elapsed-ms detector) 0)
                        +ghost-join-max-ms+)
                     (make-ghost-race :ghost ghost))))
        (let ((race *ghost-race*)
              (me (snapshot-my-player snapshot))
              (elapsed (detector-elapsed-ms detector)))
          (when (and race me elapsed)
            (ghost-race-note-room race (getf me :floor 0) (getf me :room 0)
                                  elapsed))))))

;;; Fetch orchestration. The fetch keys off the quest POINTER, not the
;;; detector: a quest loads seconds before its start trigger fires, and
;;; those seconds hide the network round trip.

(defun ghost-fetch-wanted (snapshot)
  "(values slug difficulty party-size) when a ghost fetch should start
for SNAPSHOT's freshly loaded quest, else NIL. Consumes the quest-ptr
either way once the quest is identified, so one load asks once; the
previous ghost is dropped the moment a new load is seen."
  (let ((ptr (and snapshot (getf snapshot :quest-ptr))))
    (when (and ptr (plusp ptr)
               (getf snapshot :quest-name)
               (not (eql ptr *ghost-fetch-ptr*)))
      (setf *ghost-fetch-ptr* ptr
            *ghost* nil)
      (let ((def (find-quest-def :number (getf snapshot :quest-number)
                                 :episode (getf snapshot :episode)
                                 :name (getf snapshot :quest-name))))
        (when (and def
                   (config-value :ghost-race)
                   (string/= (normalize-token (config-value :api-token)) ""))
          (values (quest-def-slug def)
                  (difficulty-label (getf snapshot :difficulty)
                                    (getf snapshot :anguish))
                  (max 1 (length (party-of snapshot)))))))))

#+lispworks
(defun maybe-start-ghost-fetch (snapshot)
  "Kick off the background ghost fetch when a quest just loaded. The
result lands in *GHOST* only if the same quest load is still current
by the time the reply arrives."
  (multiple-value-bind (slug difficulty party-size)
      (ghost-fetch-wanted snapshot)
    (when slug
      (let ((ptr *ghost-fetch-ptr*))
        (mp:process-run-function
         "eta-client-ghost-fetch" '()
         (lambda ()
           (let ((ghost (handler-case
                            (multiple-value-bind (outcome payload)
                                (fetch-ghost-splits slug
                                                    :difficulty difficulty
                                                    :party-size party-size)
                              (and (eq outcome :ok)
                                   (parse-ghost-splits payload)))
                          (error () nil))))
             (when (eql ptr *ghost-fetch-ptr*)
               (setf *ghost* ghost)))))))))

;;; Display helpers.

(defun format-ghost-delta (delta-ms precision)
  "'-3.2s' / '+4s': the live gap, whole seconds when the ghost is only
second-accurate (frame-derived splits)."
  (let ((sign (if (minusp delta-ms) "-" "+"))
        (magnitude (abs delta-ms)))
    (if (eq precision :ms)
        (format nil "~a~,1fs" sign (/ magnitude 1000.0))
        (format nil "~a~ds" sign (round magnitude 1000)))))

(defun ghost-status-suffix ()
  "' | vs 12:34.567 -3.2s' for the quest-status line while a race is
live; before the first matched room only the target time shows, so the
runner knows a ghost is loaded. NIL when no race is on."
  (let ((race *ghost-race*))
    (when race
      (let ((ghost (ghost-race-ghost race))
            (delta (ghost-race-delta-ms race)))
        (format nil " | vs ~a~@[ ~a~]"
                (format-run-time (ghost-time-ms ghost))
                (and delta (format-ghost-delta
                            delta (ghost-precision ghost))))))))

(defun ghost-title-suffix ()
  "' -3.2s' for the window title - the gap alone; the title already
leads with the live clock. NIL before the first matched room."
  (let ((race *ghost-race*))
    (when race
      (let ((delta (ghost-race-delta-ms race)))
        (when delta
          (format nil " ~a"
                  (format-ghost-delta
                   delta (ghost-precision (ghost-race-ghost race)))))))))

(defun annotate-ghost-runs (runs)
  "Stamp each completed run whose quest slug the current ghost covers
with the final comparison: :ghost-delta-ms (run time minus ghost time,
negative = beat the ghost), :ghost-time-ms and :ghost-label. The final
gap compares the runs' official times, so it is ms-accurate whatever
the ghost's room precision. Aborted runs pass untouched."
  (let ((ghost *ghost*))
    (if (null ghost)
        runs
        (mapcar (lambda (run)
                  (if (and (equal (getf run :quest-slug)
                                  (ghost-quest-slug ghost))
                           (not (getf run :aborted))
                           (integerp (ghost-time-ms ghost)))
                      (list* :ghost-delta-ms (- (getf run :time-ms)
                                                (ghost-time-ms ghost))
                             :ghost-time-ms (ghost-time-ms ghost)
                             :ghost-label (or (ghost-label ghost) "")
                             run)
                      run))
                runs))))
