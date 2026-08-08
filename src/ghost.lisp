(in-package :ephinea-ta-client)

;;; Ghost race: while a quest runs, show the live gap against a
;;; reference run ("the ghost") fetched from the server at quest load.
;;; The server resolves the reference (GET /api/quests/:slug/ghost): the
;;; run the user marked "Race this run" on the site - looked up across
;;; EVERY category slug matching the loaded quest, since one in-game
;;; quest maps to several category quests (full clear, segments, solo) -
;;; else their own best telemetry-bearing time on the same difficulty /
;;; party size / pb board dimensions.
;;;
;;; The gap updates at room entries, matched by a streaming version of
;;; the compare page's greedy room alignment: a cursor walks the ghost's
;;; room list in progression order, and each live room entry claims the
;;; ghost's next visit of that (floor, room) within a bounded lookahead.
;;; Order-based matching (rather than strict nth-visit counting) is what
;;; keeps old second-precision ghosts usable: their 1 Hz frames can miss
;;; a room crossed in under a second, and a late-arriving fetch can join
;;; mid-route - both just skip unmatched entries instead of desyncing
;;; the rest of the run.
;;;
;;; All gaps are measured on the quest telemetry's clock (the first
;;; tracker's start): that is exactly the base the reference run's room
;;; events and frames were stamped with, whichever category the ghost
;;; belongs to. The FINAL comparison at completion uses the two runs'
;;; official time_ms instead, which share a base per category by
;;; construction.
;;;
;;; Pure except for the fetch plumbing, so the SBCL tests cover the
;;; matching and formatting.

(defstruct ghost
  quest-slug ; slug of the reference run's quest category. A chosen
             ; target can be a segment category of the same in-game
             ; quest; the completion comparison keys off this slug.
  run-id
  time-ms    ; the ghost's final time - the number to beat
  label      ; submitter name, for toasts and run-list notes
  source     ; "target" (chosen on the site) or "pb"
  pb         ; the reference's board category, 0/1 (NIL: older server)
  precision  ; :ms (room events) or :sec (frame-derived)
  rooms)     ; vector of (:floor :room :nth :enter-ms), progression order

(defstruct ghost-race
  ghost          ; attached ghost, or NIL while the fetch is in flight
  (cursor 0)     ; index into GHOST's rooms of the next unclaimed entry
  last-key       ; (floor . room) on the previous frame
  delta-ms       ; latest gap (live minus ghost); NIL before the first match
  (matched-rooms 0))

(defparameter +ghost-match-lookahead+ 15
  "How many ghost room entries past the cursor a live room entry may
claim. Bounds the damage of a route deviation: a wrong-room excursion
can skip at most this many ghost entries before matching resumes, so
one detour cannot desync the whole rest of the run.")

(defvar *ghost* nil
  "The ghost fetched for the currently loaded quest, or NIL. Written by
the fetch thread, read by the poll loop; the single variable swap is
atomic enough.")

(defvar *ghost-race* nil
  "Live race state while a quest runs, or NIL. Created the moment the
detector enters a quest - before the ghost may have arrived - so the
room progression is tracked from the true start whenever the fetch
lands.")

(defvar *ghost-fetch-ptr* nil
  "Quest-ptr a ghost fetch was started (or skipped) for, so one quest
load asks the server exactly once. Cleared whenever no quest is loaded,
mirroring the detector's disarm: a reload that lands on the same
allocation address must still refetch (the target or PB may have
changed between attempts).")

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
         :pb (let ((pb (gethash "pb" payload)))
               (and (integerp pb) pb))
         :precision (if (equal (gethash "precision" payload) "ms") :ms :sec)
         :rooms (map 'vector
                     (lambda (room)
                       (when (hash-table-p room)
                         (list :floor (gethash "floor" room)
                               :room (gethash "room" room)
                               :nth (gethash "nth" room)
                               :enter-ms (gethash "enter_ms" room))))
                     rooms))))))

(defun ghost-race-note-room (race floor room elapsed-ms)
  "Feed the submitter's current (floor, room) at ELAPSED-MS (on the
quest telemetry's clock). On entry into a new room, claim the ghost's
next visit of that room from the cursor onward (bounded lookahead) and
update the gap to live-minus-ghost; rooms the ghost never entered - or
entered outside the window - leave the previous gap standing, mirroring
the compare page's one-sided rows. Returns the race's current gap."
  (let ((key (cons floor room)))
    (unless (equal key (ghost-race-last-key race))
      (setf (ghost-race-last-key race) key)
      (let* ((ghost (ghost-race-ghost race))
             (rooms (and ghost (ghost-rooms ghost))))
        (when rooms
          (loop :for i :from (ghost-race-cursor race)
                  :below (min (length rooms)
                              (+ (ghost-race-cursor race)
                                 +ghost-match-lookahead+))
                :for entry := (aref rooms i)
                :when (and entry
                           (eql (getf entry :floor) floor)
                           (eql (getf entry :room) room)
                           (numberp (getf entry :enter-ms)))
                  :do (setf (ghost-race-cursor race) (1+ i)
                            (ghost-race-delta-ms race)
                            (- elapsed-ms (getf entry :enter-ms)))
                      (incf (ghost-race-matched-rooms race))
                      (return))))))
  (ghost-race-delta-ms race))

(defun ghost-matches-snapshot-p (ghost snapshot)
  "Does GHOST belong to one of the loaded quest's category defs? Guards
a stale ghost from a previous quest load from ever racing the wrong
one. Matches DEFS, not trackers: a segment category whose start trigger
has not fired yet still belongs to this load."
  (and (find (ghost-quest-slug ghost) (snapshot-quest-defs snapshot)
             :key #'quest-def-slug :test #'equal)
       t))

(defun ghost-race-step (detector snapshot)
  "One poll-loop step: track the room progression from the moment the
detector enters a quest, attach the fetched ghost once it arrives (the
cursor alignment tolerates a late join), and drop everything when the
quest ends. Safe to call every frame."
  (if (not (eq (detector-state detector) :in-quest))
      (setf *ghost-race* nil)
      (let ((race (or *ghost-race*
                      (setf *ghost-race* (make-ghost-race))))
            (ghost *ghost*))
        (when (and ghost
                   (not (eq (ghost-race-ghost race) ghost))
                   (ghost-matches-snapshot-p ghost snapshot))
          (setf (ghost-race-ghost race) ghost))
        (let ((me (snapshot-my-player snapshot))
              (telemetry (detector-telemetry detector)))
          (when (and me telemetry)
            (ghost-race-note-room race (getf me :floor 0) (getf me :room 0)
                                  (telemetry-elapsed-ms
                                   telemetry (get-internal-real-time))))))))

;;; Fetch orchestration. The fetch keys off the quest POINTER, not the
;;; detector: a quest loads seconds before its start trigger fires, and
;;; those seconds hide the network round trip.

(defun ghost-fetch-wanted (snapshot)
  "(values slugs difficulty party-size) when a ghost fetch should start
for SNAPSHOT's freshly loaded quest, else NIL. SLUGS is every category
slug matching the load, primary first - the server checks race targets
across all of them. Consumes the quest-ptr once the quest is
identified, so one load asks once; the previous ghost is dropped the
moment a new load is seen, and the consumed pointer is forgotten again
whenever no quest is loaded (see *GHOST-FETCH-PTR*)."
  (let ((ptr (and snapshot (getf snapshot :quest-ptr))))
    (cond
      ((not (and ptr (plusp ptr)))
       (setf *ghost-fetch-ptr* nil)
       nil)
      ((and (getf snapshot :quest-name)
            (not (eql ptr *ghost-fetch-ptr*)))
       (setf *ghost-fetch-ptr* ptr
             *ghost* nil)
       (let ((defs (find-quest-defs :number (getf snapshot :quest-number)
                                    :episode (getf snapshot :episode)
                                    :name (getf snapshot :quest-name))))
         (when (and defs
                    (config-value :ghost-race)
                    (string/= (normalize-token (config-value :api-token)) ""))
           (values (mapcar #'quest-def-slug defs)
                   (difficulty-label (getf snapshot :difficulty)
                                     (getf snapshot :anguish))
                   (max 1 (length (party-of snapshot))))))))))

#+lispworks
(defun maybe-start-ghost-fetch (snapshot)
  "Kick off the background ghost fetch when a quest just loaded. The
result lands in *GHOST* only if the same quest load is still current by
the time the reply arrives. The pb dimension is always 0 here: a run is
No PB until a discharge is seen (PB-CATEGORY-AT-START-P), so the PB
fallback board at load time is the No-PB one; an explicitly chosen
target is served by the server regardless."
  (multiple-value-bind (slugs difficulty party-size)
      (ghost-fetch-wanted snapshot)
    (when slugs
      (let ((ptr *ghost-fetch-ptr*))
        (mp:process-run-function
         "eta-client-ghost-fetch" '()
         (lambda ()
           (let ((ghost (handler-case
                            (multiple-value-bind (outcome payload)
                                (fetch-ghost-splits (first slugs)
                                                    :extra-slugs (rest slugs)
                                                    :difficulty difficulty
                                                    :party-size party-size
                                                    :pb 0)
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

(defun ghost-vs-text (race)
  "'vs 12:34.567 -3.2s' for RACE, or just the target time before the
first matched room - shared by the status line and the overlay. NIL
when no ghost is attached yet."
  (let ((ghost (ghost-race-ghost race))
        (delta (ghost-race-delta-ms race)))
    (when ghost
      (format nil "vs ~a~@[ ~a~]"
              (format-run-time (ghost-time-ms ghost))
              (and delta (format-ghost-delta
                          delta (ghost-precision ghost)))))))

(defun ghost-status-suffix ()
  "' | vs 12:34.567 -3.2s' for the quest-status line while a ghost is
attached; before the first matched room only the target time shows, so
the runner knows a ghost is loaded. NIL when no race is on."
  (let ((race *ghost-race*))
    (when race
      (let ((text (ghost-vs-text race)))
        (when text
          (format nil " | ~a" text))))))

(defun ghost-title-suffix ()
  "' -3.2s' for the window title - the gap alone; the title already
leads with the live clock. NIL before the first matched room."
  (let ((race *ghost-race*))
    (when race
      (let ((ghost (ghost-race-ghost race))
            (delta (ghost-race-delta-ms race)))
        (when (and ghost delta)
          (format nil " ~a"
                  (format-ghost-delta delta (ghost-precision ghost))))))))

(defun ghost-covers-run-p (ghost run)
  "Should GHOST's final time be compared against completed RUN? The
quest slug must match (segment ghosts compare against their segment
run) and the runs must share a pb board category - unless the user
chose the target explicitly, or the ghost predates the pb field."
  (and (equal (getf run :quest-slug) (ghost-quest-slug ghost))
       (not (getf run :aborted))
       (integerp (ghost-time-ms ghost))
       (or (equal (ghost-source ghost) "target")
           (null (ghost-pb ghost))
           (eql (ghost-pb ghost) (if (getf run :pb) 1 0)))))

(defun annotate-ghost-runs (runs)
  "Stamp each completed run the current ghost covers with the final
comparison: :ghost-delta-ms (run time minus ghost time, negative = beat
the ghost), :ghost-time-ms and :ghost-label. The final gap compares the
runs' official times, so it is ms-accurate whatever the ghost's room
precision."
  (let ((ghost *ghost*))
    (if (null ghost)
        runs
        (mapcar (lambda (run)
                  (if (ghost-covers-run-p ghost run)
                      (list* :ghost-delta-ms (- (getf run :time-ms)
                                                (ghost-time-ms ghost))
                             :ghost-time-ms (ghost-time-ms ghost)
                             :ghost-label (or (ghost-label ghost) "")
                             run)
                      run))
                runs))))
