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
  rooms      ; vector of (:floor :room :nth :enter-ms), progression order
  track)     ; vector of (ms floor map x z) or (ms floor map x z y)
             ; rows, oldest first - the ghost's own position timeline
             ; for the overlay's in-world marker (4 Hz from ghost-era
             ; references, 1 Hz from frames; the trailing y height
             ; rides on y-era references only)

(defstruct ghost-race
  ghost          ; attached ghost, or NIL while the fetch is in flight
  (cursor 0)     ; index into GHOST's rooms of the next unclaimed entry
  last-key       ; (floor . room) on the previous frame
  delta-ms       ; latest gap (live minus ghost); NIL before the first match
  (matched-rooms 0)
  (splits '())   ; matched-room history for the overlay, newest first:
                 ; (:room R :floor F :ms enter :delta cumulative-gap)
                 ; plists, freshly consed so the overlay thread can walk
                 ; a torn-read prefix safely
  ;; In-world marker support, written by the poll thread and read by
  ;; the overlay thread: the live player's floor gates the marker to
  ;; same-floor ghosts, the height is its fallback for tracks that
  ;; predate the height column.
  own-y own-floor)

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

(defvar *live-camera* nil
  "The freshest camera plist from READ-CAMERA while a quest runs, or
NIL. Written by the poll loop at its full rate (GHOST-RACE-STEP) and
read by the overlay thread: the in-world marker must pan with the
camera, and the 4 Hz status-update cadence visibly lags a turn. The
single variable swap of a freshly consed plist is atomic enough.")

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
                               :enter-ms (gethash "enter_ms" room)
                               ;; The reference's kills during this
                               ;; visit, or NIL from a server that does
                               ;; not annotate them - NIL means unknown
                               ;; (show the split), not zero.
                               :kills (let ((kills (gethash "kills" room)))
                                        (and (numberp kills)
                                             (round kills))))))
                     rooms)
         :track (let ((track (gethash "track" payload))
                      ;; The height column rides out-of-band in
                      ;; "track_y" (server hunt:wire-track): rows stay
                      ;; 5 elements on the wire so v0.51/v0.52 clients
                      ;; keep their course map. Zip it back on here,
                      ;; indexed against the RAW track so a dropped
                      ;; malformed row cannot shift later heights.
                      (heights (gethash "track_y" payload)))
                  (when (vectorp track)
                    (coerce
                     (loop :for row :across track
                           :for i :from 0
                           :when (and (vectorp row) (<= 5 (length row) 6)
                                      (every #'numberp row))
                             :collect
                             (let ((row (coerce row 'list))
                                   (y (and (vectorp heights)
                                           (< i (length heights))
                                           (numberp (aref heights i))
                                           (aref heights i))))
                               (if (and y (= (length row) 5))
                                   (append row (list y))
                                   row)))
                     'vector))))))))

(defun ghost-race-note-room (race floor room elapsed-ms)
  "Feed the submitter's current (floor, room) at ELAPSED-MS (on the
quest telemetry's clock). On entry into a new room, claim the ghost's
next visit of that room from the cursor onward (bounded lookahead),
update the gap to live-minus-ghost and record the split for the
overlay's room list - unless the reference killed nothing there (a
pass-through corridor, noise among the fight rooms; an entry without
kill data still records, unknown is not empty). Rooms the ghost never
entered - or entered outside the window - leave the previous gap
standing and record no split, mirroring the compare page's one-sided
rows. Returns the race's current gap."
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
                  :do (let ((delta (- elapsed-ms (getf entry :enter-ms)))
                            (kills (getf entry :kills)))
                        (setf (ghost-race-cursor race) (1+ i)
                              (ghost-race-delta-ms race) delta)
                        (incf (ghost-race-matched-rooms race))
                        (when (or (null kills) (plusp kills))
                          (push (list :room room :floor floor
                                      :ms elapsed-ms :delta delta)
                                (ghost-race-splits race)))
                        (return)))))))
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
      (setf *ghost-race* nil
            *live-camera* nil)
      (let ((race (or *ghost-race*
                      (setf *ghost-race* (make-ghost-race))))
            (ghost *ghost*))
        (setf *live-camera* (getf snapshot :camera))
        (when (and ghost
                   (not (eq (ghost-race-ghost race) ghost))
                   (ghost-matches-snapshot-p ghost snapshot))
          (setf (ghost-race-ghost race) ghost))
        (let ((me (snapshot-my-player snapshot))
              (telemetry (detector-telemetry detector)))
          (when (and me telemetry)
            (let ((elapsed (telemetry-elapsed-ms
                            telemetry (get-internal-real-time))))
              (ghost-race-note-room race (getf me :floor 0) (getf me :room 0)
                                    elapsed)
              (ghost-race-note-position race (getf me :floor 0)
                                        (getf me :y 0.0))))))))

(defun ghost-race-note-position (race floor y)
  "Track the submitter's own floor and height for the in-world marker:
the floor gates it to same-floor ghosts, Y is its height fallback for
ghosts whose track predates the height column."
  (setf (ghost-race-own-floor race) floor
        (ghost-race-own-y race) y))

;;; In-world marker: the ghost dot's position at any elapsed time.
;;; Pure, so the SBCL tests pin the interpolation.

(defparameter +track-lerp-max-gap-ms+ 3000
  "Interpolate the ghost dot between two track samples only when they
are this close; across a bigger gap (a warp, missing data) the dot
snaps instead of gliding through walls.")

(defun ghost-track-position (track elapsed-ms)
  "(values floor map x z y) of the ghost at ELAPSED-MS on TRACK (a
vector of (ms floor map x z) or (ms floor map x z y) rows, oldest
first), linearly interpolated between neighboring samples on the same
floor. Y is NIL on pre-height rows (the marker then borrows the live
player's own height). NIL before the first sample or on an empty
track; past the last sample the dot rests there."
  (when (and (vectorp track) (plusp (length track)))
    (let* ((n (length track))
           ;; Binary search: the last row with ms <= elapsed.
           (lo 0) (hi (1- n)))
      (when (>= elapsed-ms (first (aref track 0)))
        (loop :while (< lo hi)
              :do (let ((mid (ceiling (+ lo hi) 2)))
                    (if (<= (first (aref track mid)) elapsed-ms)
                        (setf lo mid)
                        (setf hi (1- mid)))))
        (destructuring-bind (ms floor map x z &optional y) (aref track lo)
          (if (>= (1+ lo) n)
              (values floor map x z y)
              (destructuring-bind (next-ms next-floor next-map next-x next-z
                                   &optional next-y)
                  (aref track (1+ lo))
                (declare (ignore next-map))
                (if (and (eql next-floor floor)
                         (< (- next-ms ms) +track-lerp-max-gap-ms+)
                         (> next-ms ms))
                    (let ((f (/ (- elapsed-ms ms)
                                (float (- next-ms ms)))))
                      (values floor map
                              (+ x (* f (- next-x x)))
                              (+ z (* f (- next-z z)))
                              (and y next-y (+ y (* f (- next-y y))))))
                    (values floor map x z y)))))))))

(defun overlay-corner-origin (corner area-w area-h w h margin-x margin-y)
  "Top-left (values x y) of a W x H panel at CORNER of an AREA-W x
AREA-H area, MARGIN-X/-Y in from the nearest edges (centered axes take
no margin). CORNER combines a vertical position (top / middle /
bottom) with a horizontal one (left / center / right): the four
corners :top-right :top-left :bottom-right :bottom-left, the edge
midpoints :middle-right :middle-left :top-center :bottom-center;
anything else (a hand-edited config) places like :top-right. Origins
clamp at 0 so an area smaller than the panel degrades to flush edges
rather than a negative origin."
  (values (case corner
            ((:top-left :middle-left :bottom-left) margin-x)
            ((:top-center :bottom-center) (max 0 (floor (- area-w w) 2)))
            (t (max 0 (- area-w w margin-x))))
          (case corner
            ((:bottom-left :bottom-center :bottom-right)
             (max 0 (- area-h h margin-y)))
            ((:middle-left :middle-right) (max 0 (floor (- area-h h) 2)))
            (t margin-y))))

;;; In-world marker projection: the ghost's world position through the
;;; game camera onto the client area, so the overlay can draw a marker
;;; where the ghost actually stands. Transcribed from the DropBox
;;; Tracker / PartyMemberTracker addons' field-verified math: a linear
;;; projection onto the view plane, with an FOV heuristic keyed off
;;; the camera zoom step. Pure, so the SBCL tests pin the geometry.

(defparameter +camera-fov-aspect-factor+ 0.56470588
  "768/1360: the addons' empirical constant relating the aspect ratio
to the base field of view.")

(defun camera-fov (zoom aspect)
  "The screen FOV in radians for camera ZOOM step (0-4, clamped) at
ASPECT ratio - the addons' heuristic, valid for aspect ratios around
1.25-1.78."
  (let* ((zoom (min 4 (max 0 (or zoom 1))))
         (degrees (- (* 2 (atan (* +camera-fov-aspect-factor+ aspect))
                        (/ 180 pi))
                     (* (- zoom 1) 0.600)
                     (* (min zoom 1) 0.300))))
    (* degrees (/ pi 180))))

(defun ghost-screen-position (camera width height wx wy wz)
  "(values sx sy) of world point (WX WY WZ) on the game's WIDTH x
HEIGHT client area, projected through CAMERA (READ-CAMERA's plist), or
NIL when the point is behind the camera, degenerate, or CAMERA is
missing pieces. The eye direction is a unit vector; a zeroed one
(loading screens) fails the front-facing test and returns NIL."
  (when (and camera (numberp width) (numberp height)
             (plusp width) (plusp height))
    (let ((ex (getf camera :x)) (ey (getf camera :y)) (ez (getf camera :z))
          (dx (getf camera :dir-x)) (dy (getf camera :dir-y))
          (dz (getf camera :dir-z)))
      (when (and (numberp ex) (numberp ey) (numberp ez)
                 (numberp dx) (numberp dy) (numberp dz))
        (let* ((vx (- wx ex)) (vy (- wy ey)) (vz (- wz ez))
               (len (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))
          (when (> len 1e-3)
            (setf vx (/ vx len) vy (/ vy len) vz (/ vz len))
            (let ((fdp (+ (* dx vx) (* dy vy) (* dz vz))))
              (when (> fdp 1e-7)
                (let* ((aspect (/ width (float height)))
                       (fov (camera-fov (getf camera :zoom) aspect))
                       (determinant (/ (* aspect height)
                                       (* 2 (tan (* 0.5 fov)))))
                       (s (/ determinant fdp))
                       (px (* s vx)) (py (* s vy)) (pz (* s vz))
                       ;; right = dir x up(0,1,0); up' = right x dir.
                       ;; Deliberately NOT normalized (their magnitude
                       ;; is dir's horizontal component), matching the
                       ;; addons' field-verified math verbatim: the FOV
                       ;; heuristic above was tuned against exactly
                       ;; this scaling, so "fixing" it here would move
                       ;; every marker the addons place correctly.
                       (rx (- dz)) (rz dx)
                       (ux (- (* dx dy)))
                       (uy (+ (* dx dx) (* dz dz)))
                       (uz (- (* dy dz))))
                  (values (round (+ (/ width 2)
                                    (+ (* rx px) (* rz pz))))
                          (round (- (/ height 2)
                                    (+ (* ux px) (* uy py)
                                       (* uz pz))))))))))))))

(defparameter +overlay-split-rows+ 8
  "How many of the newest matched-room splits ride to the overlay -
and how many rows its panel reserves.")

(defun format-split-clock (ms)
  "m:ss for a split row's enter clock; the delta column carries the
precision, the clock only anchors the row on the run's timeline."
  (multiple-value-bind (minutes seconds) (floor (floor ms 1000) 60)
    (format nil "~d:~2,'0d" minutes seconds)))

(defun ghost-overlay-data (race elapsed-ms &key marker)
  "Snapshot for the overlay's ghost panel (vs header, room-split rows
and the in-world marker), or NIL until a ghost is attached and the
player's position has been seen - the ghostless overlay stays the
compact timer pill. All slots are plain data; the overlay thread
interpolates the marker itself from :track and :elapsed-at so it
glides between the poll loop's 4 Hz updates. MARKER (the
:ghost-marker setting) rides along so the overlay thread never
touches CONFIG-VALUE:
  (:floor F :own-y Y :track VECTOR :ghost-time-ms MS :label STRING
   :precision P :marker BOOL :splits (newest first, capped at
   +OVERLAY-SPLIT-ROWS+) :elapsed-ms MS :elapsed-at TICKS)"
  (let ((ghost (ghost-race-ghost race))
        (floor (ghost-race-own-floor race)))
    (when (and ghost floor)
      (list :marker (and marker t)
            :floor floor
            :own-y (ghost-race-own-y race)
            :track (ghost-track ghost)
            :ghost-time-ms (ghost-time-ms ghost)
            :label (ghost-label ghost)
            :precision (ghost-precision ghost)
            :splits (loop :for split :in (ghost-race-splits race)
                          :repeat +overlay-split-rows+
                          :collect split)
            :elapsed-ms elapsed-ms
            :elapsed-at (get-internal-real-time)))))

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
       ;; No quest loaded: forget the load AND the ghost, so a stale
       ;; reference can never race the next quest. (Completed runs are
       ;; annotated while the quest is still loaded, so clearing here
       ;; never robs a finish of its note.)
       (setf *ghost-fetch-ptr* nil
             *ghost* nil)
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
                    ;; The guest token counts: an anonymous player's own
                    ;; PBs live under the guest account, so they get
                    ;; ghost races against themselves too.
                    (string/= (submission-token) ""))
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
