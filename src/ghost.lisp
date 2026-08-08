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
  video-p    ; 1 when a watchable hosted recording backs the ghost -
             ; the synced mini player's precondition (0/NIL otherwise)
  precision  ; :ms (room events) or :sec (frame-derived)
  rooms      ; vector of (:floor :room :nth :enter-ms), progression order
  track)     ; vector of (ms floor map x z) or (ms floor map x z y)
             ; rows, oldest first - the ghost's own position timeline
             ; for the overlay's course map and in-world marker (4 Hz
             ; from ghost-era references, 1 Hz from frames; the
             ; trailing y height rides on y-era references only)

(defstruct ghost-race
  ghost          ; attached ghost, or NIL while the fetch is in flight
  (cursor 0)     ; index into GHOST's rooms of the next unclaimed entry
  last-key       ; (floor . room) on the previous frame
  delta-ms       ; latest gap (live minus ghost); NIL before the first match
  (matched-rooms 0)
  ;; Course-map state: the submitter's own breadcrumbs and latest
  ;; position, written by the poll thread and read by the overlay
  ;; thread (freshly consed rows, so a torn read never sees a half
  ;; entry).
  own-x own-z own-y own-floor
  (own-track '())       ; newest first: (floor x z), sampled ~2/sec
  (own-track-count 0)
  last-own-ms)

(defparameter +own-track-interval-ms+ 500
  "Breadcrumb sampling interval for the live side of the course map.")

(defparameter +max-own-track-points+ 4000
  "Breadcrumb cap (about half an hour); the trace just stops growing
past it, the current-position dot keeps moving.")

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

;; Synced-video mini player state (drivers further down; the variables
;; live here because GHOST-FETCH-WANTED resets the give-up marker).

(defvar *ghost-video* nil
  "(:capture FFMPEG-CAPTURE :ghost GHOST) of the running mini player,
or NIL.")

(defvar *ghost-video-starting* nil
  "GET-INTERNAL-REAL-TIME when the link fetch / spawn thread launched,
or NIL. A timestamp rather than a flag so a thread that dies before
its cleanup (process-creation failure) cannot latch the mini player
off for the whole session - a stale stamp just expires.")

(defparameter +ghost-video-start-timeout-ms+ 30000
  "How long a start attempt may stay in flight before the stamp is
considered stale and a new attempt is allowed.")

(defvar *ghost-video-given-up* nil
  "The ghost a player start failed (or was closed) for, so one failure
does not retry every poll tick. Reset at each quest load.")

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
         :video-p (let ((video (gethash "video" payload)))
                    (and (integerp video) video))
         :precision (if (equal (gethash "precision" payload) "ms") :ms :sec)
         :rooms (map 'vector
                     (lambda (room)
                       (when (hash-table-p room)
                         (list :floor (gethash "floor" room)
                               :room (gethash "room" room)
                               :nth (gethash "nth" room)
                               :enter-ms (gethash "enter_ms" room))))
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
                                        (getf me :x 0.0) (getf me :z 0.0)
                                        elapsed :y (getf me :y 0.0))))))))

(defun ghost-race-note-position (race floor x z elapsed-ms &key y)
  "Track the submitter's own position for the course map: the current
dot every call, a breadcrumb row at most every +OWN-TRACK-INTERVAL-MS+.
Y feeds the in-world marker's height fallback for ghosts whose track
predates the height column."
  (setf (ghost-race-own-floor race) floor
        (ghost-race-own-x race) x
        (ghost-race-own-z race) z
        (ghost-race-own-y race) y)
  (let ((last (ghost-race-last-own-ms race)))
    (when (and (< (ghost-race-own-track-count race) +max-own-track-points+)
               (or (null last)
                   (>= (- elapsed-ms last) +own-track-interval-ms+)))
      (setf (ghost-race-last-own-ms race) elapsed-ms)
      (incf (ghost-race-own-track-count race))
      (push (list floor x z) (ghost-race-own-track race)))))

;;; Course map: the ghost dot's position at any elapsed time, and the
;;; projection that fits a floor's traces into the overlay's map rect.
;;; Pure, so the SBCL tests pin the interpolation and fitting.

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

(defun ghost-floor-track-points (track floor)
  "The (x z) points of TRACK's rows on FLOOR, in time order - the
course line the ghost dot runs along."
  (when (vectorp track)
    (loop :for row :across track
          :when (eql (second row) floor)
            :collect (list (fourth row) (fifth row)))))

(defun map-projection (point-groups width height &key (pad 10))
  "A function (x z) -> (values px py) that fits every point in
POINT-GROUPS (lists of (x z) lists) into WIDTH x HEIGHT with PAD,
preserving aspect and centering; NIL when there are no points or the
points span nothing. Screen y grows with -z, so 'up' on the map is the
game's north."
  (let ((min-x nil) (max-x nil) (min-z nil) (max-z nil))
    (dolist (group point-groups)
      (dolist (point group)
        (let ((x (first point)) (z (second point)))
          (setf min-x (if min-x (min min-x x) x)
                max-x (if max-x (max max-x x) x)
                min-z (if min-z (min min-z z) z)
                max-z (if max-z (max max-z z) z)))))
    (when min-x
      (let* ((span-x (max 1e-3 (- max-x min-x)))
             (span-z (max 1e-3 (- max-z min-z)))
             (scale (min (/ (- width (* 2 pad)) span-x)
                         (/ (- height (* 2 pad)) span-z)))
             (cx (/ (+ min-x max-x) 2))
             (cz (/ (+ min-z max-z) 2)))
        (lambda (x z)
          (values (round (+ (/ width 2) (* scale (- x cx))))
                  (round (- (/ height 2) (* scale (- z cz))))))))))

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

(defun ghost-direction-arrow (dx dz)
  "8-way arrow from the own dot toward the ghost, in map orientation
(up = north = -z... in game terms +z is drawn upward here, matching
MAP-PROJECTION's flipped y)."
  (let* ((angle (atan (float dz) (float dx)))  ; radians, +z = up on the map
         (octant (mod (round (* 4 (/ angle pi))) 8)))
    (aref #("→" "↗" "↑" "↖" "←" "↙" "↓" "↘") octant)))

(defun format-ghost-distance (dx dz)
  "Approximate distance label: PSO world units are ~10 per meter."
  (format nil "~dm" (max 1 (round (sqrt (+ (* dx dx) (* dz dz))) 10))))

(defun ghost-map-data (race elapsed-ms &key marker)
  "Snapshot for the overlay's course map, or NIL when there is nothing
to draw yet. All slots are plain data; the overlay thread interpolates
the ghost dot itself from :track and :elapsed-at so it glides between
the poll loop's 4 Hz updates. MARKER (the :ghost-marker setting) rides
along so the overlay thread never touches CONFIG-VALUE:
  (:floor F :own (x z) :own-y Y :own-points ((x z)...)
   :track VECTOR :ghost-time-ms MS :label STRING :marker BOOL
   :elapsed-ms MS :elapsed-at TICKS)"
  (let ((ghost (ghost-race-ghost race))
        (floor (ghost-race-own-floor race))
        (x (ghost-race-own-x race))
        (z (ghost-race-own-z race)))
    (when (and floor x z)
      (list :marker (and marker t)
            :floor floor
            :own (list x z)
            :own-y (ghost-race-own-y race)
            :own-points (loop :for (f px pz) :in (ghost-race-own-track race)
                              :when (eql f floor)
                                :collect (list px pz))
            :track (and ghost (ghost-track ghost))
            :ghost-time-ms (and ghost (ghost-time-ms ghost))
            :label (and ghost (ghost-label ghost))
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
       ;; reference can neither race the next quest nor let an
       ;; in-flight video fetch spawn its player after the run.
       ;; (Completed runs are annotated while the quest is still
       ;; loaded, so clearing here never robs a finish of its note.)
       (setf *ghost-fetch-ptr* nil
             *ghost* nil)
       nil)
      ((and (getf snapshot :quest-name)
            (not (eql ptr *ghost-fetch-ptr*)))
       (setf *ghost-fetch-ptr* ptr
             *ghost* nil
             *ghost-video-given-up* nil)
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

;;; Ghost synced-video mini player: the reference run's hosted
;;; recording in a small always-on-top ffplay window, seeked so it
;;; shows the ghost's screen at the live run's elapsed time - the
;;; closest thing to running alongside the ghost that read-only access
;;; allows. One player at a time; the quest ending (or the setting
;;; going off) kills it, closing it by hand just gives it up for the
;;; run. (State variables live up with the fetch state.)

(defun resolve-ffplay-path ()
  "ffplay.exe next to the resolved ffmpeg (bundled or configured), or
the bare \"ffplay.exe\" when ffmpeg itself resolved to a bare
PATH-searched name - CreateProcess then searches the PATH the same way
it does for ffmpeg, instead of probing the process CWD. NIL when the
sibling is missing; the mini player is optional."
  (let ((ffmpeg (resolve-ffmpeg-path)))
    (when ffmpeg
      (if (null (pathname-directory (pathname ffmpeg)))
          "ffplay.exe"
          (let ((ffplay (merge-pathnames
                         "ffplay.exe"
                         (uiop:pathname-directory-pathname
                          (pathname ffmpeg)))))
            (and (probe-file ffplay) (namestring ffplay)))))))

#+lispworks
(defun ghost-video-position ()
  "(values left top) for the player: the game window's bottom-left
corner, or NILs when the window is gone (ffplay then places itself)."
  (let* ((hwnd (find-psobb-window))
         (rect (and hwnd (window-rect-of hwnd))))
    (if rect
        (destructuring-bind (left top right bottom) rect
          (declare (ignore right))
          (values (+ left 16) (max top (- bottom 270 60))))
        (values nil nil))))

#+lispworks
(defun stop-ghost-video! ()
  "Kill the mini player if one is up. Safe from any thread."
  (let ((playing *ghost-video*))
    (when playing
      (setf *ghost-video* nil)
      (let ((capture (getf playing :capture)))
        (ignore-errors
          (%terminate-process (ffmpeg-capture-process-handle capture) 1))
        (ignore-errors (close-capture-handles capture))))))

#+lispworks
(defun start-ghost-video-player (ghost telemetry detector)
  "Fetch the ghost's video link and spawn ffplay seeked to the live
elapsed time (video_offset_ms maps timer zero into the video). Runs on
its own thread; every failure just gives the ghost's video up for this
run. The quest state, the setting and the ghost's currency are all
re-checked AFTER the fetch: a run that ended (or a toggle flipped)
during the 1-3 s round trip must not pop a player over the results."
  (unwind-protect
       (handler-case
           (multiple-value-bind (outcome url offset-ms)
               (fetch-ghost-video-link (ghost-run-id ghost))
             (let ((ffplay (resolve-ffplay-path)))
               (when (and (eq outcome :ok) (stringp url) ffplay
                          (eq *ghost* ghost)
                          (eq (detector-state detector) :in-quest)
                          (config-value :ghost-video))
                 (let ((seek (/ (+ (or offset-ms 0)
                                   (telemetry-elapsed-ms
                                    telemetry (get-internal-real-time)))
                                1000.0)))
                   (multiple-value-bind (left top) (ghost-video-position)
                     (let ((capture
                             (spawn-process
                              ffplay
                              (append
                               (list "-hide_banner" "-loglevel" "error"
                                     "-an" "-noborder" "-alwaysontop"
                                     "-autoexit"
                                     "-x" "480" "-y" "270"
                                     "-window_title" "Rappy Runs Ghost"
                                     "-ss" (format nil "~,2f" (max 0.0 seek)))
                               (when left
                                 (list "-left" (princ-to-string left)
                                       "-top" (princ-to-string top)))
                               (list url)))))
                       (setf *ghost-video*
                             (list :capture capture :ghost ghost))))))))
         (error () nil))
    (unless (and *ghost-video*
                 (eq (getf *ghost-video* :ghost) ghost))
      (setf *ghost-video-given-up* ghost))
    (setf *ghost-video-starting* nil)))

#+lispworks
(defun ghost-video-start-in-flight-p ()
  "A start attempt is in flight and its stamp has not gone stale."
  (let ((stamp *ghost-video-starting*))
    (and stamp
         (< (round (* 1000 (- (get-internal-real-time) stamp))
                   internal-time-units-per-second)
            +ghost-video-start-timeout-ms+))))

#+lispworks
(defun ghost-video-capture-safe-p ()
  "The mini player may only show when the game runs windowed: windowed
recordings capture the game window itself (WGC / gdigrab), which the
player's window cannot pollute, while fullscreen recordings duplicate
the whole monitor and would burn the always-on-top player into the
submitted footage - the v0.47 write-in problem all over again."
  (let ((hwnd (find-psobb-window)))
    (and hwnd (not (window-covers-monitor-p hwnd)))))

#+lispworks
(defun ghost-video-step (detector)
  "Poll-loop driver: start the mini player when a videoed ghost races
and the setting is on, notice it dying (user closed it / hit the end),
kill it when the quest is over. Safe to call every tick."
  (let* ((race *ghost-race*)
         (ghost (and race (ghost-race-ghost race)))
         (playing *ghost-video*))
    (cond
      ((or (not (eq (detector-state detector) :in-quest))
           (not (config-value :ghost-video)))
       (stop-ghost-video!))
      ((and playing (not (capture-alive-p (getf playing :capture))))
       ;; Closed by hand or played out: no restart this run.
       (setf *ghost-video-given-up* (getf playing :ghost)
             *ghost-video* nil)
       (ignore-errors (close-capture-handles (getf playing :capture))))
      ((and ghost (null playing)
            (not (ghost-video-start-in-flight-p))
            (not (eq ghost *ghost-video-given-up*))
            (eql (ghost-video-p ghost) 1)
            (ghost-run-id ghost)
            (detector-telemetry detector)
            (ghost-video-capture-safe-p))
       (setf *ghost-video-starting* (get-internal-real-time))
       (let ((telemetry (detector-telemetry detector)))
         (mp:process-run-function
          "eta-client-ghost-video" '()
          (lambda ()
            (start-ghost-video-player ghost telemetry detector))))))))

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
