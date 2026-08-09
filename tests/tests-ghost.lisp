;;; Ghost race: split parsing, streaming room alignment, formatting and
;;; the completion annotation (client/src/ghost.lisp), plus the
;;; room-event telemetry that feeds the server's ms-precision splits.

(in-package :ephinea-ta-client-tests)

(defparameter +ghost-payload+
  "{\"run_id\":42,\"quest\":\"ep1-towards-the-future\",\"time_ms\":123456,
    \"submitter\":\"teapot\",\"precision\":\"ms\",\"source\":\"pb\",\"pb\":0,
    \"rooms\":[{\"floor\":1,\"room\":10,\"nth\":1,\"enter_ms\":0},
               {\"floor\":1,\"room\":11,\"nth\":1,\"enter_ms\":30000},
               {\"floor\":1,\"room\":10,\"nth\":2,\"enter_ms\":60000},
               {\"floor\":2,\"room\":1,\"nth\":1,\"enter_ms\":90000}]}")

(defun test-ghost ()
  (parse-ghost-splits (com.inuoe.jzon:parse +ghost-payload+)))

(defun run-ghost-tests ()
  (let ((ghost (test-ghost)))
    (check "ghost payload parses"
           (and ghost
                (= (ghost-time-ms ghost) 123456)
                (equal (ghost-quest-slug ghost) "ep1-towards-the-future")
                (equal (ghost-label ghost) "teapot")
                (equal (ghost-source ghost) "pb")
                (eql (ghost-pb ghost) 0)
                (eq (ghost-precision ghost) :ms)
                (= (length (ghost-rooms ghost)) 4)))
    (check "malformed payload parses to NIL"
           (and (null (parse-ghost-splits
                       (com.inuoe.jzon:parse "{\"rooms\":[]}")))
                (null (parse-ghost-splits nil))))
    ;; Streaming greedy alignment: a cursor over the ghost's room list,
    ;; the same progression-order identity /compare aligns rooms by.
    (let ((race (make-ghost-race :ghost ghost)))
      (check "start room matches at once"
             (eql (ghost-race-note-room race 1 10 800) 800))
      (check "staying in the room does not rematch"
             (eql (ghost-race-note-room race 1 10 5000) 800))
      (check "next room updates the gap"
             (eql (ghost-race-note-room race 1 11 34000) 4000))
      (check "a room the ghost never entered keeps the previous gap"
             (eql (ghost-race-note-room race 1 99 40000) 4000))
      (check "a revisit matches the ghost's next visit of that room"
             (eql (ghost-race-note-room race 1 10 55000) -5000))
      (check "matched-room count"
             (= (ghost-race-matched-rooms race) 3)))
    ;; A room the GHOST passed through but the live side skips must not
    ;; desync later matches: the cursor lookahead steps over it.
    (let ((race (make-ghost-race :ghost ghost)))
      (ghost-race-note-room race 1 10 500)
      (check "skipping a ghost room still matches the one after it"
             (eql (ghost-race-note-room race 2 1 92000) 2000)))
    ;; A late-arriving ghost joins mid-route: the race tracked the
    ;; progression from the start, and the cursor finds the current
    ;; position on the next room change.
    (let ((race (make-ghost-race)))
      (check "no ghost yet, no gap"
             (null (ghost-race-note-room race 1 10 800)))
      (setf (ghost-race-ghost race) ghost)
      (check "late-attached ghost matches from the next room on"
             (eql (ghost-race-note-room race 1 11 31000) 1000)))
    ;; The lookahead bound keeps a detour from consuming the whole list.
    (let* ((rooms (coerce
                   (append
                    (loop :for i :from 0 :below 20
                          :collect (list :floor 1 :room (+ 100 i)
                                         :nth 1 :enter-ms (* 1000 i)))
                    (list (list :floor 9 :room 9 :nth 1 :enter-ms 99000)))
                   'vector))
           (far (make-ghost :quest-slug "q" :time-ms 1 :precision :ms
                            :rooms rooms))
           (race (make-ghost-race :ghost far)))
      (check "a match past the lookahead window is not taken"
             (null (ghost-race-note-room race 9 9 500)))
      (check "the unmatched entry leaves the cursor in place"
             (eql (ghost-race-note-room race 1 100 1500) 1500)))
    ;; Status suffix.
    (let ((eta-client::*ghost-race* (make-ghost-race :ghost ghost)))
      (check "status suffix shows the target before any match"
             (equal (ghost-status-suffix) " | vs 2:03.456"))
      (setf (ghost-race-delta-ms eta-client::*ghost-race*) 4000)
      (check "status suffix shows the gap after a match"
             (equal (ghost-status-suffix) " | vs 2:03.456 +4.0s")))
    (let ((eta-client::*ghost-race* (make-ghost-race)))
      (check "race without an attached ghost shows nothing"
             (null (ghost-status-suffix))))
    (let ((eta-client::*ghost-race* nil))
      (check "no race, no suffix" (null (ghost-status-suffix))))
    ;; Completion annotation.
    (let ((eta-client::*ghost* ghost))
      (let ((annotated (annotate-ghost-runs
                        (list (list :quest-slug "ep1-towards-the-future"
                                    :time-ms 113456)
                              (list :quest-slug "some-other-quest"
                                    :time-ms 113456)
                              (list :quest-slug "ep1-towards-the-future"
                                    :time-ms 113456 :aborted t)
                              ;; The live run discharged a Photon Blast:
                              ;; another board, no comparison.
                              (list :quest-slug "ep1-towards-the-future"
                                    :time-ms 113456 :pb t)))))
        (check "beat-the-ghost delta stamped"
               (and (eql (getf (first annotated) :ghost-delta-ms) -10000)
                    (eql (getf (first annotated) :ghost-time-ms) 123456)
                    (equal (getf (first annotated) :ghost-label) "teapot")))
        (check "other quest untouched"
               (null (getf (second annotated) :ghost-delta-ms)))
        (check "aborted run untouched"
               (null (getf (third annotated) :ghost-delta-ms)))
        (check "pb category mismatch withholds the comparison"
               (null (getf (fourth annotated) :ghost-delta-ms)))))
    ;; An explicitly chosen target compares across pb categories - the
    ;; user picked it on purpose.
    (let ((target (test-ghost)))
      (setf (eta-client::ghost-source target) "target")
      (check "a chosen target ignores the pb dimension"
             (ghost-covers-run-p target
                                 (list :quest-slug "ep1-towards-the-future"
                                       :time-ms 1 :pb t))))
    (let ((eta-client::*ghost* nil))
      (check "no ghost leaves runs alone"
             (let ((runs (list (list :quest-slug "q" :time-ms 1))))
               (eq runs (annotate-ghost-runs runs))))))
  ;; Gap formatting.
  (check "ms gap formats with one decimal"
         (equal (format-ghost-delta -3210 :ms) "-3.2s"))
  (check "sec gap formats whole seconds"
         (equal (format-ghost-delta 4400 :sec) "+4s"))
  (check "sec gap rounds"
         (equal (format-ghost-delta -1600 :sec) "-2s"))
  ;; Query-value encoding for the fetch.
  (check "url encoding passes unreserved and encodes spaces"
         (equal (url-encode-component "Very Hard") "Very%20Hard"))
  (check "url encoding is utf-8 percent-encoded"
         (equal (url-encode-component "Aあ") "A%E3%81%82"))
  ;; Room events (ms precision for future ghosts).
  (let ((telemetry (make-telemetry :start-time 0)))
    (eta-client::update-room-tracking telemetry '(:floor 0 :room 0) 0 33)
    (eta-client::update-room-tracking telemetry '(:floor 0 :room 0) 1 1033)
    (eta-client::update-room-tracking telemetry '(:floor 1 :room 3) 2 2345)
    (let ((events (reverse (eta-client::telemetry-events telemetry))))
      (check "room events record once per change"
             (= (length events) 2))
      (check "room event carries floor, room and ms"
             (let ((event (second events)))
               (and (equal (getf event :type) "room")
                    (eql (getf event :floor) 1)
                    (eql (getf event :room) 3)
                    (eql (getf event :ms) 2345)))))
    (let* ((json (eta-client::telemetry-json
                  (eta-client:telemetry-run-data telemetry)))
           (events (gethash "events" json))
           (event (aref events 1)))
      (check "event json carries room and ms"
             (and (eql (gethash "room" event) 3)
                  (eql (gethash "ms" event) 2345)
                  (eql (gethash "floor" event) 1)
                  (equal (gethash "type" event) "room")))))
  ;; Fetch gating: one ask per quest load, refetch after a lobby visit
  ;; (even at the same allocation address), none without the setting.
  (let* ((def (first eta-client::*quest-defs*))
         (snapshot (list :quest-ptr 4660
                         :quest-name (first (eta-client:quest-def-names def))
                         :quest-number (eta-client:quest-def-number def)
                         :episode (eta-client:quest-def-episode def)
                         :difficulty 3
                         :anguish nil
                         :players '((:name "a" :class "HUcast")))))
    (let ((eta-client::*config* (list* :api-token "tok"
                                       (copy-list eta-client::*default-config*)))
          (eta-client::*ghost-fetch-ptr* nil)
          (eta-client::*ghost* nil))
      (multiple-value-bind (slugs difficulty party-size)
          (eta-client::ghost-fetch-wanted snapshot)
        (check "fetch wanted on a fresh quest load"
               (and (equal (first slugs) (eta-client:quest-def-slug def))
                    (equal difficulty "Ultimate")
                    (eql party-size 1))))
      (check "same load never asks twice"
             (null (eta-client::ghost-fetch-wanted snapshot)))
      ;; Lobby (no quest loaded) forgets the pointer, so reloading the
      ;; quest at the same address fetches again.
      (eta-client::ghost-fetch-wanted (list :quest-ptr 0))
      (check "reload after a lobby visit asks again"
             (multiple-value-bind (slugs) (eta-client::ghost-fetch-wanted snapshot)
               (equal (first slugs) (eta-client:quest-def-slug def)))))
    (let ((eta-client::*config* (list* :ghost-race nil :api-token "tok"
                                       (copy-list eta-client::*default-config*)))
          (eta-client::*ghost-fetch-ptr* nil)
          (eta-client::*ghost* nil))
      (check "setting off never asks"
             (null (eta-client::ghost-fetch-wanted snapshot))))))

;;; Ghost overlay data: track parsing and interpolation (the in-world
;;; marker), the room-split history and the panel snapshot.

(defparameter +tracked-ghost-payload+
  "{\"run_id\":7,\"quest\":\"q\",\"time_ms\":6000,\"precision\":\"ms\",
    \"source\":\"pb\",\"pb\":0,\"rooms\":[],
    \"track\":[[0,1,10,0.0,0.0],[1000,1,10,10.0,20.0],
               [5000,2,11,50.0,50.0],\"junk\",[1,2]]}")

(defun run-ghost-overlay-tests ()
  (let ((ghost (parse-ghost-splits
                (com.inuoe.jzon:parse +tracked-ghost-payload+))))
    (check "track parses and drops malformed rows"
           (and ghost
                (= (length (ghost-track ghost)) 3)
                ;; jzon parses reals as doubles, so compare with =.
                (every #'= (aref (ghost-track ghost) 1)
                       '(1000 1 10 10.0 20.0))))
    (let ((track (ghost-track ghost)))
      (check "position before the first sample is NIL"
             (null (ghost-track-position track -1)))
      (check "position interpolates on one floor"
             (multiple-value-bind (floor map x z)
                 (ghost-track-position track 500)
               (and (eql floor 1) (eql map 10)
                    (< (abs (- x 5.0)) 0.01)
                    (< (abs (- z 10.0)) 0.01))))
      (check "a floor change snaps instead of gliding"
             (multiple-value-bind (floor map x z)
                 (ghost-track-position track 3000)
               (declare (ignore map))
               (and (eql floor 1) (= x 10.0) (= z 20.0))))
      (check "past the end the dot rests on the last sample"
             (multiple-value-bind (floor map x z)
                 (ghost-track-position track 999999)
               (declare (ignore map))
               (and (eql floor 2) (= x 50.0) (= z 50.0)))))
    ;; A wide sample gap also snaps (warps must not glide through walls).
    (let ((gappy (coerce (list (list 0 1 10 0.0 0.0)
                               (list 8000 1 10 100.0 0.0))
                         'vector)))
      (check "a gap beyond the lerp window snaps"
             (multiple-value-bind (floor map x)
                 (ghost-track-position gappy 4000)
               (declare (ignore map))
               (and (eql floor 1) (eql x 0.0)))))
    (check "a pre-height track yields NIL y"
           (multiple-value-bind (floor map x z y)
               (ghost-track-position (ghost-track ghost) 500)
             (declare (ignore floor map x z))
             (null y))))
  ;; The height column arrives out-of-band in "track_y" (the wire keeps
  ;; rows 5-wide for v0.51/v0.52 clients; server hunt:wire-track). It is
  ;; zipped back on by RAW track index, so a dropped malformed row must
  ;; not shift later heights; a heights vector shorter than the track
  ;; leaves the tail heightless.
  (let ((ghost (parse-ghost-splits
                (com.inuoe.jzon:parse
                 "{\"run_id\":8,\"quest\":\"q\",\"time_ms\":6000,
                   \"precision\":\"ms\",\"source\":\"pb\",\"pb\":0,
                   \"rooms\":[],
                   \"track\":[[0,1,10,0.0,0.0],
                              \"junk\",
                              [1000,1,10,10.0,20.0],
                              [2000,1,10,20.0,20.0]],
                   \"track_y\":[5.0,99.0,15.0]}"))))
    (check "out-of-band heights zip onto the rows by raw index"
           (and ghost
                (= (length (ghost-track ghost)) 3)
                (every #'= (aref (ghost-track ghost) 0)
                       '(0 1 10 0.0 0.0 5.0))
                ;; The junk row's 99.0 is skipped with it.
                (every #'= (aref (ghost-track ghost) 1)
                       '(1000 1 10 10.0 20.0 15.0))
                ;; Heights exhausted: the tail row stays 5-wide.
                (= (length (aref (ghost-track ghost) 2)) 5)))
    (check "height interpolates between samples"
           (multiple-value-bind (floor map x z y)
               (ghost-track-position (ghost-track ghost) 500)
             (declare (ignore map z))
             (and (eql floor 1)
                  (< (abs (- x 5.0)) 0.01)
                  (< (abs (- y 10.0)) 0.01))))
    (check "a lerp into a heightless row yields NIL y"
           (multiple-value-bind (floor map x z y)
               (ghost-track-position (ghost-track ghost) 1500)
             (declare (ignore floor map x z))
             (null y))))
  ;; Inline six-element rows (e.g. a future wire) still parse.
  (let ((ghost (parse-ghost-splits
                (com.inuoe.jzon:parse
                 "{\"run_id\":9,\"quest\":\"q\",\"time_ms\":6000,
                   \"precision\":\"ms\",\"source\":\"pb\",\"pb\":0,
                   \"rooms\":[],
                   \"track\":[[0,1,10,0.0,0.0,5.0]]}"))))
    (check "inline height-bearing rows parse too"
           (and ghost
                (every #'= (aref (ghost-track ghost) 0)
                       '(0 1 10 0.0 0.0 5.0)))))
  ;; Own-position sampling into the race state (marker support), and
  ;; the split history behind the overlay's room rows.
  (let ((race (make-ghost-race)))
    (ghost-race-note-position race 1 5.0)
    (check "own floor and height update"
           (and (eql (eta-client::ghost-race-own-floor race) 1)
                (eql (eta-client::ghost-race-own-y race) 5.0)))
    (check "no ghost, no overlay data"
           (null (ghost-overlay-data race 600))))
  (let ((race (make-ghost-race :ghost (test-ghost))))
    (check "no position seen yet, no overlay data"
           (null (ghost-overlay-data race 600)))
    (ghost-race-note-position race 1 5.0)
    (ghost-race-note-room race 1 10 800)
    (ghost-race-note-room race 1 11 34000)
    (ghost-race-note-room race 1 99 40000)  ; ghost never went: no split
    (let ((splits (eta-client::ghost-race-splits race)))
      (check "matched rooms record splits newest first"
             (and (= (length splits) 2)
                  (eql (getf (first splits) :room) 11)
                  (eql (getf (first splits) :delta) 4000)
                  (eql (getf (second splits) :room) 10)
                  (eql (getf (second splits) :ms) 800)
                  (eql (getf (second splits) :delta) 800))))
    (let ((data (ghost-overlay-data race 40000 :marker t)))
      (check "overlay data carries splits, precision and marker"
             (and (eql (getf data :floor) 1)
                  (eql (getf data :own-y) 5.0)
                  (eq (getf data :precision) :ms)
                  (getf data :marker)
                  (= (length (getf data :splits)) 2)
                  (eql (getf (first (getf data :splits)) :room) 11))))
    (setf (eta-client::ghost-race-splits race)
          (loop :for i :from 20 :downto 1
                :collect (list :room i :floor 1 :ms (* i 1000) :delta 0)))
    (check "overlay data caps the split rows"
           (= (length (getf (ghost-overlay-data race 0) :splits))
              eta-client::+overlay-split-rows+)))
  ;; A room where the reference killed nothing is matched (the gap
  ;; still moves) but records no split row; kills absent from the wire
  ;; (an older server) means unknown, so those rooms keep their rows -
  ;; the +ghost-payload+ tests above cover that path.
  (let* ((ghost (parse-ghost-splits
                 (com.inuoe.jzon:parse
                  "{\"run_id\":10,\"quest\":\"q\",\"time_ms\":99000,
                    \"precision\":\"ms\",\"source\":\"pb\",\"pb\":0,
                    \"rooms\":[{\"floor\":1,\"room\":10,\"nth\":1,
                                \"enter_ms\":0,\"kills\":3},
                               {\"floor\":1,\"room\":11,\"nth\":1,
                                \"enter_ms\":30000,\"kills\":0},
                               {\"floor\":1,\"room\":12,\"nth\":1,
                                \"enter_ms\":60000,\"kills\":5}]}")))
         (race (make-ghost-race :ghost ghost)))
    (ghost-race-note-room race 1 10 500)
    (ghost-race-note-room race 1 11 34000)
    (check "the zero-kill room still moves the gap"
           (eql (ghost-race-delta-ms race) 4000))
    (ghost-race-note-room race 1 12 61000)
    (check "a zero-kill room records no split row"
           (and (= (eta-client::ghost-race-matched-rooms race) 3)
                (equal (mapcar (lambda (split) (getf split :room))
                               (eta-client::ghost-race-splits race))
                       '(12 10)))))
  ;; Split-row clock.
  (check "split clock formats m:ss"
         (equal (format-split-clock 754321) "12:34"))
  (check "split clock zero-pads seconds"
         (equal (format-split-clock 61000) "1:01"))
  ;; High-frequency own-position telemetry (the future ghost's track).
  (let ((telemetry (make-telemetry :start-time 0)))
    (eta-client::update-track-recording
     telemetry '(:floor 1 :x 1.04 :z 2.06 :y 5.02) '(:map 10) 0)
    (eta-client::update-track-recording
     telemetry '(:floor 1 :x 9.0 :z 9.0 :y 9.0) '(:map 10) 100) ; too soon
    (eta-client::update-track-recording
     telemetry '(:floor 1 :x 3.0 :z 4.0 :y 6.0) '(:map 10) 300)
    (let ((track (reverse (eta-client::telemetry-track telemetry))))
      (check "track samples decimate to 250 ms"
             (= (length track) 2))
      (check "track rows carry ms floor map and rounded coords plus height"
             (equal (first track) '(0 1 10 1.0 2.1 5.0))))
    (let* ((json (eta-client::telemetry-json
                  (eta-client:telemetry-run-data telemetry)))
           (track (gethash "track" json)))
      (check "track rides the telemetry json as compact rows"
             (and (= (length track) 2)
                  (equalp (aref track 1) #(300 1 10 3.0 4.0 6.0))))))
  ;; In-world marker projection (camera math ported from the DropBox
  ;; Tracker / PartyMemberTracker addons).
  (let ((camera '(:x 0.0 :y 0.0 :z 0.0
                  :dir-x 0.0 :dir-y 0.0 :dir-z 1.0 :zoom 1)))
    (check "a point straight ahead projects to the screen center"
           (multiple-value-bind (sx sy)
               (eta-client::ghost-screen-position camera 1360 768
                                                  0.0 0.0 100.0)
             (and (eql sx 680) (eql sy 384))))
    (check "a point above the eye line projects above the center"
           (multiple-value-bind (sx sy)
               (eta-client::ghost-screen-position camera 1360 768
                                                  0.0 10.0 100.0)
             (and (eql sx 680) (< sy 384))))
    (check "sideways offsets move sx off center and mirror"
           (multiple-value-bind (sx) (eta-client::ghost-screen-position
                                      camera 1360 768 10.0 0.0 100.0)
             (multiple-value-bind (mx) (eta-client::ghost-screen-position
                                        camera 1360 768 -10.0 0.0 100.0)
               (and sx mx (/= sx 680) (= (- sx 680) (- 680 mx))))))
    (check "a point behind the camera projects to NIL"
           (null (eta-client::ghost-screen-position camera 1360 768
                                                    0.0 0.0 -100.0)))
    (check "the eye point itself projects to NIL"
           (null (eta-client::ghost-screen-position camera 1360 768
                                                    0.0 0.0 0.0)))
    (check "a zeroed direction (loading screen) projects to NIL"
           (null (eta-client::ghost-screen-position
                  '(:x 0.0 :y 0.0 :z 0.0
                    :dir-x 0.0 :dir-y 0.0 :dir-z 0.0 :zoom 1)
                  1360 768 0.0 0.0 100.0)))
    (check "a missing camera projects to NIL"
           (null (eta-client::ghost-screen-position nil 1360 768
                                                    0.0 0.0 100.0))))
  (check "fov heuristic lands near 90 degrees at 16:9"
         (let ((fov (eta-client::camera-fov 1 (/ 1360.0 768.0))))
           (< 1.5 fov 1.65)))
  (check "fov shrinks as the camera zooms in"
         (> (eta-client::camera-fov 0 1.5)
            (eta-client::camera-fov 4 1.5)))
  ;; Panel placement: the :overlay-corner setting picks which corner of
  ;; the client area the overlay panel occupies (the top-right default
  ;; sat on PSO's own minimap once the ghost panel grew tall).
  (flet ((origin (corner &optional (area-w 1360) (area-h 768))
           (multiple-value-list
            (eta-client::overlay-corner-origin corner area-w area-h
                                               260 352 24 16))))
    (check "top-right origin keeps the old geometry"
           (equal (origin :top-right) '(1076 16)))
    (check "top-left origin"
           (equal (origin :top-left) '(24 16)))
    (check "bottom-right origin"
           (equal (origin :bottom-right) '(1076 400)))
    (check "bottom-left origin"
           (equal (origin :bottom-left) '(24 400)))
    (check "middle-right centers vertically without a margin"
           (equal (origin :middle-right) '(1076 208)))
    (check "middle-left origin"
           (equal (origin :middle-left) '(24 208)))
    (check "top-center centers horizontally without a margin"
           (equal (origin :top-center) '(550 16)))
    (check "bottom-center origin"
           (equal (origin :bottom-center) '(550 400)))
    (check "an unknown corner places like top-right"
           (equal (origin :center) '(1076 16)))
    (check "an area smaller than the panel clamps to the edges"
           (equal (origin :bottom-right 200 200) '(0 0))))
  ;; The Ctrl+dragged custom spot: fractions of the area's slack.
  (flet ((panel (corner custom &optional (area-w 1360) (area-h 768))
           (multiple-value-list
            (eta-client::overlay-panel-origin corner custom area-w area-h
                                              260 352 24 16))))
    (check "custom origin scales by the slack"
           (equal (panel :custom '(0.5 1.0)) '(550 416)))
    (check "custom origin clamps out-of-range fractions"
           (equal (panel :custom '(-0.5 1.5)) '(0 416)))
    (check "custom without a usable position places top-right"
           (and (equal (panel :custom nil) '(1076 16))
                (equal (panel :custom '(0.5)) '(1076 16))
                (equal (panel :custom '(0.5 . 0.7)) '(1076 16))
                (equal (panel :custom "junk") '(1076 16))))
    (check "a preset corner ignores the stored custom spot"
           (equal (panel :bottom-left '(0.5 0.5)) '(24 400)))
    (check "custom in an area smaller than the panel pins to 0"
           (equal (panel :custom '(0.7 0.7) 200 200) '(0 0)))))
