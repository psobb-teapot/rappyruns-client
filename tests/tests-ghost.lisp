;;; Ghost race: split parsing, room matching, formatting and the
;;; completion annotation (client/src/ghost.lisp), plus the room-event
;;; telemetry that feeds the server's ms-precision splits.

(in-package :ephinea-ta-client-tests)

(defparameter +ghost-payload+
  "{\"run_id\":42,\"quest\":\"ep1-towards-the-future\",\"time_ms\":123456,
    \"submitter\":\"teapot\",\"precision\":\"ms\",\"source\":\"pb\",
    \"rooms\":[{\"floor\":1,\"room\":10,\"nth\":1,\"enter_ms\":0},
               {\"floor\":1,\"room\":11,\"nth\":1,\"enter_ms\":30000},
               {\"floor\":1,\"room\":10,\"nth\":2,\"enter_ms\":60000},
               {\"floor\":2,\"room\":1,\"nth\":1,\"enter_ms\":90000}]}")

(defun run-ghost-tests ()
  (let ((ghost (parse-ghost-splits (com.inuoe.jzon:parse +ghost-payload+))))
    (check "ghost payload parses"
           (and ghost
                (= (ghost-time-ms ghost) 123456)
                (equal (ghost-quest-slug ghost) "ep1-towards-the-future")
                (equal (ghost-label ghost) "teapot")
                (equal (ghost-source ghost) "pb")
                (eq (ghost-precision ghost) :ms)
                (= (length (ghost-rooms ghost)) 4)))
    (check "malformed payload parses to NIL"
           (and (null (parse-ghost-splits
                       (com.inuoe.jzon:parse "{\"rooms\":[]}")))
                (null (parse-ghost-splits nil))))
    ;; Live room matching: same (floor, room, nth visit) identity as
    ;; the site's /compare alignment.
    (let ((race (make-ghost-race :ghost ghost)))
      (check "start room matches at once"
             (eql (ghost-race-note-room race 1 10 800) 800))
      (check "staying in the room does not rematch"
             (eql (ghost-race-note-room race 1 10 5000) 800))
      (check "next room updates the gap"
             (eql (ghost-race-note-room race 1 11 34000) 4000))
      (check "a room the ghost never entered keeps the previous gap"
             (eql (ghost-race-note-room race 1 99 40000) 4000))
      (check "a revisit matches the ghost's second visit"
             (eql (ghost-race-note-room race 1 10 55000) -5000))
      (check "matched-room count"
             (= (ghost-race-matched-rooms race) 3)))
    ;; Status suffix.
    (let ((eta-client::*ghost-race* (make-ghost-race :ghost ghost)))
      (check "status suffix shows the target before any match"
             (equal (ghost-status-suffix) " | vs 2:03.456"))
      (setf (ghost-race-delta-ms eta-client::*ghost-race*) 4000)
      (check "status suffix shows the gap after a match"
             (equal (ghost-status-suffix) " | vs 2:03.456 +4.0s")))
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
                                    :time-ms 113456 :aborted t)))))
        (check "beat-the-ghost delta stamped"
               (and (eql (getf (first annotated) :ghost-delta-ms) -10000)
                    (eql (getf (first annotated) :ghost-time-ms) 123456)
                    (equal (getf (first annotated) :ghost-label) "teapot")))
        (check "other quest untouched"
               (null (getf (second annotated) :ghost-delta-ms)))
        (check "aborted run untouched"
               (null (getf (third annotated) :ghost-delta-ms)))))
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
  ;; Fetch gating: one ask per quest load, none without the setting.
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
      (multiple-value-bind (slug difficulty party-size)
          (eta-client::ghost-fetch-wanted snapshot)
        (check "fetch wanted on a fresh quest load"
               (and (equal slug (eta-client:quest-def-slug def))
                    (equal difficulty "Ultimate")
                    (eql party-size 1))))
      (check "same load never asks twice"
             (null (eta-client::ghost-fetch-wanted snapshot))))
    (let ((eta-client::*config* (list* :ghost-race nil :api-token "tok"
                                       (copy-list eta-client::*default-config*)))
          (eta-client::*ghost-fetch-ptr* nil)
          (eta-client::*ghost* nil))
      (check "setting off never asks"
             (null (eta-client::ghost-fetch-wanted snapshot))))))
