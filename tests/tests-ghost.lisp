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
