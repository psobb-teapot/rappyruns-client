(in-package :ephinea-ta-client-tests)

;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;; Server-defined detection categories (GET /api/quests -> quest-def)
;;; ------------------------------------------------------------------

(defun api-quest (&rest keys-and-values)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on keys-and-values :by #'cddr
          :do (setf (gethash key table) value))
    table))

(defun floor-switch-json (floor switch)
  (api-quest "type" "floor-switch" "floor" floor "switch" switch))

(defun run-server-defs-tests ()
  (format t "~&--- server-defined categories ---~%")
  (load-quest-defs)
  (let ((builtin-count (length ephinea-ta-client::*builtin-quest-defs*)))
    ;; A moderator-created "GDV reset": ep2 quest 944, ends at floor 5 sw 2.
    (let ((quests (vector
                   (api-quest "slug" "ep2-gdv-reset" "episode" 2
                              "game_number" 944
                              "start" (floor-switch-json 5 0)
                              "end" (floor-switch-json 5 2))
                   ;; A display-only entry (no triggers) is ignored.
                   (api-quest "slug" "ep1-some-catalog-quest" "episode" 1))))
      (check "set-server-quest-defs counts only timeable entries"
             (= 1 (set-server-quest-defs quests)))
      (check "server def merged into active defs"
             (find "ep2-gdv-reset" ephinea-ta-client::*quest-defs*
                   :key #'quest-def-slug :test #'equal))
      (check "builtin defs still present after merge"
             (= (1+ builtin-count) (length ephinea-ta-client::*quest-defs*)))
      (let ((def (find "ep2-gdv-reset" ephinea-ta-client::*quest-defs*
                       :key #'quest-def-slug :test #'equal)))
        (check "server def start trigger converted"
               (equal '(:floor-switch 5 0) (quest-def-start def)))
        (check "server def end trigger converted"
               (equal '(:floor-switch 5 2) (quest-def-end def)))
        (check "server def keeps game number" (eql 944 (quest-def-number def)))))
    ;; Re-fetching replaces server defs without duplicating.
    (set-server-quest-defs (vector))
    (check "empty refetch drops server defs, keeps builtin"
           (= builtin-count (length ephinea-ta-client::*quest-defs*)))))

;;; A GDV reset (server-defined) tracked alongside the full GDV clear.
(defun gdv-reader (&key (start 0) (room2 0) (full 0))
  "GDV = Maximum Attack E: Gal Da Val, ep2 quest 944. Start = floor 5
switch 0; room 2 cleared = floor 5 switch 2; full clear = register 254."
  (let ((switches (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
    ;; floor 5 switch 0 and switch 2 live in the floor-5 block.
    (flet ((set-switch (floor switch)
             (let ((offset (+ (* 32 floor) (floor switch 8)))
                   (mask (ash #x80 (- (mod switch 8)))))
               (setf (aref switches offset)
                     (logior (aref switches offset) mask)))))
      (when (plusp start) (set-switch 5 0))
      (when (plusp room2) (set-switch 5 2)))
    (let ((reader (make-game-regions
                   :episode-raw 1  ; raw 1 -> episode 2
                   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 5))
                   :quest-name "Maximum Attack E: Gal Da Val" :quest-number 944
                   :register-values (list (cons 254 full)))))
      ;; Overlay our crafted floor-switch block.
      (push (cons #x00AC9FA0 switches)
            (ephinea-ta-client::mock-reader-regions reader))
      reader)))

(defun run-gdv-segment-test ()
  (format t "~&--- GDV reset alongside full clear ---~%")
  (load-quest-defs)
  (set-server-quest-defs
   (vector (api-quest "slug" "ep2-gdv-reset" "episode" 2 "game_number" 944
                      "start" (floor-switch-json 5 0)
                      "end" (floor-switch-json 5 2))))
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (gdv-reader))                ; loaded, not started
    (step-with detector (gdv-reader :start 1))       ; start switch set
    (check "GDV: both full-clear and reset tracked"
           (= 2 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (gdv-reader :start 1 :room2 1))))
      (check "GDV reset emitted at room 2"
             (equal '("ep2-gdv-reset")
                    (mapcar (lambda (r) (getf r :quest-slug)) runs))))
    (check "GDV full clear still running"
           (= 1 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (gdv-reader :start 1 :room2 1 :full 1))))
      (check "GDV full clear emitted at register 254"
             (equal '("ep2-maximum-attack-e-gal-da-val")
                    (mapcar (lambda (r) (getf r :quest-slug)) runs)))))
  (set-server-quest-defs (vector)))

(defun run-trigger-log-tests ()
  (format t "~&--- trigger log ---~%")
  (ephinea-ta-client::close-trigger-log)
  (let ((path (ephinea-ta-client::trigger-log-path)))
    (ignore-errors (delete-file path))
    ;; Enabling logging must create the file immediately (before any
    ;; trigger changes), so the user can see it is working.
    (start-trigger-log)
    (check "start-trigger-log creates the file at once" (probe-file path))
    ;; A floor switch flipping between two frames is recorded.
    (let* ((clear (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0))
           (set-2 (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
      ;; floor 5 switch 2 on in the second frame.
      (setf (aref set-2 (+ (* 32 5) 0)) (ash #x80 (- 2)))
      (let ((prev (list :quest-name "GDV" :quest-ptr 1 :floor-switches clear
                        :registers (make-array 1024 :element-type '(unsigned-byte 8)
                                                     :initial-element 0)))
            (next (list :quest-name "GDV" :quest-ptr 1 :floor-switches set-2
                        :registers (make-array 1024 :element-type '(unsigned-byte 8)
                                                     :initial-element 0))))
        (check "diff records the flipped switch"
               (= 1 (log-trigger-changes prev next)))))
    (ephinea-ta-client::close-trigger-log)
    (let ((text (with-output-to-string (out)
                  (with-open-file (in path :external-format :utf-8)
                    (loop :for line := (read-line in nil nil)
                          :while line :do (format out "~a~%" line))))))
      (check "log names floor 5 switch 2"
             (search "floor 5 switch 2" text)))
    (ignore-errors (delete-file path))))

;;; ------------------------------------------------------------------
;;; Quest-rule registration: last-kill tracking and trigger JSON
;;; ------------------------------------------------------------------

(defun run-quest-rule-tests ()
  (format t "~&--- quest-rule registration ---~%")
  ;; newly-killed-monsters: only alive->dead transitions count.
  (let ((prev (list :quest-ptr 1
                    :monsters '((:id 7 :hp 200 :name "Booma" :unitxt 44)
                                (:id 8 :hp 50 :name "Rag" :unitxt 45))))
        (next (list :quest-ptr 1
                    :monsters '((:id 7 :hp 0 :name "Booma" :unitxt 44)
                                (:id 8 :hp 50 :name "Rag" :unitxt 45)))))
    (let ((killed (newly-killed-monsters prev next)))
      (check "one enemy transitioned alive->dead" (= 1 (length killed)))
      (check "the killed enemy is id 7" (eql 7 (getf (first killed) :id)))))
  ;; An enemy only ever seen at 0 hp was never alive -> not a kill.
  (check "an enemy only ever at 0 hp is not a kill"
         (null (newly-killed-monsters
                (list :quest-ptr 1 :monsters '((:id 9 :hp 0)))
                (list :quest-ptr 1 :monsters '((:id 9 :hp 0))))))
  ;; update-last-kill records the kill and its name.
  (let ((ephinea-ta-client::*last-kill* nil))
    (update-last-kill
     (list :quest-ptr 1 :monsters '((:id 7 :hp 200 :name "Booma" :unitxt 44)))
     (list :quest-ptr 1 :monsters '((:id 7 :hp 0 :name "Booma" :unitxt 44))))
    (check "last-kill records the id"
           (eql 7 (getf ephinea-ta-client::*last-kill* :id)))
    (check "last-kill records the name"
           (equal "Booma" (getf ephinea-ta-client::*last-kill* :name)))
    ;; Leaving the quest (quest-ptr 0) forgets the kill.
    (update-last-kill (list :quest-ptr 1) (list :quest-ptr 0))
    (check "leaving the quest clears last-kill"
           (null ephinea-ta-client::*last-kill*)))
  ;; A quest reload (changed :quest-ptr) also forgets the old kill.
  (let ((ephinea-ta-client::*last-kill* (list :id 7 :name "Booma")))
    (update-last-kill
     (list :quest-ptr 1 :monsters '((:id 7 :hp 100)))
     (list :quest-ptr 2 :monsters '((:id 7 :hp 0))))
    (check "a quest reload clears last-kill"
           (null ephinea-ta-client::*last-kill*)))
  ;; trigger->json emits the wire shape the server expects.
  (let ((m (trigger->json '(:monster-dead 1234))))
    (check "monster trigger type" (equal "monster" (gethash "type" m)))
    (check "monster trigger id" (eql 1234 (gethash "monster" m))))
  (let ((f (trigger->json '(:floor-switch 5 2))))
    (check "floor-switch type" (equal "floor-switch" (gethash "type" f)))
    (check "floor-switch floor" (eql 5 (gethash "floor" f)))
    (check "floor-switch switch" (eql 2 (gethash "switch" f))))
  (let ((r (trigger->json '(:register 254))))
    (check "register type" (equal "register" (gethash "type" r)))
    (check "register value" (eql 254 (gethash "register" r))))
  (check "warp-in trigger type"
         (equal "warp-in" (gethash "type" (trigger->json '(:warp-in)))))
  (check "nil trigger -> nil" (null (trigger->json nil)))
  ;; The trigger object survives a JSON round trip inside a request body.
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "end" obj) (trigger->json '(:monster-dead 1234)))
    (let ((parsed (com.inuoe.jzon:parse (com.inuoe.jzon:stringify obj))))
      (check "trigger survives json round-trip"
             (eql 1234 (gethash "monster" (gethash "end" parsed)))))))

;;; ------------------------------------------------------------------
;;; Room/enemy rule picker: run-log accumulation and room grouping
;;; ------------------------------------------------------------------

(defun fsw-array (&rest floor-switch-pairs)
  "A 32*18 floor-switch byte block with the given (floor switch) bits on,
using the SNAPSHOT-FLOOR-SWITCH-SET-P bit layout."
  (let ((a (make-array (* 32 18) :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (loop :for (f s) :on floor-switch-pairs :by #'cddr
          :for off := (+ (* 32 f) (floor s 8))
          :do (setf (aref a off) (logior (aref a off)
                                         (ash #x80 (- (mod s 8))))))
    a))

(defun room-snap (ptr floor room monsters switches)
  "A minimal snapshot for run-log tests: a loaded quest at PTR, the local
player on FLOOR/ROOM (map 1 = Forest 1), plus MONSTERS and a floor-switch
block."
  (list :quest-ptr ptr :my-index 0
        :episode 1 :quest-number 9001 :quest-name "Room Test" :map 1
        :players (list (list :index 0 :floor floor :room room))
        :monsters monsters
        :floor-switches switches))

(defun run-room-picker-tests ()
  (format t "~&--- room/enemy rule picker ---~%")
  ;; newly-set-floor-switches: only 0->1 transitions.
  (let ((clear (fsw-array))
        (on (fsw-array 5 2)))
    (let ((flipped (newly-set-floor-switches (list :floor-switches clear)
                                             (list :floor-switches on))))
      (check "floor-switch 0->1 detected"
             (and (= 1 (length flipped))
                  (eql 5 (getf (first flipped) :floor))
                  (eql 2 (getf (first flipped) :switch)))))
    (check "floor-switch 1->0 ignored"
           (null (newly-set-floor-switches (list :floor-switches on)
                                           (list :floor-switches clear)))))
  ;; update-run-logs + run-rooms end to end.
  (setf ephinea-ta-client::*run-kill-log* '()
        ephinea-ta-client::*run-switch-log* '())
  (let* ((clear (fsw-array))
         (lobby (list :quest-ptr 0))
         (a (room-snap 1 1 2 '((:id 7 :hp 100 :name "Booma" :unitxt 44)) clear))
         ;; kill 7 in room 2 and flip floor 1 switch 5 the same frame
         (b (room-snap 1 1 2 '((:id 7 :hp 0 :name "Booma" :unitxt 44))
                       (fsw-array 1 5)))
         (c (room-snap 1 1 3 '((:id 8 :hp 80 :name "Rag" :unitxt 45))
                       (fsw-array 1 5)))
         (d (room-snap 1 1 3 '((:id 8 :hp 0 :name "Rag" :unitxt 45))
                       (fsw-array 1 5))))
    (setf ephinea-ta-client::*run-quest* nil)
    (update-run-logs lobby a)   ; new load -> reset, no kill yet
    (check "run-quest captured on load"
           (and ephinea-ta-client::*run-quest*
                (eql 1 (getf ephinea-ta-client::*run-quest* :episode))))
    (update-run-logs a b)       ; kill 7 (room 2) + switch flip (room 2)
    (update-run-logs b c)       ; walk into room 3
    (update-run-logs c d)       ; kill 8 (room 3)
    (let ((rooms (run-rooms)))
      (check "run-rooms groups two rooms" (= 2 (length rooms)))
      (let ((r2 (find 2 rooms :key (lambda (r) (getf r :room))))
            (r3 (find 3 rooms :key (lambda (r) (getf r :room)))))
        (check "room 2 last kill is enemy 7"
               (and r2 (eql 7 (getf (getf r2 :last-kill) :id))))
        (check "room 2 kill tagged with its floor/room"
               (and r2 (eql 1 (getf (first (getf r2 :kills)) :floor))
                    (eql 2 (getf (first (getf r2 :kills)) :room))))
        (check "room 2 correlated clear switch is floor 1 switch 5"
               (and r2 (getf r2 :switch)
                    (eql 1 (getf (getf r2 :switch) :floor))
                    (eql 5 (getf (getf r2 :switch) :switch))))
        (check "room 3 last kill is enemy 8"
               (and r3 (eql 8 (getf (getf r3 :last-kill) :id))))
        (check "room 3 has no clear switch" (and r3 (null (getf r3 :switch))))))
    ;; Area names and flattened pickable rows.
    (check "client-map-name in range" (string= "Forest 1" (client-map-name 1)))
    (check "client-map-name out of range" (string= "Map 99" (client-map-name 99)))
    (check "room carries its map" (eql 1 (getf (first (run-rooms)) :map)))
    (let ((r2 (find 2 (run-rooms) :key (lambda (r) (getf r :room)))))
      (check "room-area-label uses the map name"
             (string= "Forest 1 · room 2" (room-area-label r2))))
    (let ((rows (run-room-rows)))
      ;; room 2: clear(switch) + enemy 7; room 3: clear(last enemy) + enemy 8
      ;; = 4 rows (every room gets a clear row).
      (check "run-room-rows count" (= 4 (length rows)))
      (let ((clears (remove :clear rows :key (lambda (r) (getf r :kind))
                            :test-not #'eq)))
        (check "every room has a clear row" (= 2 (length clears)))
        ;; room 2 clear uses its door switch...
        (check "room 2 clear is the floor-switch"
               (find '(:floor-switch 1 5) clears
                     :key (lambda (r) (getf r :trigger)) :test #'equal))
        ;; ...room 3 has no switch, so clear falls back to its last enemy (8).
        (check "room 3 clear falls back to the last enemy"
               (find '(:monster-dead 8) clears
                     :key (lambda (r) (getf r :trigger)) :test #'equal)))
      (let ((enemy (find :enemy rows :key (lambda (r) (getf r :kind)))))
        (check "enemy row trigger is monster-dead"
               (eq :monster-dead (first (getf enemy :trigger))))))
    ;; Leaving to the lobby keeps the data for post-run registration.
    (update-run-logs d lobby)
    (check "run logs survive into the lobby" (= 2 (length (run-rooms))))
    ;; A new quest load resets them.
    (update-run-logs lobby (room-snap 2 1 1 '() clear))
    (check "a new quest load resets the run logs" (null (run-rooms)))))

