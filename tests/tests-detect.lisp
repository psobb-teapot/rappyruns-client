(in-package :ephinea-ta-client-tests)

;;; ------------------------------------------------------------------
;;; Run JSON payload
;;; ------------------------------------------------------------------

(defun run-payload-tests ()
  (format t "~&--- run payload ---~%")
  (let* ((run (list :quest-slug "ep1-towards-the-future"
                    :quest-name "Towards the Future"
                    :episode 1 :time-ms 754321 :party-size 1 :pb t
                    :difficulty "Ultimate" :death-count 2
                    :submitter-section-id "Skyly"
                    :players (list (list :name "Ryu" :class "HUcast"
                                         :level 142 :section-id "Skyly"
                                         :guild-card "42001234"))
                    :telemetry (list :frames '((0 945 300 0 0 1 2 10.0 -3.1
                                                0 0 0 1 12 0))
                                     :events '((:t 12 :type "death"))
                                     :death-count 2 :meseta-charged 400
                                     :kills 55 :tp-used 120
                                     :traps-used '(:dt 0 :ft 2 :ct 0)
                                     :items-used '((:monomate . 1))
                                     :techs-cast '(("Resta" . 3))
                                     :time-by-state '((1 . 60000))
                                     :weapons (list
                                               (list :id "00010000"
                                                     :display "Charge Vulcan +9"
                                                     :type :weapon :seconds 700
                                                     :attacks 512 :techs 0)))))
         (parsed (com.inuoe.jzon:parse (ephinea-ta-client::run-json run))))
    (check "payload difficulty" (equal "Ultimate" (gethash "difficulty" parsed)))
    (check "payload death count" (eql 2 (gethash "death_count" parsed)))
    (check "payload episode" (eql 1 (gethash "episode" parsed)))
    (check "payload submitter section"
           (equal "Skyly" (gethash "submitter_section_id" parsed)))
    (let ((player (aref (gethash "players" parsed) 0)))
      (check "payload player level" (eql 142 (gethash "level" player)))
      (check "payload player section" (equal "Skyly" (gethash "section_id" player)))
      (check "payload player guild card"
             (equal "42001234" (gethash "guild_card" player))))
    (let ((telemetry (gethash "telemetry" parsed)))
      (check "payload telemetry present" (hash-table-p telemetry))
      (check "payload frame keys"
             (equalp (coerce ephinea-ta-client::+frame-keys+ 'list)
                     (coerce (gethash "frame_keys" telemetry) 'list)))
      (check "payload one frame"
             (= 1 (length (gethash "frames" telemetry))))
      (check "payload items snake_cased"
             (eql 1 (gethash "monomate" (gethash "items_used" telemetry))))
      (check "payload traps skip zeroes"
             (and (eql 2 (gethash "ft" (gethash "traps_used" telemetry)))
                  (null (gethash "dt" (gethash "traps_used" telemetry)))))
      (check "payload weapon display"
             (equal "Charge Vulcan +9"
                    (gethash "display" (aref (gethash "weapons" telemetry) 0))))
      (check "payload event"
             (equal "death" (gethash "type"
                                     (aref (gethash "events" telemetry) 0)))))
    (check "payload omits unranked and private for a board run"
           (and (null (gethash "unranked" parsed))
                (null (gethash "private" parsed)))))
  ;; Tracking-only mode's flags (APPLY-TRACKING-MODE) ride the payload.
  (let ((parsed (com.inuoe.jzon:parse
                 (ephinea-ta-client::run-json
                  (list :quest-slug "ep1-test" :time-ms 60000 :party-size 1
                        :players '() :unranked t :run-private t)))))
    (check "payload unranked rides when stamped"
           (eq t (gethash "unranked" parsed)))
    (check "payload private rides with the tracking sub-setting"
           (eq t (gethash "private" parsed)))
    (check "payload notes mention record only"
           (search "record only" (gethash "notes" parsed)))))

;;; ------------------------------------------------------------------
;;; Detector integration: telemetry rides along with completed runs
;;; ------------------------------------------------------------------

(defun run-detect-telemetry-tests ()
  (format t "~&--- detect + telemetry ---~%")
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader))
    (step-with detector (ttf-reader :start 1))
    (sleep 0.05)
    (step-with detector (ttf-reader :start 1))
    (sleep 0.05)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "run has difficulty" (equal "Normal" (getf run :difficulty)))
      (check "run has death count" (eql 0 (getf run :death-count)))
      (check "run has telemetry" (listp (getf run :telemetry)))
      (check "telemetry recorded a frame"
             (plusp (length (getf (getf run :telemetry) :frames))))
      (check "player carries level"
             (= 1 (getf (first (getf run :players)) :level))))))

;;; ------------------------------------------------------------------
;;; Detection state machine (driven through mock memory images)
;;; ------------------------------------------------------------------

(defun ttf-reader (&key (start 0) (end 0) (seg 0) (pb 0.0) warping
                        (difficulty 0) hp-scale)
  (make-game-regions
   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1 :pb pb
                                     :warping warping)
                  (make-player-block :name "Elly" :class-id 8 :floor 1))
   :quest-name "Towards the Future" :quest-number 118
   :difficulty difficulty :hp-scale hp-scale
   :register-values (list (cons 12 start) (cons 254 end) (cons 100 seg))))

(defun lobby-reader ()
  (make-game-regions
   :players (list (make-player-block :name "Ryu" :class-id 2 :floor 0))))

(defun step-with (detector reader)
  (detector-step detector (read-snapshot reader)))

(defun run-detect-tests ()
  (format t "~&--- detect ---~%")
  ;; Full TTF flow: lobby -> quest loaded -> start -> finish.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (check "idle after lobby" (eq :idle (detector-state detector)))
    (step-with detector (ttf-reader))
    (check "still idle before start register" (eq :idle (detector-state detector)))
    (step-with detector (ttf-reader :start 1))
    (check "in-quest after start register" (eq :in-quest (detector-state detector)))
    (sleep 0.05)
    (check "no run before end register"
           (null (step-with detector (ttf-reader :start 1))))
    (sleep 0.05)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "run emitted on end register" (not (null run)))
      (check "run slug" (equal "ep1-towards-the-future" (getf run :quest-slug)))
      (check "run time >= 100ms" (>= (getf run :time-ms) 100))
      (check "run party" (= 2 (getf run :party-size)))
      (check "run players"
             (equal '(("Ryu" . "HUcast") ("Elly" . "FOnewearl"))
                    (mapcar (lambda (p) (cons (getf p :name) (getf p :class)))
                            (getf run :players))))
      (check "run not PB" (not (getf run :pb)))
      (check "detector reset after run" (eq :idle (detector-state detector))))
    ;; Triggers stay set after completion while the quest is loaded; the
    ;; detector must not re-start (and re-submit) the same run.
    (step-with detector (ttf-reader :start 1 :end 1))
    (check "no restart while completed quest stays loaded"
           (eq :idle (detector-state detector)))
    ;; Back through the lobby re-arms; a fresh take of the quest starts.
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "re-armed after lobby visit"
           (eq :in-quest (detector-state detector))))
  ;; Attaching mid-quest must not start a run (not armed).
  (let ((detector (make-detector)))
    (step-with detector (ttf-reader :start 1))
    (check "mid-quest attach stays idle" (eq :idle (detector-state detector))))
  ;; A charged gauge / cast Shifta at the start is NOT enough for PB - a
  ;; normal No-PB run often starts that way. It must be No PB.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1 :pb 80.0))
    (sleep 0.02)
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1 :pb 80.0)))))
      (check "charged PB at start alone stays No PB" (null (getf run :pb)))))
  ;; Actually discharging a Photon Blast mid-run (a full gauge
  ;; collapsing) is PB.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1 :pb 100.0))
    (step-with detector (ttf-reader :start 1 :pb 2.0))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "PB discharge -> PB category" (eq t (getf run :pb)))))
  ;; A Pioneer 2 telepipe trip zeroes the gauge in-game. A partial
  ;; charge collapsing is therefore a warp, never a Blast (a Blast
  ;; needs 100) - the run 1717 miscategorization.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1 :pb 60.0))
    (step-with detector (ttf-reader :start 1 :pb 0.0))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "partial gauge zeroed (telepipe) stays No PB"
             (null (getf run :pb)))))
  ;; Even a full gauge zeroed across a warp is the warp's reset, not a
  ;; discharge - the warp frames drop the baseline instead of comparing.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1 :pb 100.0))
    (step-with detector (ttf-reader :start 1 :pb 100.0 :warping t))
    (step-with detector (ttf-reader :start 1 :pb 0.0))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "full gauge zeroed across a warp stays No PB"
             (null (getf run :pb)))))
  ;; A quest run that never discharges is No PB, even with Shifta up
  ;; (the segment tests below and this one cover the common case).
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (step-with detector (ttf-reader :start 1))
    (let ((run (first (step-with detector (ttf-reader :start 1 :end 1)))))
      (check "no discharge -> No PB" (null (getf run :pb)))))
  ;; A segment definition sharing the quest is tracked in parallel with
  ;; the full clear: one run through the quest yields both records.
  (let* ((segment (ephinea-ta-client::make-quest-def
                   :slug "ep1-towards-the-future-2-rooms" :episode 1
                   :names '("Towards the Future") :number 118
                   :start '(:register 12) :end '(:register 100)))
         (ephinea-ta-client::*quest-defs*
           (cons segment ephinea-ta-client::*quest-defs*))
         (detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "segment: both trackers active"
           (= 2 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (ttf-reader :start 1 :seg 1))))
      (check "segment run emitted first"
             (equal '("ep1-towards-the-future-2-rooms")
                    (mapcar (lambda (run) (getf run :quest-slug)) runs))))
    (check "full clear still running"
           (= 1 (ephinea-ta-client:detector-active-count detector)))
    (sleep 0.05)
    (let ((runs (step-with detector (ttf-reader :start 1 :seg 1 :end 1))))
      (check "full clear emitted second"
             (equal '("ep1-towards-the-future")
                    (mapcar (lambda (run) (getf run :quest-slug)) runs)))
      (check "full clear time > segment possible"
             (>= (getf (first runs) :time-ms) 100)))
    (check "segment does not restart while quest loaded"
           (null (step-with detector (ttf-reader :start 1 :seg 1 :end 1)))))
  ;; Warp-in quests start when a player leaves Pioneer 2.
  (let ((detector (make-detector))
        (en1-p2 (make-game-regions
                 :players (list (make-player-block :name "Ryu" :class-id 2 :floor 0))
                 :quest-name "Endless Nightmare #1" :quest-number 108))
        (en1-in (make-game-regions
                 :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1))
                 :quest-name "Endless Nightmare #1" :quest-number 108
                 :register-values '((30 . 1)))))
    (step-with detector (lobby-reader))
    (step-with detector en1-p2)
    (check "warp-in: idle on Pioneer 2" (eq :idle (detector-state detector)))
    ;; Same frame reaches the end register here; start frame first:
    (step-with detector en1-in)
    (check "warp-in: started once on the field"
           (eq :in-quest (detector-state detector)))
    (let ((run (first (step-with detector en1-in))))
      (check "warp-in quest completes" (equal "ep1-endless-nightmare-1"
                                              (getf run :quest-slug)))))
  ;; Unknown quests never start.
  (let ((detector (make-detector))
        (unknown (make-game-regions
                  :players (list (make-player-block :name "Ryu" :class-id 2 :floor 1))
                  :quest-name "Gallon's Shop" :quest-number 9999
                  :register-values '((12 . 1)))))
    (step-with detector (lobby-reader))
    (step-with detector unknown)
    (check "unknown quest stays idle" (eq :idle (detector-state detector))))
  ;; Losing the game or reloading the quest voids the run.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (detector-step detector nil)
    (check "game gone -> idle" (eq :idle (detector-state detector))))
  ;; Abandoning a quest mid-run: attempts shorter than +abort-min-ms+
  ;; are noise and emit nothing.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :start 1))
    (check "abort: quick lobby return emits nothing"
           (null (step-with detector (lobby-reader)))))
  ;; Past the threshold, returning to the lobby emits an aborted run
  ;; built from state captured at the start (no quest snapshot left).
  (flet ((age-trackers (detector seconds)
           (dolist (tracker (ephinea-ta-client::detector-trackers detector))
             (decf (ephinea-ta-client::tracker-start-time tracker)
                   (* seconds internal-time-units-per-second)))))
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (let ((run (first (step-with detector (lobby-reader)))))
        (check "abort: lobby return emits aborted run" (not (null run)))
        (check "abort: marked aborted" (eq t (getf run :aborted)))
        (check "abort: slug" (equal "ep1-towards-the-future"
                                    (getf run :quest-slug)))
        (check "abort: time >= 20s" (>= (getf run :time-ms) 20000))
        (check "abort: quest name captured at start"
               (equal "Towards the Future" (getf run :quest-name)))
        (check "abort: party captured" (= 2 (getf run :party-size)))
        (check "abort: telemetry attached" (not (null (getf run :telemetry))))
        (check "abort: detector reset" (eq :idle (detector-state detector)))))
    ;; Game exit mid-run aborts the same way.
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (let ((run (first (detector-step detector nil))))
        (check "abort: game exit emits aborted run"
               (eq t (getf run :aborted)))))
    ;; A completed run must never be re-emitted as aborted afterwards.
    (let ((detector (make-detector)))
      (step-with detector (lobby-reader))
      (step-with detector (ttf-reader :start 1))
      (age-trackers detector 20)
      (step-with detector (ttf-reader :start 1 :end 1))
      (check "abort: nothing re-emitted after completion"
             (null (step-with detector (lobby-reader)))))))

;;; ------------------------------------------------------------------
;;; (:monster-dead ID) end trigger: a specific enemy's kill clears the run
;;; ------------------------------------------------------------------

(defun mon-snapshot (monsters)
  "Minimal in-quest snapshot (built directly, not via mock memory) for the
monster-kill detector tests: a warp-in start and a :monsters list."
  (list :episode 1 :my-index 0
        :quest-ptr 1
        :quest-name "Monster Test" :quest-number 9001
        :difficulty 0
        :players (list (list :index 0 :name "Ryu" :class "HUcast" :floor 1))
        :monsters monsters))

(defun mon-lobby ()
  (list :players (list (list :index 0 :name "Ryu" :class "HUcast" :floor 0))
        :quest-ptr 0))

(defun run-anguish-tests ()
  (format t "~&--- anguish ---~%")
  ;; f64 decoding round-trips through the mock image encoder.
  (let ((bytes (make-array 8 :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (put-f64 bytes 0 1.82d0)
    (check "read-f64 decodes 1.82"
           (< (abs (- 1.82d0 (read-f64 (make-mock-reader (cons 100 bytes)) 100)))
              1d-9)))
  (check "scale 1.0 -> no anguish" (null (anguish-level 1.0d0)))
  (check "scale NIL -> no anguish" (null (anguish-level nil)))
  (check "scale 1.30 -> Anguish 1" (eql 1 (anguish-level 1.30d0)))
  (check "scale 1.82 -> Anguish 2" (eql 2 (anguish-level 1.82d0)))
  (check "scale 2.50 -> Anguish 3" (eql 3 (anguish-level 2.50d0)))
  (check "odd scale matches nearest level" (eql 2 (anguish-level 2.0d0)))
  (check "label Ultimate + level 2" (equal "Anguish 2" (difficulty-label 3 2)))
  (check "label Ultimate alone" (equal "Ultimate" (difficulty-label 3 nil)))
  (check "label non-Ultimate ignores anguish"
         (equal "Normal" (difficulty-label 0 2)))
  ;; The snapshot carries the level while a quest is loaded.
  (let ((snapshot (read-snapshot (ttf-reader :difficulty 3 :hp-scale 2.50d0))))
    (check "snapshot anguish level" (eql 3 (getf snapshot :anguish))))
  (let ((snapshot (read-snapshot (ttf-reader :difficulty 3))))
    (check "snapshot without hp table -> no anguish"
           (null (getf snapshot :anguish))))
  (let ((snapshot (read-snapshot (ttf-reader :difficulty 3 :hp-scale 1.0d0))))
    (check "snapshot at scale 1.0 -> no anguish"
           (null (getf snapshot :anguish))))
  ;; Full detector flow: the emitted run lands in the Anguish category.
  (let ((detector (make-detector)))
    (step-with detector (lobby-reader))
    (step-with detector (ttf-reader :difficulty 3 :hp-scale 1.30d0 :start 1))
    (sleep 0.02)
    (let ((run (first (step-with detector (ttf-reader :difficulty 3
                                                      :hp-scale 1.30d0
                                                      :start 1 :end 1)))))
      (check "anguish run difficulty label"
             (equal "Anguish 1" (getf run :difficulty))))))

(defun run-monster-clear-tests ()
  (format t "~&--- monster-kill clear trigger ---~%")
  (let* ((def (ephinea-ta-client::make-quest-def
               :slug "monster-test-kill-boss" :episode 1
               :names '("Monster Test") :number 9001
               :start '(:warp-in) :end '(:monster-dead 7)))
         (ephinea-ta-client::*quest-defs* (list def)))
    ;; Killing the target enemy (id 7) clears the run; killing another
    ;; enemy (id 8) first does not.
    (let ((detector (make-detector)))
      (detector-step detector (mon-lobby))            ; arm
      (detector-step detector (mon-snapshot '((:id 7 :hp 200) (:id 8 :hp 50))))
      (check "in-quest after warp-in start" (eq :in-quest (detector-state detector)))
      (sleep 0.02)
      (check "wrong enemy dying does not clear"
             (null (detector-step
                    detector (mon-snapshot '((:id 7 :hp 120) (:id 8 :hp 0))))))
      (sleep 0.02)
      (let ((run (first (detector-step
                         detector (mon-snapshot '((:id 7 :hp 0) (:id 8 :hp 0)))))))
        (check "target enemy dying emits the run" (not (null run)))
        (check "monster-clear run slug"
               (equal "monster-test-kill-boss" (getf run :quest-slug)))
        (check "detector idle after monster-kill clear"
               (eq :idle (detector-state detector)))))
    ;; An enemy only ever seen at 0 hp was never alive, so it must not
    ;; fire a clear (mirrors telemetry's alive->dead rule).
    (let ((detector (make-detector)))
      (detector-step detector (mon-lobby))
      (detector-step detector (mon-snapshot '((:id 7 :hp 0) (:id 9 :hp 80))))
      (sleep 0.02)
      (check "target seen only at 0 hp never clears"
             (null (detector-step
                    detector (mon-snapshot '((:id 7 :hp 0) (:id 9 :hp 80)))))))))

