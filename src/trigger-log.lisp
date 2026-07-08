(in-package :ephinea-ta-client)

;;; Trigger discovery mode: when enabled, diff quest registers and floor
;;; switches between consecutive frames and append every change to
;;; %APPDATA%/ephinea-ta-client/trigger-log.txt. Used to find the end
;;; trigger for a new category (e.g. "kill everything up to room 2"):
;;; play the segment with logging on and look at which floor switch (or
;;; register) fires the moment the last enemy of the room dies. Monster
;;; kills are logged too (with the enemy's entity id), so a
;;; (:monster-dead ID) / "monster:ID" trigger for one specific enemy can
;;; be found by killing it and reading the id from the log.

(defvar *trigger-log-stream* nil)

;; The GUI thread (toggle -> header) and the poll thread (per-frame diffs)
;; both write here; serialize file access.
(defvar *trigger-log-lock*
  #+lispworks (mp:make-lock :name "eta-client-trigger-log")
  #-lispworks nil)

(defmacro with-trigger-log-lock (&body body)
  #+lispworks `(mp:with-lock (*trigger-log-lock*) ,@body)
  #-lispworks `(progn ,@body))

(defun trigger-log-path ()
  (merge-pathnames "trigger-log.txt" (config-dir)))

(defun trigger-log-stream ()
  (or *trigger-log-stream*
      (setf *trigger-log-stream*
            (progn
              (ensure-directories-exist (trigger-log-path))
              (open (trigger-log-path) :direction :output
                    :if-exists :append :if-does-not-exist :create
                    :external-format :utf-8)))))

(defun close-trigger-log ()
  (with-trigger-log-lock
    (when *trigger-log-stream*
      (ignore-errors (close *trigger-log-stream*))
      (setf *trigger-log-stream* nil))))

(defun start-trigger-log ()
  "Open (creating) the log and write a session header, so the file exists
the moment logging is enabled - before any trigger has changed. Returns
the log path."
  (with-trigger-log-lock
    (let ((stream (trigger-log-stream)))
      (format stream "~&=== trigger logging started ~a ===~%" (time-of-day))
      (format stream "Play the segment; each register / floor-switch change is listed below.~%")
      (finish-output stream)))
  (trigger-log-path))

(defun time-of-day ()
  (multiple-value-bind (second minute hour) (decode-universal-time
                                             (get-universal-time))
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun newly-killed-monsters (previous snapshot)
  "Monsters alive (hp>0) in PREVIOUS and at 0 hp in SNAPSHOT - the
alive->dead transition a \"monster:ID\" end trigger keys off. Returns the
SNAPSHOT monster plists in :monsters order. Pure (no I/O); shared by the
trigger log and last-kill tracking."
  (let ((old (getf previous :monsters))
        (new (getf snapshot :monsters)))
    (loop :for monster :in new
          :for was := (find (getf monster :id) old
                            :key (lambda (m) (getf m :id)))
          :when (and was (plusp (getf was :hp 0))
                     (zerop (getf monster :hp 0)))
            :collect monster)))

(defun newly-set-floor-switches (previous snapshot)
  "Floor switches that flipped 0->1 between PREVIOUS and SNAPSHOT, as a
list of (:floor F :switch S). Clearing a room commonly fires such a
switch, so this feeds the room-clear trigger candidate. Same bit layout
as SNAPSHOT-FLOOR-SWITCH-SET-P. Pure."
  (let ((old (getf previous :floor-switches))
        (new (getf snapshot :floor-switches))
        (result '()))
    (when (and old new)
      (dotimes (i (min (length old) (length new)))
        (let ((old-byte (aref old i))
              (new-byte (aref new i)))
          (unless (= old-byte new-byte)
            (dotimes (bit 8)
              (let ((mask (ash #x80 (- bit))))
                (when (and (zerop (logand old-byte mask))
                           (plusp (logand new-byte mask)))
                  (push (list :floor (floor i 32)
                              :switch (+ (* 8 (mod i 32)) bit))
                        result))))))))
    (nreverse result)))

(defvar *last-kill* nil
  "The most recently killed enemy this quest load, as a plist
(:id :name :unitxt), or NIL. Set by the poll loop from the alive->dead
frame diff and read by the GUI's quest-rule registration to prefill a
\"monster:ID\" end trigger. Cleared when the quest reloads or the player
leaves it, so a stale id never prefills a different quest's rule. Defined
here (loads before gui.lisp) so the GUI's reference is a known special.")

(defun update-last-kill (previous snapshot)
  "Refresh *LAST-KILL* from the PREVIOUS->SNAPSHOT frame diff. Forgets the
old kill when no quest is loaded, or when the quest pointer changed (a
reload or lobby return - that frame's diff spans two loads and is
meaningless). Pure except for the *LAST-KILL* write; safe to call every
frame regardless of the trigger-log toggle."
  (cond
    ((not (and snapshot (getf snapshot :quest-ptr)
               (plusp (getf snapshot :quest-ptr))))
     (setf *last-kill* nil))
    ((and previous (getf previous :quest-ptr)
          (not (eql (getf previous :quest-ptr) (getf snapshot :quest-ptr))))
     (setf *last-kill* nil))
    (t
     (let ((killed (car (last (newly-killed-monsters previous snapshot)))))
       (when killed
         (setf *last-kill*
               (list :id (getf killed :id)
                     :name (getf killed :name)
                     :unitxt (getf killed :unitxt))))))))

(defvar *run-kill-log* '()
  "Kills observed during the current or most-recent quest load, NEWEST
FIRST, each a plist (:id :name :unitxt :floor :room :map :time). :floor/
:room are the local player's at kill time (a monster carries no room of
its own); :map is the snapshot's real loaded map number (for area names);
:time is GET-INTERNAL-REAL-TIME, used only for ordering. Read by the GUI
room/enemy picker to build a rule. Unlike *LAST-KILL* this is kept through
the run AND into the lobby - it is only reset when a new quest loads - so
a rule can be registered after finishing or aborting.")

(defvar *run-switch-log* '()
  "Floor switches that fired during the current or most-recent quest load,
NEWEST FIRST, each (:floor F :switch S :room R :time T) where :room is the
local player's room when the switch flipped. Feeds the room-clear
(floor-switch) trigger candidate. Same lifecycle as *RUN-KILL-LOG*.")

(defvar *run-quest* nil
  "Identity of the quest that produced the current run logs, as a plist
(:number :name :episode), or NIL. Set when a quest loads and kept into the
lobby (same lifecycle as the run logs) so the rule-registration form can
auto-select the quest just played.")

(defun reset-run-logs ()
  (setf *run-kill-log* '() *run-switch-log* '()))

(defun update-run-logs (previous snapshot)
  "Accumulate this frame's kills and floor-switch flips into the run logs,
tagging each with the local player's floor/room. Resets the logs when a
new quest loads (a different non-zero quest-ptr); leaves them intact when
no quest is loaded, so the picker still has the last run's data back in
the lobby. Safe to call every frame."
  (let ((ptr (and snapshot (getf snapshot :quest-ptr))))
    (when (and ptr (plusp ptr))
      ;; A fresh load (from the lobby or a different quest): start over
      ;; and remember which quest this run is, for the registration form.
      (when (or (null previous)
                (not (eql (getf previous :quest-ptr) ptr)))
        (reset-run-logs)
        (setf *run-quest* (list :number (getf snapshot :quest-number)
                                :name (getf snapshot :quest-name)
                                :episode (getf snapshot :episode))))
      (let* ((me (snapshot-my-player snapshot))
             (floor (and me (getf me :floor)))
             (room (and me (getf me :room)))
             (map (getf snapshot :map))
             (time (get-internal-real-time)))
        (dolist (monster (newly-killed-monsters previous snapshot))
          (push (list :id (getf monster :id)
                      :name (getf monster :name)
                      :unitxt (getf monster :unitxt)
                      :floor floor :room room :map map :time time)
                *run-kill-log*))
        (dolist (sw (newly-set-floor-switches previous snapshot))
          (push (list :floor (getf sw :floor)
                      :switch (getf sw :switch)
                      :room room :time time)
                *run-switch-log*))))))

(defun room-clear-switch (room last-kill)
  "The floor switch from *RUN-SWITCH-LOG* that best marks ROOM's clear:
same player room as ROOM, nearest in time to LAST-KILL. Correlation is on
room (both logs record the player's room), while the returned switch keeps
its own :floor for the trigger. NIL when no switch fired in the room."
  (let ((rm (getf room :room))
        (target (and last-kill (getf last-kill :time)))
        (best nil) (best-dist nil))
    (dolist (sw *run-switch-log* best)
      (when (eql (getf sw :room) rm)
        (let ((dist (if target (abs (- (getf sw :time) target)) 0)))
          (when (or (null best) (< dist best-dist))
            (setf best sw best-dist dist)))))))

(defun run-rooms ()
  "Group *RUN-KILL-LOG* into rooms for the rule picker. Returns a list of
room plists in the order rooms were first entered:
  (:floor F :room R :map M :kills (kill... oldest first)
   :last-kill kill :switch (:floor :switch ...)|nil)
:last-kill is the room's final kill; :map is the area map number (for the
label); :switch is the room-clear candidate from ROOM-CLEAR-SWITCH. Pure;
reads the run-log globals."
  (let ((kills (reverse *run-kill-log*))   ; oldest first
        (rooms '()))                        ; (key . plist), reverse first-seen
    (dolist (kill kills)
      (let* ((key (cons (getf kill :floor) (getf kill :room)))
             (cell (assoc key rooms :test #'equal)))
        (if cell
            (setf (getf (cdr cell) :kills)
                  (append (getf (cdr cell) :kills) (list kill)))
            (push (cons key (list :floor (getf kill :floor)
                                  :room (getf kill :room)
                                  :map (getf kill :map)
                                  :kills (list kill)))
                  rooms))))
    (loop :for (nil . room) :in (nreverse rooms)
          :for last-kill := (car (last (getf room :kills)))
          :collect (list* :last-kill last-kill
                          :switch (room-clear-switch room last-kill)
                          room))))

;;; Area names: a room's :map number -> a human area label, mirroring the
;;; site (src/views.lisp +map-names+ / room-area-text). The client reads
;;; the real loaded map (+current-map-address+, snapshot :map), which
;;; indexes this table directly, so no episode/floor-slot fallback is
;;; needed. English only (area names are proper nouns used as-is in JA).

(defparameter +map-names+
  #("Pioneer II" "Forest 1" "Forest 2" "Cave 1" "Cave 2" "Cave 3"
    "Mine 1" "Mine 2" "Ruins 1" "Ruins 2" "Ruins 3"
    "Under the Dome" "Underground Channel" "Control Room" "????"
    "Lobby" "BA Spaceship" "BA Temple" "Lab"
    "Temple Alpha" "Temple Beta 2" "Spaceship Alpha" "Spaceship Beta"
    "CCA" "Jungle North" "Jungle East" "Mountain" "Seaside"
    "Seabed Upper" "Seabed Lower" "Cliffs of Gal Da Val"
    "Test Subject Disposal Area" "Temple Final" "Spaceship Final"
    "Seaside at Night" "Control Tower"
    "Crater East" "Crater West" "Crater South" "Crater North"
    "Crater Interior" "Desert 1" "Desert 2" "Desert 3"
    "Meteor Impact Site" "Pioneer II"))

(defun client-map-name (number)
  "Area name for a map NUMBER, or \"Map N\" when out of range/unknown."
  (if (and (integerp number) (< -1 number (length +map-names+)))
      (aref +map-names+ number)
      (format nil "Map ~a" number)))

(defun room-area-label (room)
  "\"<area> · room <n>\" for a ROOM plist (from RUN-ROOMS), or
\"Room <n>\" when the map is unknown. Matches the site's room label."
  (let ((map (getf room :map))
        (rm (getf room :room)))
    (if (integerp map)
        (format nil "~a · room ~a" (client-map-name map) rm)
        (format nil "Room ~a" rm))))

(defun run-room-rows ()
  "Flatten RUN-ROOMS into pickable rows for the live Rooms list. Each row
is a plist (:area LABEL :kind :clear|:enemy :name NAME|nil :trigger LIST).
Per room:
  - a :clear row (\"clear this room\"): the door floor-switch when one
    fired in the room, otherwise the room's LAST enemy dying - i.e. all
    enemies dead. Most rooms fire no switch (the room boundary comes from
    the player's room field, not a switch), so the last-enemy fallback is
    what makes \"clear the room\" selectable everywhere;
  - then one :enemy row per distinct enemy (kill order) for a specific kill.
Pure; the GUI renders and, on click, uses :trigger as the rule's end
condition."
  (loop :for room :in (run-rooms)
        :for area := (room-area-label room)
        :nconc (let* ((sw (getf room :switch))
                      (last (getf room :last-kill))
                      (enemies (remove-duplicates (getf room :kills)
                                                  :key (lambda (k) (getf k :id))
                                                  :from-end t)))
                 (append
                  (cond
                    (sw (list (list :area area :kind :clear :name nil
                                    :trigger (list :floor-switch
                                                   (getf sw :floor)
                                                   (getf sw :switch)))))
                    (last (list (list :area area :kind :clear :name nil
                                      :trigger (list :monster-dead
                                                     (getf last :id))))))
                  (loop :for k :in enemies
                        :collect (list :area area :kind :enemy
                                       :name (getf k :name)
                                       :trigger (list :monster-dead
                                                      (getf k :id))))))))

(defun log-trigger-changes (previous snapshot)
  "Append register / floor-switch diffs between two consecutive
snapshots of the same loaded quest."
  (when (and previous snapshot
             (getf previous :quest-name)
             (getf snapshot :quest-name)
             (eql (getf previous :quest-ptr) (getf snapshot :quest-ptr)))
    (with-trigger-log-lock
    (let ((stream (trigger-log-stream))
          (stamp (time-of-day))
          (quest (getf snapshot :quest-name))
          (changes 0))
      (let ((old (getf previous :registers))
            (new (getf snapshot :registers)))
        (when (and old new)
          (dotimes (id +register-count+)
            (let ((old-value (bytes-u16 old (* 4 id)))
                  (new-value (bytes-u16 new (* 4 id))))
              (unless (= old-value new-value)
                (incf changes)
                (format stream "~a ~s register ~d: ~d -> ~d~%"
                        stamp quest id old-value new-value))))))
      (let ((old (getf previous :floor-switches))
            (new (getf snapshot :floor-switches)))
        (when (and old new)
          (dotimes (i (min (length old) (length new)))
            (let ((old-byte (aref old i))
                  (new-byte (aref new i)))
              (unless (= old-byte new-byte)
                (dotimes (bit 8)
                  (let ((mask (ash #x80 (- bit))))
                    (unless (= (logand old-byte mask) (logand new-byte mask))
                      (incf changes)
                      (format stream "~a ~s floor ~d switch ~d: ~:[off~;on~] -> ~:[off~;on~]~%"
                              stamp quest
                              (floor i 32)
                              (+ (* 8 (mod i 32)) bit)
                              (plusp (logand old-byte mask))
                              (plusp (logand new-byte mask)))))))))))
      ;; Monster kills: an enemy seen alive last frame now at 0 hp. The
      ;; id printed here is what a "monster:ID" end trigger matches.
      (dolist (monster (newly-killed-monsters previous snapshot))
        (incf changes)
        (format stream "~a ~s monster ~d killed (~a, unitxt ~d)~%"
                stamp quest (getf monster :id)
                (or (getf monster :name) "?")
                (getf monster :unitxt 0)))
      (when (plusp changes)
        (force-output stream))
      changes))))
