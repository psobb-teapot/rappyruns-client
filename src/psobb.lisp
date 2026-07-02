(in-package :ephinea-ta-client)

;;; PSOBB memory layout, transcribed from psostats-client (MIT,
;;; https://github.com/phelix-/psostats-client) client/internal/pso/.
;;; All addresses are for the 32-bit Ephinea PSOBB client and are
;;; deliberately collected in this one file.

;; Global addresses
(defconstant +base-player-array+ #x00A94254)  ; 12 pointers, 4 bytes each
(defconstant +my-player-index+ #x00A9C4F4)    ; u8
(defconstant +episode-address+ #x00A9B1C8)    ; u16, 0-based; 2 means ep4
(defconstant +quest-pointer+ #x00A95AA8)      ; u32 -> quest struct
(defconstant +floor-switches+ #x00AC9FA0)     ; 32 bytes per floor

;; Offsets within the quest struct / quest data block
(defconstant +quest-data-offset+ #x19C)       ; u32 -> quest data block
(defconstant +quest-register-offset+ #x2C)    ; u32 -> u16 registers, stride 4
(defconstant +quest-number-offset+ #x10)      ; u16 within data block
(defconstant +quest-name-offset+ #x18)        ; UTF-16, 32 chars max

;; Offsets within a player struct
(defconstant +player-name-offset+ #x428)      ; UTF-16, 12 chars
(defconstant +player-class-offset+ #x960)     ; u16, class in bits 8-11
(defconstant +player-floor-offset+ #x3F0)     ; u16
(defconstant +player-state-offset+ #x33E)     ; u16 bitfield, #x04 = warping
(defconstant +player-pb-offset+ #x520)        ; f32, photon blast charge
(defconstant +player-shifta-offset+ #x278)    ; f32 multiplier, 0 = none
(defconstant +player-hp-offset+ #x334)        ; u16
(defconstant +player-max-hp-offset+ #x2BC)    ; u16

(defconstant +floor-count+ 18)                ; floors 0-17 (tower)
(defconstant +register-count+ 256)

(defparameter +class-ids+
  '((#x00 . "HUmar") (#x01 . "HUnewearl") (#x02 . "HUcast") (#x09 . "HUcaseal")
    (#x03 . "RAmar") (#x0B . "RAmarl") (#x04 . "RAcast") (#x05 . "RAcaseal")
    (#x0A . "FOmar") (#x06 . "FOmarl") (#x07 . "FOnewm") (#x08 . "FOnewearl")))

(defun class-name-for-id (id)
  (cdr (assoc id +class-ids+)))

(defun strip-name-prefix (name)
  ;; Character names are prefixed with a "\tE" language marker.
  (if (and (>= (length name) 2)
           (char= (char name 0) #\Tab))
      (subseq name 2)
      name))

(defun read-player (reader address)
  (let ((name (read-utf16-string reader (+ address +player-name-offset+) 24))
        (class-bits (read-u16 reader (+ address +player-class-offset+)))
        (floor (read-u16 reader (+ address +player-floor-offset+)))
        (state (read-u16 reader (+ address +player-state-offset+)))
        (pb (read-f32 reader (+ address +player-pb-offset+)))
        (shifta (read-f32 reader (+ address +player-shifta-offset+))))
    (when (and name class-bits)
      (list :name (strip-name-prefix name)
            :class (class-name-for-id (ash (logand class-bits #xF00) -8))
            :floor (or floor 0)
            :warping (and state (plusp (logand state #x04)))
            :pb (or pb 0.0)
            :shifta (or shifta 0.0)))))

(defun read-players (reader)
  "Players in the current game with their party slot order preserved."
  (loop :for i :from 0 :below 12
        :for pointer := (read-u32 reader (+ +base-player-array+ (* 4 i)))
        :when (and pointer (plusp pointer))
          :collect (let ((player (read-player reader pointer)))
                     (when player (list* :index i player)))
            :into players
        :finally (return (remove nil players))))

(defun read-episode (reader)
  (let ((raw (read-u16 reader +episode-address+)))
    (case (and raw (1+ raw))
      (3 4)
      ((1 2 4) (1+ raw))
      (t nil))))

(defun read-snapshot (reader)
  "One frame of game state as a plist; NIL if the process is unreadable.
Trigger evaluation happens on the snapshot (see SNAPSHOT-REGISTER-SET-P),
so the detection state machine stays pure and testable."
  (let ((my-index (read-u8 reader +my-player-index+)))
    (when my-index
      (let* ((episode (read-episode reader))
             (players (read-players reader))
             (quest-ptr (read-u32 reader +quest-pointer+))
             (snapshot (list :episode episode
                             :my-index my-index
                             :players players
                             :quest-ptr (or quest-ptr 0))))
        (when (and quest-ptr (plusp quest-ptr))
          (let* ((data-ptr (read-u32 reader (+ quest-ptr +quest-data-offset+)))
                 (register-ptr (read-u32 reader (+ quest-ptr +quest-register-offset+)))
                 (quest-name (and data-ptr (plusp data-ptr)
                                  (read-utf16-string
                                   reader (+ data-ptr +quest-name-offset+) 64)))
                 (quest-number (and data-ptr (plusp data-ptr)
                                    (read-u16 reader (+ data-ptr +quest-number-offset+)))))
            (setf snapshot
                  (append snapshot
                          (list :quest-name (and quest-name
                                                 (string-trim " " quest-name))
                                :quest-number quest-number
                                :registers (and register-ptr (plusp register-ptr)
                                                (read-block reader register-ptr
                                                            (* 4 +register-count+)))
                                :floor-switches (read-block
                                                 reader +floor-switches+
                                                 (* 32 +floor-count+)))))))
        snapshot))))

(defun snapshot-register-set-p (snapshot register-id)
  "Quest register REGISTER-ID holds a non-zero u16 (psostats IsRegisterSet)."
  (let ((registers (getf snapshot :registers)))
    (and registers
         (< (+ (* 4 register-id) 1) (length registers))
         (plusp (bytes-u16 registers (* 4 register-id))))))

(defun snapshot-floor-switch-set-p (snapshot floor switch-id)
  "Floor switch bit, matching psostats getFloorSwitch bit layout."
  (let ((switches (getf snapshot :floor-switches))
        (offset (+ (* 32 floor) (floor switch-id 8))))
    (and switches
         (< offset (length switches))
         (plusp (logand (aref switches offset)
                        (ash #x80 (- (mod switch-id 8))))))))

(defun snapshot-my-player (snapshot)
  (find (getf snapshot :my-index) (getf snapshot :players)
        :key (lambda (player) (getf player :index))))
