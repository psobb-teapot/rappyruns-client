(in-package :ephinea-ta-client)

;;; The ephinea-ta JSON API client. On LispWorks, requests go through
;;; WinHTTP (winhttp.lisp) so https works without shipping OpenSSL DLLs.
;;; SBCL uses a minimal HTTP/1.0 client over sb-bsd-sockets (plain http,
;;; good enough for tests against a local server); HTTP/1.0 +
;;; "Connection: close" avoids chunked encoding: the response is simply
;;; everything until EOF.

(define-condition api-error (error)
  ((message :initarg :message :reader api-error-message))
  (:report (lambda (condition stream)
             (format stream "API error: ~a" (api-error-message condition)))))

(defun string-to-utf8 (string)
  #+lispworks (external-format:encode-lisp-string string :utf-8)
  #+sbcl (sb-ext:string-to-octets string :external-format :utf-8)
  #-(or lispworks sbcl) (map 'vector #'char-code string))

(defun utf8-to-string (octets)
  #+lispworks (external-format:decode-external-string octets :utf-8)
  #+sbcl (sb-ext:octets-to-string octets :external-format :utf-8)
  #-(or lispworks sbcl) (map 'string #'code-char octets))

(defun valid-http-url-p (url)
  "Only http(s) URLs may be handed to the OS to open in a browser; this
keeps a mangled Server URL setting (a local path, file://...) from ever
reaching ShellExecute."
  (and (stringp url)
       (or (eql 0 (search "http://" url))
           (eql 0 (search "https://" url)))
       (notany (lambda (char) (char<= char #\Space)) url)))

(defun windows-error-code (message)
  "Extract N from a \"... (Windows error N)\" winhttp message, or NIL."
  (let ((start (search "(Windows error " message)))
    (when start
      (parse-integer message :start (+ start 15) :junk-allowed t))))

(defun connection-error-hint (code)
  "Plain-language hint for the common WinHTTP error codes."
  (case code
    (12007 (tr :hint-address))
    ((12029 12030) (tr :hint-connect))
    (12002 (tr :hint-timeout))
    ((12157 12175) (tr :hint-tls))
    (t nil)))

(defun server-status-error-text (condition)
  "The server-status line for a failed server check, phrased for
humans instead of echoing the raw condition."
  (if (typep condition 'api-error)
      (let* ((message (api-error-message condition))
             (hint (connection-error-hint (windows-error-code message))))
        (cond (hint (tr :server-error-prefix hint))
              ((search "Bad URL" message)
               (tr :server-bad-url))
              ((search "-> " message)
               (tr :server-unexpected message))
              (t (tr :server-error-prefix message))))
      (tr :server-check-failed condition)))

(defun token-status-error-text (condition)
  "The token-status line when /api/me could not be reached at all (as
opposed to a definite 401, which the caller words itself)."
  (if (typep condition 'api-error)
      (let* ((message (api-error-message condition))
             (hint (connection-error-hint (windows-error-code message))))
        (tr :token-could-not-verify (or hint message)))
      (tr :token-could-not-verify condition)))

(defun parse-url (url)
  "Returns (values scheme host port path)."
  (let* ((scheme-end (or (search "://" url)
                         (error 'api-error :message (format nil "Bad URL: ~a" url))))
         (scheme (string-downcase (subseq url 0 scheme-end)))
         (rest (subseq url (+ scheme-end 3)))
         (path-start (position #\/ rest))
         (authority (if path-start (subseq rest 0 path-start) rest))
         (path (if path-start (subseq rest path-start) "/"))
         (colon (position #\: authority))
         (host (if colon (subseq authority 0 colon) authority))
         (port (cond (colon (parse-integer authority :start (1+ colon)))
                     ((string= scheme "https") 443)
                     (t 80))))
    (values scheme host port path)))

#-lispworks
(defun open-http-stream (scheme host port)
  "Binary stream to HOST:PORT (plain http only)."
  #+sbcl
  (progn
    (when (string= scheme "https")
      (error 'api-error :message "https is only supported on LispWorks"))
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-connect
       socket
       (sb-bsd-sockets:host-ent-address
        (sb-bsd-sockets:get-host-by-name host))
       port)
      (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                :element-type '(unsigned-byte 8))))
  #-sbcl
  (error 'api-error :message "No socket backend for this implementation"))

#-lispworks
(defun write-ascii (stream string)
  (loop :for char :across string
        :do (write-byte (char-code char) stream)))

#-lispworks
(defun read-all-bytes (stream)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)))
    (handler-case
        (loop :for byte := (read-byte stream nil nil)
              :while byte
              :do (vector-push-extend byte buffer))
      ;; A peer reset after full delivery still counts as EOF.
      (error () nil))
    buffer))

#-lispworks
(defun split-response (bytes)
  "Returns (values status-code body-octets) from a raw HTTP response."
  (let ((header-end
          (loop :for i :from 0 :to (- (length bytes) 4)
                :when (and (= (aref bytes i) 13) (= (aref bytes (+ i 1)) 10)
                           (= (aref bytes (+ i 2)) 13) (= (aref bytes (+ i 3)) 10))
                  :return i)))
    (unless header-end
      (error 'api-error :message "Malformed HTTP response"))
    (let* ((head (utf8-to-string (subseq bytes 0 header-end)))
           (space (position #\Space head))
           (status (parse-integer head :start (1+ space) :junk-allowed t)))
      (values status (subseq bytes (+ header-end 4))))))

#+lispworks
(defun http-request (method url &key body (content-type "application/json") token
                                     headers)
  "Blocking HTTP request. HEADERS is an alist of extra (name . value)
header strings. Returns (values status-code body-string)."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (handler-case
        (multiple-value-bind (status response-body)
            (winhttp-request method scheme host port path
                             :headers (append
                                       headers
                                       (when token
                                         (list (cons "Authorization"
                                                     (format nil "Bearer ~a" token))))
                                       (when body
                                         (list (cons "Content-Type" content-type))))
                             :body (and body (string-to-utf8 body)))
          (values status (utf8-to-string response-body)))
      (winhttp-error (condition)
        (error 'api-error :message (winhttp-error-message condition))))))

#-lispworks
(defun http-request (method url &key body (content-type "application/json") token
                                     headers)
  "Blocking HTTP request. HEADERS is an alist of extra (name . value)
header strings. Returns (values status-code body-string)."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (let ((stream (open-http-stream scheme host port))
          (body-octets (and body (string-to-utf8 body))))
      (unwind-protect
           (progn
             (write-ascii stream (format nil "~a ~a HTTP/1.0~c~c" method path
                                         #\Return #\Linefeed))
             (write-ascii stream (format nil "Host: ~a~c~c" host
                                         #\Return #\Linefeed))
             (write-ascii stream (format nil "Connection: close~c~c"
                                         #\Return #\Linefeed))
             (loop :for (name . value) :in headers
                   :do (write-ascii stream (format nil "~a: ~a~c~c" name value
                                                   #\Return #\Linefeed)))
             (when token
               (write-ascii stream (format nil "Authorization: Bearer ~a~c~c"
                                           token #\Return #\Linefeed)))
             (when body-octets
               (write-ascii stream (format nil "Content-Type: ~a~c~c"
                                           content-type #\Return #\Linefeed))
               (write-ascii stream (format nil "Content-Length: ~d~c~c"
                                           (length body-octets)
                                           #\Return #\Linefeed)))
             (write-ascii stream (format nil "~c~c" #\Return #\Linefeed))
             (when body-octets
               (write-sequence body-octets stream))
             (force-output stream)
             (multiple-value-bind (status response-body)
                 (split-response (read-all-bytes stream))
               (values status (utf8-to-string response-body))))
        (close stream)))))

;;; ephinea-ta API

(defun api-url (server-url path)
  (format nil "~a~a" (string-right-trim "/" server-url) path))

(defun fetch-quests (&key (server-url (config-value :server-url)))
  "Vector of quest hash-tables ({slug, name, episode, category}) from the
server, used to sanity-check local trigger definitions."
  (multiple-value-bind (status body)
      (http-request "GET" (api-url server-url "/api/quests"))
    (unless (eql status 200)
      (error 'api-error :message (format nil "GET /api/quests -> ~a" status)))
    (jzon:parse body)))

(defun normalize-token (string)
  "A pasted token, trimmed of the whitespace (including newlines) that
tends to come along when copying from a browser. NIL becomes \"\"."
  (string-trim '(#\Space #\Tab #\Return #\Linefeed) (or string "")))

(defun fetch-me (&key (server-url (config-value :server-url))
                      (token (config-value :api-token)))
  "Verify TOKEN against GET /api/me.
Returns (values :ok user-hash) on 200 and (values :unauthorized nil) on
401, so a bad token is distinguishable from the API-ERROR signalled on
transport failures and unexpected statuses."
  (multiple-value-bind (status body)
      (http-request "GET" (api-url server-url "/api/me") :token token)
    (case status
      (200 (values :ok (jzon:parse body)))
      (401 (values :unauthorized nil))
      (t (error 'api-error :message (format nil "GET /api/me -> ~a" status))))))

(defun alist-json (alist &key (key #'string-downcase))
  "Alist -> JSON object hash table, dropping zero counts."
  (let ((object (make-hash-table :test 'equal)))
    (loop :for (name . count) :in alist
          :when (and count (plusp count))
            :do (setf (gethash (funcall key name) object) count))
    object))

(defun consumable-json-key (key)
  ;; :moon-atomizer -> "moon_atomizer"
  (substitute #\_ #\- (string-downcase (symbol-name key))))

(defun locs-json (entries)
  "Location rows ((key . values) lists) -> JSON object key -> vector."
  (let ((object (make-hash-table :test 'equal)))
    (dolist (entry entries object)
      (setf (gethash (princ-to-string (first entry)) object)
            (coerce (rest entry) 'vector)))))

(defun frame-json (frame)
  "One frame row -> JSON vector; the player_locs / monster_locs columns
hold nested location lists that become objects."
  (coerce (loop :for value :in frame
                :for key :across +frame-keys+
                :collect (if (member key '("player_locs" "monster_locs")
                                     :test #'string=)
                             (locs-json value)
                             value))
          'vector))

(defun monsters-json (monsters)
  "Per-monster records -> JSON array (psostats QuestRun.Monsters)."
  (coerce
   (loop :for monster :in monsters
         :collect (let ((entry (make-hash-table :test 'equal)))
                    (setf (gethash "id" entry) (getf monster :id)
                          (gethash "unitxt_id" entry) (getf monster :unitxt)
                          (gethash "spawn_ms" entry) (getf monster :spawn-ms))
                    (when (getf monster :name)
                      (setf (gethash "name" entry) (getf monster :name)))
                    (when (getf monster :killed-ms)
                      (setf (gethash "killed_ms" entry)
                            (getf monster :killed-ms)))
                    (when (getf monster :frame1)
                      (setf (gethash "frame1" entry) t))
                    entry))
   'vector))

(defun bosses-json (bosses)
  "Boss records -> JSON object keyed by monster id (psostats Bosses)."
  (let ((object (make-hash-table :test 'equal)))
    (dolist (boss bosses object)
      (let ((entry (make-hash-table :test 'equal)))
        (setf (gethash "name" entry) (getf boss :name)
              (gethash "unitxt_id" entry) (getf boss :unitxt)
              (gethash "spawn_t" entry) (getf boss :spawn-t)
              (gethash "hp" entry) (coerce (getf boss :hp) 'vector))
        (when (getf boss :killed-t)
          (setf (gethash "killed_t" entry) (getf boss :killed-t)))
        (setf (gethash (princ-to-string (getf boss :id)) object) entry)))))

(defun telemetry-json (data)
  "Telemetry plist (TELEMETRY-RUN-DATA) -> JSON object hash table."
  (let ((object (make-hash-table :test 'equal)))
    (setf (gethash "frame_keys" object) +frame-keys+
          (gethash "frames" object)
          (coerce (mapcar #'frame-json (getf data :frames)) 'vector)
          (gethash "death_count" object) (or (getf data :death-count) 0)
          (gethash "kills" object) (or (getf data :kills) 0)
          (gethash "meseta_charged" object) (or (getf data :meseta-charged) 0)
          (gethash "tp_used" object) (or (getf data :tp-used) 0)
          (gethash "items_used" object)
          (alist-json (getf data :items-used) :key #'consumable-json-key)
          (gethash "techs_cast" object)
          (alist-json (getf data :techs-cast) :key #'identity)
          (gethash "time_by_state" object)
          (alist-json (getf data :time-by-state) :key #'princ-to-string))
    (let ((traps (getf data :traps-used)))
      (setf (gethash "traps_used" object)
            (alist-json (loop :for (key value) :on traps :by #'cddr
                              :collect (cons key value))
                        :key (lambda (key) (string-downcase (symbol-name key))))))
    (setf (gethash "weapons" object)
          (coerce (loop :for weapon :in (getf data :weapons)
                        :collect (let ((entry (make-hash-table :test 'equal)))
                                   (setf (gethash "id" entry) (getf weapon :id)
                                         (gethash "type" entry)
                                         (string-downcase
                                          (symbol-name (getf weapon :type :weapon)))
                                         (gethash "display" entry)
                                         (getf weapon :display)
                                         (gethash "seconds" entry)
                                         (getf weapon :seconds 0)
                                         (gethash "attacks" entry)
                                         (getf weapon :attacks 0)
                                         (gethash "techs" entry)
                                         (getf weapon :techs 0))
                                   entry))
                  'vector)
          (gethash "events" object)
          (coerce (loop :for event :in (getf data :events)
                        :collect (let ((entry (make-hash-table :test 'equal)))
                                   (setf (gethash "t" entry) (getf event :t)
                                         (gethash "type" entry) (getf event :type))
                                   (when (getf event :floor)
                                     (setf (gethash "floor" entry)
                                           (getf event :floor)))
                                   entry))
                  'vector))
    ;; psostats parity: per-monster records, boss HP histories, damage
    ;; and last-hit attribution, and the cheat heuristics. Absent from
    ;; runs queued by older client versions, hence the WHENs.
    (when (getf data :monsters)
      (setf (gethash "monsters" object) (monsters-json (getf data :monsters))))
    (when (getf data :bosses)
      (setf (gethash "bosses" object) (bosses-json (getf data :bosses))))
    (setf (gethash "player_damage" object)
          (alist-json (getf data :player-damage) :key #'princ-to-string)
          (gethash "last_hits" object)
          (alist-json (getf data :last-hits) :key #'princ-to-string)
          (gethash "monster_hp_pool" object)
          (coerce (getf data :monster-hp-pool) 'vector))
    (when (getf data :max-party-pb-shifta)
      (setf (gethash "max_party_pb_shifta" object)
            (getf data :max-party-pb-shifta)))
    ;; Booleans ride only when true, like pb in RUN-JSON.
    (when (getf data :illegal-shifta)
      (setf (gethash "illegal_shifta" object) t))
    (when (getf data :fast-warps)
      (setf (gethash "fast_warps" object) t))
    object))

(defun run-json (run)
  "Encode a detector run plist as the POST /api/runs request body."
  (let ((object (make-hash-table :test 'equal)))
    (setf (gethash "quest" object) (getf run :quest-slug)
          (gethash "time_ms" object) (getf run :time-ms)
          (gethash "party_size" object) (getf run :party-size))
    ;; The server defaults pb to false; only send it when true to avoid
    ;; depending on how the JSON encoder spells booleans.
    (when (getf run :pb)
      (setf (gethash "pb" object) t))
    (when (getf run :episode)
      (setf (gethash "episode" object) (getf run :episode)))
    (when (getf run :difficulty)
      (setf (gethash "difficulty" object) (getf run :difficulty)))
    (when (getf run :death-count)
      (setf (gethash "death_count" object) (getf run :death-count)))
    (when (getf run :aborted)
      (setf (gethash "aborted" object) t))
    (setf (gethash "players" object)
          (coerce (loop :for player :in (getf run :players)
                        :collect (let ((entry (make-hash-table :test 'equal)))
                                   (setf (gethash "name" entry) (getf player :name)
                                         (gethash "class" entry) (getf player :class))
                                   (when (getf player :level)
                                     (setf (gethash "level" entry)
                                           (getf player :level)))
                                   (when (getf player :section-id)
                                     (setf (gethash "section_id" entry)
                                           (getf player :section-id)))
                                   (when (getf player :guild-card)
                                     (setf (gethash "guild_card" entry)
                                           (getf player :guild-card)))
                                   entry))
                  'vector)
          (gethash "notes" object)
          (format nil "Auto-submitted by ephinea-ta-client (~a~:[~;, aborted~])"
                  (getf run :quest-name (getf run :quest-slug))
                  (getf run :aborted)))
    (when (getf run :telemetry)
      (setf (gethash "telemetry" object)
            (telemetry-json (getf run :telemetry))))
    (jzon:stringify object)))

(defun submit-run (run &key (server-url (config-value :server-url))
                            (token (config-value :api-token)))
  "POST a detector run as a draft.
Returns (values outcome payload) where outcome is :created, :duplicate or
:rejected; PAYLOAD is the parsed JSON response. Signals API-ERROR on
authentication and transport failures (worth retrying / reconfiguring)."
  (multiple-value-bind (status body)
      (http-request "POST" (api-url server-url "/api/runs")
                    :body (run-json run) :token token)
    (let ((payload (ignore-errors (jzon:parse body))))
      (case status
        (201 (values :created payload))
        (200 (values :duplicate payload))
        (400 (values :rejected payload))
        (401 (error 'api-error :message "Invalid or revoked API token"))
        (403 (values :rejected payload))
        (t (error 'api-error
                  :message (format nil "POST /api/runs -> ~a: ~a" status body)))))))

(defun attach-run-video (server-id video-url
                         &key (server-url (config-value :server-url))
                              (token (config-value :api-token)))
  "POST /api/runs/:id/video: attach VIDEO-URL to the draft SERVER-ID,
promoting it to pending review. Returns (values outcome payload) where
outcome is :attached or :rejected; PAYLOAD is the parsed JSON response.
Signals API-ERROR on authentication and transport failures."
  (let ((request-body (let ((object (make-hash-table :test 'equal)))
                        (setf (gethash "video_url" object) video-url)
                        (jzon:stringify object))))
    (multiple-value-bind (status body)
        (http-request "POST" (api-url server-url
                                      (format nil "/api/runs/~d/video" server-id))
                      :body request-body :token token)
      (let ((payload (ignore-errors (jzon:parse body))))
        (case status
          (200 (values :attached payload))
          ((400 403 404) (values :rejected payload))
          (401 (error 'api-error :message "Invalid or revoked API token"))
          (t (error 'api-error
                    :message (format nil "POST /api/runs/~d/video -> ~a: ~a"
                                     server-id status body))))))))

(defun video-file-path (server-id offset-ms)
  (format nil "/api/runs/~d/video-file~@[?offset_ms=~d~]" server-id offset-ms))

#+lispworks
(defun video-file-request (server-id file-path server-url token on-progress
                           &optional offset-ms)
  (multiple-value-bind (scheme host port path)
      (parse-url (api-url server-url (video-file-path server-id offset-ms)))
    (handler-case
        (multiple-value-bind (status response-body)
            (winhttp-upload-file "POST" scheme host port path file-path
                                 :headers (append
                                           (when token
                                             (list (cons "Authorization"
                                                         (format nil "Bearer ~a" token))))
                                           (list (cons "Content-Type" "video/mp4")))
                                 :on-progress on-progress)
          (values status (utf8-to-string response-body)))
      (winhttp-error (condition)
        (error 'api-error :message (winhttp-error-message condition))))))

#-lispworks
(defun video-file-request (server-id file-path server-url token on-progress
                           &optional offset-ms)
  "Test backend: buffers the whole file, fine for small fixtures."
  (multiple-value-bind (scheme host port path)
      (parse-url (api-url server-url (video-file-path server-id offset-ms)))
    (let ((body (with-open-file (in file-path :element-type '(unsigned-byte 8))
                  (let ((bytes (make-array (file-length in)
                                           :element-type '(unsigned-byte 8))))
                    (read-sequence bytes in)
                    bytes)))
          (stream (open-http-stream scheme host port)))
      (unwind-protect
           (progn
             (write-ascii stream (format nil "POST ~a HTTP/1.0~c~c" path
                                         #\Return #\Linefeed))
             (write-ascii stream (format nil "Host: ~a~c~c" host
                                         #\Return #\Linefeed))
             (write-ascii stream (format nil "Connection: close~c~c"
                                         #\Return #\Linefeed))
             (when token
               (write-ascii stream (format nil "Authorization: Bearer ~a~c~c"
                                           token #\Return #\Linefeed)))
             (write-ascii stream (format nil "Content-Type: video/mp4~c~c"
                                         #\Return #\Linefeed))
             (write-ascii stream (format nil "Content-Length: ~d~c~c"
                                         (length body) #\Return #\Linefeed))
             (write-ascii stream (format nil "~c~c" #\Return #\Linefeed))
             (write-sequence body stream)
             (force-output stream)
             (when on-progress
               (funcall on-progress (length body) (length body)))
             (multiple-value-bind (status response-body)
                 (split-response (read-all-bytes stream))
               (values status (utf8-to-string response-body))))
        (close stream)))))

(defun upload-run-video (server-id file-path
                         &key (server-url (config-value :server-url))
                              (token (config-value :api-token))
                              offset-ms
                              on-progress)
  "POST /api/runs/:id/video-file: stream the recording at FILE-PATH up
to the server, which relays it into hosted storage. Ordinary drafts are
promoted to pending review; aborted runs stay drafts. OFFSET-MS, when
known, is the video timestamp where the run's timer starts (the
recorder measures it); the server stores it as video_offset_ms so the
telemetry timeline seeks land where they should. Returns (values
outcome payload): :attached, :duplicate (a video was already on file -
both count as done) or :rejected (permanent, except the pending-limit
error, which is worth retrying later); PAYLOAD is the parsed JSON
response. Signals API-ERROR on authentication and transport failures
\(worth retrying). ON-PROGRESS is called with (bytes-so-far total)."
  (multiple-value-bind (status body)
      (video-file-request server-id file-path server-url token on-progress
                          offset-ms)
    (let ((payload (ignore-errors (jzon:parse body))))
      (case status
        ((200 201) (values (if (and (hash-table-p payload)
                                    (gethash "duplicate" payload))
                               :duplicate
                               :attached)
                           payload))
        ((400 403 404 409 411 413) (values :rejected payload))
        (401 (error 'api-error :message "Invalid or revoked API token"))
        (t (error 'api-error
                  :message (format nil "POST /api/runs/~d/video-file -> ~a: ~a"
                                   server-id status body)))))))

;;; Recognizing YouTube links on the clipboard. Deliberately simple
;;; string matching (no regex dependency); the server's validator has
;;; the final say, this only decides when to offer the attach prompt.

(defun youtube-id-char-p (char)
  (or (char<= #\a char #\z) (char<= #\A char #\Z)
      (char<= #\0 char #\9) (char= char #\-) (char= char #\_)))

(defun youtube-id-at-p (url start)
  "True when an 11-character video id sits at START in URL, ending the
string or followed by a URL delimiter (so longer id-like runs fail)."
  (let ((end (+ start 11)))
    (and (<= end (length url))
         (loop :for i :from start :below end
               :always (youtube-id-char-p (char url i)))
         (or (= end (length url))
             (member (char url end) '(#\? #\& #\# #\/))))))

(defun youtube-video-url (text)
  "Trimmed TEXT when it looks like a link to a single YouTube video
\(watch?v=..., youtu.be/..., /shorts/..., /live/...), else NIL."
  (let* ((url (string-trim '(#\Space #\Tab #\Return #\Linefeed) (or text "")))
         (host-start (cond ((eql 0 (search "https://" url)) 8)
                           ((eql 0 (search "http://" url)) 7))))
    (when (and host-start
               (notany (lambda (char) (char<= char #\Space)) url))
      (let* ((host-end (or (position #\/ url :start host-start) (length url)))
             (host (string-downcase (subseq url host-start host-end))))
        (flet ((id-after (prefix)
                 "Id right after PREFIX when the path starts with it."
                 (let ((end (+ host-end (length prefix))))
                   (and (<= end (length url))
                        (string= prefix url :start2 host-end :end2 end)
                        (youtube-id-at-p url end)))))
          (cond
            ((string= host "youtu.be")
             (and (youtube-id-at-p url (1+ host-end)) url))
            ((member host '("www.youtube.com" "youtube.com" "m.youtube.com")
                     :test #'string=)
             (cond
               ((or (id-after "/shorts/") (id-after "/live/")) url)
               (t ;; /watch: the v= parameter may sit anywhere in the query.
                (let ((v (search "v=" url :start2 host-end)))
                  (and v
                       (member (char url (1- v)) '(#\? #\&))
                       (eql (search "/watch?" url :start2 host-end) host-end)
                       (youtube-id-at-p url (+ v 2))
                       url)))))))))))
