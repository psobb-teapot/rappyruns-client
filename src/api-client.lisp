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
    (12007 "server address not found - check the Server URL")
    ((12029 12030) "could not connect - server down, or no internet?")
    (12002 "connection timed out")
    ((12157 12175) "secure connection (https) failed")
    (t nil)))

(defun server-status-error-text (condition)
  "The server-status line for a failed server check, phrased for
humans instead of echoing the raw condition."
  (if (typep condition 'api-error)
      (let* ((message (api-error-message condition))
             (hint (connection-error-hint (windows-error-code message))))
        (cond (hint (format nil "Server: ~a" hint))
              ((search "Bad URL" message)
               "Server: the Server URL looks wrong - fix it and press Save settings")
              ((search "-> " message)
               (format nil "Server: unexpected response (~a) - is the URL right?"
                       message))
              (t (format nil "Server: ~a" message))))
      (format nil "Server: check failed (~a)" condition)))

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
(defun http-request (method url &key body (content-type "application/json") token)
  "Blocking HTTP request. Returns (values status-code body-string)."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (handler-case
        (multiple-value-bind (status response-body)
            (winhttp-request method scheme host port path
                             :headers (append
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
(defun http-request (method url &key body (content-type "application/json") token)
  "Blocking HTTP request. Returns (values status-code body-string)."
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

(defun telemetry-json (data)
  "Telemetry plist (TELEMETRY-RUN-DATA) -> JSON object hash table."
  (let ((object (make-hash-table :test 'equal)))
    (setf (gethash "frame_keys" object) +frame-keys+
          (gethash "frames" object)
          (coerce (mapcar (lambda (frame) (coerce frame 'vector))
                          (getf data :frames))
                  'vector)
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
          (format nil "Auto-submitted by ephinea-ta-client (~a)"
                  (getf run :quest-name (getf run :quest-slug))))
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
