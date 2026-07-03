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
    (setf (gethash "players" object)
          (coerce (loop :for player :in (getf run :players)
                        :collect (let ((entry (make-hash-table :test 'equal)))
                                   (setf (gethash "name" entry) (getf player :name)
                                         (gethash "class" entry) (getf player :class))
                                   entry))
                  'vector)
          (gethash "notes" object)
          (format nil "Auto-submitted by ephinea-ta-client (~a)"
                  (getf run :quest-name (getf run :quest-slug))))
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
