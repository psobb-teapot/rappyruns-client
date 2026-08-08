(in-package :ephinea-ta-client-tests)

;;; ------------------------------------------------------------------
;;; Coverage gaps: pure helpers that had no direct tests (T16)
;;; ------------------------------------------------------------------

(defun run-pure-helper-tests ()
  (format t "~&--- pure helpers ---~%")
  ;; parse-url (api-client): the URL splitter every request goes through.
  (multiple-value-bind (scheme host port path)
      (ephinea-ta-client::parse-url "https://example.com/api/quests")
    (check "parse-url https defaults"
           (and (equal scheme "https") (equal host "example.com")
                (eql port 443) (equal path "/api/quests"))))
  (multiple-value-bind (scheme host port path)
      (ephinea-ta-client::parse-url "http://localhost:8123")
    (check "parse-url explicit port, bare authority"
           (and (equal scheme "http") (equal host "localhost")
                (eql port 8123) (equal path "/"))))
  (check "parse-url schemeless URL signals api-error"
         (eq :api-error
             (handler-case (ephinea-ta-client::parse-url "example.com/x")
               (ephinea-ta-client::api-error () :api-error)
               (error () :other))))
  ;; Pinned current behavior: a junk port escapes as a bare
  ;; parse-error, NOT api-error (backlog T16 - fix is a behavior
  ;; change, so first freeze what it does today).
  (check "parse-url junk port signals a non-api error (pinned)"
         (eq :other
             (handler-case (ephinea-ta-client::parse-url "http://h:abc/")
               (ephinea-ta-client::api-error () :api-error)
               (error () :other))))
  ;; u64-double (memory): the f64 decoder under the Anguish HP-scale read.
  (check "u64-double decodes 1.0"
         (= 1.0d0 (ephinea-ta-client::u64-double #x3FF0000000000000)))
  (check "u64-double decodes -2.5"
         (= -2.5d0 (ephinea-ta-client::u64-double #xC004000000000000)))
  (check "u64-double decodes the Anguish 1.30 scale"
         (< (abs (- 1.30d0 (ephinea-ta-client::u64-double #x3FF4CCCCCCCCCCCD)))
            1d-12))
  (check "u64-double clamps infinity/NaN"
         (= 1.7d308 (ephinea-ta-client::u64-double #x7FF0000000000000)))
  (check "u64-double decodes subnormals"
         (< 0 (ephinea-ta-client::u64-double 1) 1d-300))
  ;; trigger-met-p (detect): the two arms not exercised through
  ;; detector-step's snapshots.
  (check "trigger-met-p warp-in needs a landed player"
         (and (ephinea-ta-client::trigger-met-p
               '(:warp-in) '(:players ((:floor 1 :warping nil))))
              (not (ephinea-ta-client::trigger-met-p
                    '(:warp-in) '(:players ((:floor 1 :warping t)))))
              (not (ephinea-ta-client::trigger-met-p
                    '(:warp-in) '(:players ((:floor 0 :warping nil)))))))
  (check "trigger-met-p monster-dead consults killed ids"
         (and (ephinea-ta-client::trigger-met-p
               '(:monster-dead 42) '() '(41 42))
              (not (ephinea-ta-client::trigger-met-p
                    '(:monster-dead 42) '() '(41)))))
  ;; deduplicate-path (recording): Explorer-style collision counter.
  (let* ((dir (uiop:temporary-directory))
         (stem (format nil "eta-t16-~d" (get-universal-time)))
         (taken (namestring (merge-pathnames (format nil "~a.mp4" stem) dir)))
         (free (namestring (merge-pathnames (format nil "~a-free.mp4" stem) dir))))
    (unwind-protect
         (progn
           (with-open-file (out taken :direction :output) (declare (ignorable out)))
           (check "deduplicate-path leaves a free name alone"
                  (equal free (ephinea-ta-client::deduplicate-path free)))
           (check "deduplicate-path counters a taken name"
                  (equal (namestring (merge-pathnames
                                      (format nil "~a (2).mp4" stem) dir))
                         (ephinea-ta-client::deduplicate-path taken))))
      (ignore-errors (delete-file taken))))
  ;; unknown-slugs (quests): the startup slug cross-check.
  (let ((defs (list (ephinea-ta-client::make-quest-def :slug "ep1-known")
                    (ephinea-ta-client::make-quest-def :slug "ep2-typo"))))
    (check "unknown-slugs reports only the unmatched"
           (equal '("ep2-typo")
                  (ephinea-ta-client::unknown-slugs '("ep1-known") defs)))
    (check "unknown-slugs empty when everything matches"
           (null (ephinea-ta-client::unknown-slugs
                  '("ep1-known" "ep2-typo") defs)))))

;;; ------------------------------------------------------------------
;;; Quest-rule form logic (rule-form.lisp)
;;; ------------------------------------------------------------------

(defun run-rule-form-tests ()
  (format t "~&--- rule form ---~%")
  (flet ((table (&rest kvs)
           (let ((h (make-hash-table :test 'equal)))
             (loop :for (k v) :on kvs :by #'cddr :do (setf (gethash k h) v))
             h)))
    (let* ((timeable (table "name" "GDV reset" "slug" "ep2-gdv-reset"
                            "episode" 2
                            "start" (table "type" "warp-in")
                            "end" (table "type" "register")
                            "game_number" 118
                            "game_names" (vector "Gal Da Val")))
           (bare (table "name" "Plain" "slug" "ep1-plain" "episode" 1))
           (parents (timeable-quests (vector timeable bare))))
      (check "timeable-quests keeps only trigger-carrying quests"
             (equal parents (list timeable)))
      (check "detected-parent matches by game number"
             (eq timeable
                 (detected-parent parents (list :number 118 :episode 2))))
      (check "detected-parent falls back to episode + name"
             (eq timeable
                 (detected-parent parents
                                  (list :number 0 :name "Gal Da Val"
                                        :episode 2))))
      (check "detected-parent needs the episode to agree"
             (null (detected-parent parents
                                    (list :number 0 :name "Gal Da Val"
                                          :episode 1))))
      (check "detected-parent nil run-quest"
             (null (detected-parent parents nil))))
    (check "rule-error-message prefers the message"
           (equal "boom" (rule-error-message (table "message" "boom"))))
    (check "rule-error-message joins the errors"
           (equal "a; b" (rule-error-message
                          (table "errors" (vector "a" "b")))))
    (check "rule-error-message shrugs at garbage"
           (equal "?" (rule-error-message nil))))
  (check "rule-trigger-label nil" (equal "" (rule-trigger-label nil)))
  (check "rule-trigger-label warp-in"
         (equal "warp-in" (rule-trigger-label '(:warp-in))))
  (check "rule-trigger-label register"
         (equal "register:7" (rule-trigger-label '(:register 7))))
  (check "rule-trigger-label floor-switch"
         (equal "floor-switch:3:42" (rule-trigger-label '(:floor-switch 3 42))))
  (check "rule-trigger-label monster"
         (equal "monster:9" (rule-trigger-label '(:monster-dead 9))))
  (check "rule-manual-marker-p marker" (rule-manual-marker-p :monster))
  (check "rule-manual-marker-p trigger list"
         (not (rule-manual-marker-p '(:warp-in))))
  (check "parse-int-in-range trims and parses"
         (eql 42 (parse-int-in-range " 42 " 0 255)))
  (check "parse-int-in-range rejects out of range"
         (null (parse-int-in-range "256" 0 255)))
  (check "parse-int-in-range rejects junk"
         (null (parse-int-in-range "4x" 0 255)))
  (check "resolve monster"
         (equal '(:monster-dead 300) (resolve-manual-trigger :monster "300" nil)))
  (check "resolve floor-switch"
         (equal '(:floor-switch 5 128)
                (resolve-manual-trigger :floor-switch "5" "128")))
  (check "resolve floor-switch rejects a bad switch"
         (null (resolve-manual-trigger :floor-switch "5" "300")))
  (check "resolve register"
         (equal '(:register 8) (resolve-manual-trigger :register "8" nil)))
  (check "moderator-role-p"
         (and (moderator-role-p "moderator")
              (moderator-role-p "admin")
              (not (moderator-role-p "user"))
              (not (moderator-role-p nil)))))

;;; ------------------------------------------------------------------
;;; UX helpers: status labels, list trimming, URL and error text
;;; ------------------------------------------------------------------

(defun run-ux-helper-tests ()
  (format t "~&--- ux helpers ---~%")
  ;; Status labels (shown in the runs list).
  (let ((label (ephinea-ta-client::run-status-label
                (list :status :submitted
                      :url "https://example.com/runs/42"))))
    (check "submitted label does not leak the URL"
           (not (search "http" label)))
    (check "submitted label says draft and hints at the video step"
           (and (search "draft" label) (search "video" label))))
  (check "rejected label carries the reason"
         (search "too fast"
                 (ephinea-ta-client::run-status-label
                  (list :status :rejected :reason "too fast"))))
  (check "format-run-time formats minutes:seconds.millis"
         (string= "9:59.123" (ephinea-ta-client::format-run-time 599123)))
  ;; Reward hint: personal-best delta and provisional board rank the
  ;; server returns with a fresh submission.
  (check "format-improvement-ms shows a sub-minute delta in seconds"
         (string= "3.21s" (ephinea-ta-client::format-improvement-ms 3210)))
  (check "run-standing-note leads with the personal best, then the rank"
         (let ((note (ephinea-ta-client::run-standing-note
                      (list :standing-rank 2 :standing-parties 5
                            :standing-delta-ms -3210))))
           (and note (search "PB! -3.21s" note) (search "#2" note))))
  (check "run-standing-note behind the own best shows the gap, still ranked"
         (let ((note (ephinea-ta-client::run-standing-note
                      (list :standing-rank 3 :standing-parties 8
                            :standing-delta-ms 1500))))
           (and note (search "1.50" note) (search "#3" note))))
  ;; A solo board (one competing party) drops the "#1 of 1" noise and
  ;; shows only the personal-best line - the GDV-reset case.
  (check "run-standing-note on a solo board hides the rank on a PB"
         (let ((note (ephinea-ta-client::run-standing-note
                      (list :standing-rank 1 :standing-parties 1
                            :standing-delta-ms -3210))))
           (and note (search "PB! -3.21s" note) (not (search "#" note)))))
  (check "run-standing-note on a solo board shows the gap behind the own best"
         (let ((note (ephinea-ta-client::run-standing-note
                      (list :standing-rank 1 :standing-parties 1
                            :standing-delta-ms 16830))))
           (and note (search "16.83s" note) (not (search "#" note)))))
  (check "run-standing-note marks a first run when there is no prior time"
         (let ((note (ephinea-ta-client::run-standing-note
                      (list :standing-rank 1 :standing-parties 1))))
           (and note (search "first run" note) (not (search "#" note)))))
  (check "run-standing-note is NIL when the server sent no standing"
         (null (ephinea-ta-client::run-standing-note (list :status :submitted))))
  ;; Celebration toast: only genuinely good news makes a balloon; the
  ;; rest stays a quiet line in the list.
  (check "standing-toast celebrates a provisional #1 over another party"
         (multiple-value-bind (title text)
             (ephinea-ta-client::standing-toast
              (list :quest-name "Lost HEAT SWORD" :time-ms 599123
                    :standing-rank 1 :standing-parties 5
                    :standing-delta-ms -3210))
           (and (search "#1" title)
                (search "Lost HEAT SWORD" text)
                (search "9:59.123" text))))
  (check "standing-toast celebrates a top-3 entry that beat somebody"
         (multiple-value-bind (title text)
             (ephinea-ta-client::standing-toast
              (list :quest-name "Q" :time-ms 60000
                    :standing-rank 3 :standing-parties 8))
           (and (search "#3" title) (search "of 8" text))))
  (check "standing-toast stays quiet in last place, even inside the top 3"
         (null (ephinea-ta-client::standing-toast
                (list :quest-name "Q" :time-ms 60000
                      :standing-rank 3 :standing-parties 3
                      :standing-delta-ms 1500))))
  (check "standing-toast on a solo board celebrates the PB, never '#1 of 1'"
         (multiple-value-bind (title text)
             (ephinea-ta-client::standing-toast
              (list :quest-name "Q" :time-ms 60000
                    :standing-rank 1 :standing-parties 1
                    :standing-delta-ms -3210))
           (and (not (search "#" title)) (search "3.21s" text))))
  (check "standing-toast marks a first run on a solo board without a rank"
         (multiple-value-bind (title text)
             (ephinea-ta-client::standing-toast
              (list :quest-name "Q" :time-ms 60000
                    :standing-rank 1 :standing-parties 1))
           (and (search "First" title) (not (search "#" text)))))
  (check "standing-toast says nothing when behind your own best mid-board"
         (null (ephinea-ta-client::standing-toast
                (list :quest-name "Q" :time-ms 60000
                      :standing-rank 5 :standing-parties 8
                      :standing-delta-ms 1500))))
  (check "standing-toast is NIL without a standing"
         (null (ephinea-ta-client::standing-toast
                (list :quest-name "Q" :time-ms 60000 :status :submitted))))
  (check "the submitted status label appends the standing note"
         (with-recording-config (:video-upload t)
           (let ((label (ephinea-ta-client::run-status-label
                         (list :status :submitted
                               :standing-rank 2 :standing-parties 4
                               :standing-delta-ms -1000))))
             (and (search "draft" label) (search "#2" label)))))
  ;; Trimming: unfinished entries survive, finished ones are capped.
  (let* ((runs (loop :for i :from 0 :below 70
                     :collect (list :status (case (mod i 7)
                                              (3 :queued)
                                              (5 :failed)
                                              (t :submitted))
                                    :n i)))
         (trimmed (ephinea-ta-client::trim-finished-runs runs 50)))
    (check "trim keeps every queued/failed entry"
           (= (count-if (lambda (entry)
                          (member (getf entry :status) '(:queued :failed)))
                        runs)
              (count-if (lambda (entry)
                          (member (getf entry :status) '(:queued :failed)))
                        trimmed)))
    (check "trim caps finished entries at the limit"
           (= 50 (count :submitted trimmed :key (lambda (e) (getf e :status)))))
    (check "trim keeps the newest finished entries in order"
           (equal (subseq (mapcar (lambda (e) (getf e :n)) runs) 0 10)
                  (subseq (mapcar (lambda (e) (getf e :n)) trimmed) 0 10))))
  ;; Unlinked (no API token) mode: measuring works but nothing submits -
  ;; the queued label points at linking and SUBMIT-QUEUED! stays off the
  ;; network entirely, so no 401 failures pile up before the user links.
  (check "unlinked-p is true with the default (empty) token"
         (with-recording-config ()
           (ephinea-ta-client::unlinked-p)))
  (check "unlinked-p is false once a token is configured"
         (with-recording-config (:api-token "eta_x")
           (not (ephinea-ta-client::unlinked-p))))
  (check "queued label while unlinked points at linking with the site"
         (with-recording-config ()
           (search "link" (ephinea-ta-client::run-status-label
                           (list :status :queued)))))
  (check "queued label with a token stays the plain queued"
         (with-recording-config (:api-token "eta_x")
           (string= "queued" (ephinea-ta-client::run-status-label
                              (list :status :queued)))))
  (check "submit-queued! is a no-op while unlinked, leaving entries queued"
         (with-recording-config ()
           (with-test-store ((list :status :queued :quest-slug "q" :time-ms 1))
             (and (null (ephinea-ta-client::submit-queued!))
                  (eq :queued (getf (first (ephinea-ta-client::queued-runs))
                                    :status))))))
  ;; Debug mode gates developer-only settings (the Server URL field).
  (check "debug mode is off by default"
         (with-recording-config ()
           (not (ephinea-ta-client::debug-mode-p))))
  (check "debug mode can be enabled in config"
         (with-recording-config (:debug t)
           (ephinea-ta-client::debug-mode-p)))
  ;; Browser URL guard.
  (check "http and https URLs are openable"
         (and (ephinea-ta-client::valid-http-url-p "http://x/y")
              (ephinea-ta-client::valid-http-url-p "https://x/y")))
  (check "non-web strings are rejected"
         (notany #'ephinea-ta-client::valid-http-url-p
                 (list "C:\\evil.exe" "file:///c:/x" "https://x/a b"
                       "javascript:alert(1)" nil 42 "")))
  ;; Human-readable server errors.
  (check "windows error code is extracted"
         (= 12029 (ephinea-ta-client::windows-error-code
                   "WinHttpConnect failed (Windows error 12029)")))
  (check "windows error code absent -> nil"
         (null (ephinea-ta-client::windows-error-code "plain message")))
  (flet ((text-for (message)
           (ephinea-ta-client::server-status-error-text
            (make-condition 'ephinea-ta-client::api-error :message message))))
    (check "connection failure reads like a sentence, not a condition"
           (search "could not connect"
                   (text-for "WinHttpConnect failed (Windows error 12029)")))
    (check "bad URL points at the settings fix"
           (search "Save & verify" (text-for "Bad URL: nonsense")))
    (check "unexpected HTTP status mentions the response"
           (search "unexpected response"
                   (text-for "GET /api/quests -> 500"))))
  (check "non-api conditions still say what happened"
         (search "check failed"
                 (ephinea-ta-client::server-status-error-text
                  (make-condition 'simple-error
                                  :format-control "boom"))))
  ;; Token paste normalization (browser copies drag whitespace along).
  (check "normalize-token trims spaces and CRLF"
         (string= "eta_abc123"
                  (normalize-token (format nil "  eta_abc123~c~c" #\Return #\Linefeed))))
  (check "normalize-token trims tabs"
         (string= "eta_abc123"
                  (normalize-token (format nil "~ceta_abc123~c" #\Tab #\Tab))))
  (check "normalize-token maps nil to empty"
         (string= "" (normalize-token nil)))
  (check "normalize-token keeps empty empty"
         (string= "" (normalize-token "   ")))
  ;; Token check errors read like sentences too.
  (check "token transport failure reads like a sentence"
         (search "could not connect"
                 (ephinea-ta-client::token-status-error-text
                  (make-condition 'ephinea-ta-client::api-error
                                  :message "WinHttpConnect failed (Windows error 12029)")))))

;;; ------------------------------------------------------------------
;;; Self-update: version compare, release JSON, zip check, helper script
;;; ------------------------------------------------------------------

(defparameter *release-json-sample*
  "{\"tag_name\": \"v0.6.0\",
    \"prerelease\": false,
    \"assets\": [
      {\"name\": \"notes.txt\",
       \"size\": 12,
       \"browser_download_url\": \"https://example.com/notes.txt\"},
      {\"name\": \"OtherTool.zip\",
       \"size\": 12345678,
       \"browser_download_url\": \"https://github.com/x/y/releases/download/v0.6.0/OtherTool.zip\"},
      {\"name\": \"RappyRunsClient.zip\",
       \"size\": 12345678,
       \"browser_download_url\": \"https://github.com/x/y/releases/download/v0.6.0/RappyRunsClient.zip\"}]}")

(defun run-updater-tests ()
  (format t "~&--- updater ---~%")
  ;; Version parsing: strict X.Y.Z, malformed tags never update.
  (check "parse-version reads v-prefixed tags"
         (equal '(1 2 3) (parse-version "v1.2.3")))
  (check "parse-version reads bare versions"
         (equal '(0 10 0) (parse-version "0.10.0")))
  (check "parse-version rejects two components"
         (null (parse-version "v1.2")))
  (check "parse-version rejects four components"
         (null (parse-version "v1.2.3.4")))
  (check "parse-version rejects suffixes"
         (null (parse-version "v1.2.3-rc1")))
  (check "parse-version rejects words and empties"
         (notany #'parse-version (list "abc" "" "v" "v.." nil 42)))
  ;; Comparison is numeric, not textual.
  (check "version< compares components as numbers"
         (and (version< '(0 9 9) '(0 10 0))
              (not (version< '(0 10 0) '(0 9 9)))))
  (check "version< is false on equal versions"
         (not (version< '(1 2 3) '(1 2 3))))
  (check "version< orders on the major first"
         (version< '(1 9 9) '(2 0 0)))
  ;; update-available-p falls on the do-not-update side.
  (check "update available for a newer tag"
         (update-available-p "0.5.0" "v0.6.0"))
  (check "no update for the same tag"
         (not (update-available-p "0.6.0" "v0.6.0")))
  (check "no update when the current version is nil (dev)"
         (not (update-available-p nil "v0.6.0")))
  (check "no update when the latest tag is malformed"
         (not (update-available-p "0.5.0" "release-2")))
  ;; The pre-GUI startup pass: only :apply downloads before the main
  ;; window shows; everything else starts the client normally.
  (let ((release (parse-release-json *release-json-sample*)))
    (check "startup decision applies a newer release"
           (eq :apply (startup-update-decision release "0.5.0" t)))
    (check "startup decision defers to the manual page when not writable"
           (eq :not-writable (startup-update-decision release "0.5.0" nil)))
    (check "startup decision is up-to-date on the same version"
           (eq :up-to-date (startup-update-decision release "0.6.0" t)))
    (check "startup decision never updates a dev build"
           (eq :up-to-date (startup-update-decision release nil t))))
  (check "startup decision reports a failed release check"
         (eq :check-failed (startup-update-decision nil "0.5.0" t)))
  ;; Release JSON -> plist.
  (let ((release (parse-release-json *release-json-sample*)))
    (check "release json yields the tag"
           (equal "v0.6.0" (getf release :tag)))
    (check "release json picks the client zip asset, not other assets"
           (search "download/v0.6.0/RappyRunsClient.zip"
                   (getf release :asset-url)))
    (check "release json carries the asset size"
           (eql 12345678 (getf release :asset-size))))
  (check "release without the zip asset is ignored"
         (null (parse-release-json
                "{\"tag_name\": \"v0.6.0\", \"assets\": [{\"name\": \"other.zip\", \"browser_download_url\": \"https://x/o.zip\"}]}")))
  (check "empty and malformed responses are ignored"
         (notany #'parse-release-json
                 (list "{}" "" "not json" "{\"assets\": []}")))
  ;; Downloaded zip verification: size and PK magic.
  (let ((path (merge-pathnames "eta-test-update.zip"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
             (write-sequence #(80 75 3 4 9 9 9 9) out))
           (check "zip check passes on matching size and magic"
                  (valid-update-zip-p path 8))
           (check "zip check passes without an expected size"
                  (valid-update-zip-p path nil))
           (check "zip check fails on a size mismatch"
                  (not (valid-update-zip-p path 7)))
           (with-open-file (out path :direction :output
                                     :if-exists :supersede
                                     :element-type '(unsigned-byte 8))
             (write-sequence #(60 104 116 109 108 62 10 10) out))
           (check "zip check fails on an html error page"
                  (not (valid-update-zip-p path 8))))
      (ignore-errors (delete-file path))))
  (check "zip check fails on a missing file"
         (not (valid-update-zip-p
               (merge-pathnames "eta-no-such-file.zip"
                                (uiop:temporary-directory))
               nil)))
  ;; The helper script: staging, verification, rollback, quoting. The
  ;; running exe carries a different name than the canonical one so the
  ;; name-agnostic side (the update always installs under
  ;; +CLIENT-EXE-NAME+) is pinned too.
  (let ((script (updater-script-text
                 :pid 4242
                 :exe-path "C:\\Program Files\\Rappy Runs\\OldName.exe"
                 :target-exe-path "C:\\Program Files\\Rappy Runs\\RappyRunsClient.exe"
                 :install-dir "C:\\Program Files\\Rappy Runs\\"
                 :zip-path "C:\\Temp\\RappyRunsClient-update.zip"
                 :stage-dir "C:\\Temp\\rappyruns-update-stage\\"
                 :log-path "C:\\Temp\\it's a log.txt")))
    (check "script waits for the old process"
           (and (search "Wait-Process -Id 4242" script)
                (search "Get-Process -Id 4242" script)))
    (check "script stages the zip before touching the install"
           (let ((expand (search "Expand-Archive" script))
                 (move (search "Move-Item -Force $exe $old" script)))
             (and expand move (< expand move))))
    (check "script verifies the staged exe"
           (search "no RappyRunsClient.exe in the update zip" script))
    (check "script installs under the canonical exe name"
           (and (search "$target = 'C:\\Program Files\\Rappy Runs\\RappyRunsClient.exe'"
                        script)
                (search "Copy-Item $newExe $target -Force" script)))
    (check "script rolls the .old exe back on failure"
           (search "Move-Item $old $exe" script))
    (check "a failed differing-name update drops the half-installed new exe"
           (let ((remove (search "Remove-Item -Force $target" script))
                 (rollback (search "Move-Item $old $exe" script)))
             (and remove rollback (< remove rollback))))
    (check "script restarts the new exe"
           (search "Start-Process -FilePath $target" script))
    (check "a failed update restarts the old exe"
           (search "Start-Process -FilePath $exe" script))
    (check "script updates the data folder"
           (search "Join-Path $stage 'data'" script))
    (check "script treats ffmpeg as best effort"
           (search "ffmpeg update skipped" script))
    (check "paths with spaces are single-quoted"
           (search "'C:\\Program Files\\Rappy Runs\\OldName.exe'"
                   script))
    (check "embedded quotes in paths are doubled"
           (search "'C:\\Temp\\it''s a log.txt'" script))
    (check "the running exe is never deleted, only moved"
           (not (search "Remove-Item -Force $exe" script)))))

;;; ------------------------------------------------------------------
;;; Config migration (dropped keys are scrubbed; everything else
;;; passes through)
;;; ------------------------------------------------------------------

(defun run-config-migration-tests ()
  (format t "~&--- config migration ---~%")
  (check "other keys survive the migration"
         (let ((migrated (ephinea-ta-client::migrate-config
                          (list :server-url "http://localhost:8080"
                                :api-token "eta_x"))))
           (and (equal "http://localhost:8080" (getf migrated :server-url))
                (equal "eta_x" (getf migrated :api-token)))))
  (check "the dropped token-prompt-shown key is scrubbed"
         (let ((migrated (ephinea-ta-client::migrate-config
                          (list :token-prompt-shown t :record-audio t))))
           (and (null (getf migrated :token-prompt-shown))
                (getf migrated :record-audio))))
  ;; The forced (hidden) settings are scrubbed on load so a stale saved
  ;; value can never override the fixed default.
  (check "forced config keys are scrubbed so the default wins"
         (let ((migrated (ephinea-ta-client::migrate-config
                          (list :completion-sound t :video-upload nil
                                :auto-submit nil))))
           (every (lambda (key) (null (getf migrated key)))
                  ephinea-ta-client::+forced-config-keys+)))
  ;; The default recordings folder rename (Videos/EphineaTA -> RappyRuns).
  (check "a fresh install uses the new recordings folder"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice nil nil)))
  (check "only the pre-rename folder present triggers the migration"
         (eq :migrate (ephinea-ta-client::default-record-dir-choice t nil)))
  (check "an already-migrated install stays on the new folder"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice nil t)))
  (check "both folders present never renames onto the existing one"
         (eq :use-new (ephinea-ta-client::default-record-dir-choice t t))))

