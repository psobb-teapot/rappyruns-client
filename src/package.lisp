(defpackage :ephinea-ta-client
  (:use :cl)
  (:nicknames :eta-client)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export #:main
           #:run-headless
           ;; config
           #:load-config! #:save-config! #:config-value
           ;; memory protocol (used by tests)
           #:read-block #:mock-reader #:make-mock-reader
           #:read-u8 #:read-u16 #:read-u32 #:read-f32 #:read-utf16-string
           ;; psobb
           #:read-snapshot #:snapshot-register-set-p #:snapshot-floor-switch-set-p
           #:read-player #:read-inventory #:read-monsters
           #:snapshot-my-player #:difficulty-name #:tech-name #:shifta-level
           ;; telemetry
           #:make-telemetry #:telemetry-step #:telemetry-run-data
           #:telemetry-death-count #:telemetry-kills #:telemetry-frames
           ;; quests
           #:load-quest-defs #:find-quest-def #:find-quest-defs
           #:set-server-quest-defs
           #:quest-def-slug #:quest-def-episode #:quest-def-names
           #:quest-def-number #:quest-def-start #:quest-def-end
           ;; detect
           #:make-detector #:detector-step #:detector-state
           #:detector-active-def #:detector-active-count
           #:detector-elapsed-ms
           ;; recording (backend generics are specialized by tests and
           ;; by the live ffmpeg backend)
           #:make-recorder #:recorder-step #:recorder-shutdown
           #:recorder-state #:recorder-last-error
           #:cleanup-stale-recordings
           #:build-ffmpeg-args #:run-video-filename #:sanitize-filename
           #:best-session-run #:reader-window-title
           #:backend-start-capture #:backend-capture-alive-p
           #:backend-request-stop #:backend-kill-capture
           #:backend-close-capture #:backend-rename-file
           #:backend-delete-file #:backend-list-stale-files
           ;; trigger discovery
           #:log-trigger-changes #:start-trigger-log #:close-trigger-log
           ;; api
           #:fetch-quests #:submit-run
           ;; store
           #:enqueue-run! #:submit-queued! #:queued-runs))
