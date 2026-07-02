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
           ;; quests
           #:load-quest-defs #:find-quest-def
           #:quest-def-slug #:quest-def-episode #:quest-def-names
           #:quest-def-number #:quest-def-start #:quest-def-end
           ;; detect
           #:make-detector #:detector-step #:detector-state
           #:detector-quest-def #:detector-elapsed-ms
           ;; api
           #:fetch-quests #:submit-run
           ;; store
           #:enqueue-run! #:submit-queued! #:queued-runs))
