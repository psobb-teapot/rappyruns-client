;;; Desktop companion client for ephinea-ta. Reads the Ephinea PSOBB
;;; process memory, detects quest completions and auto-submits them as
;;; drafts via the site's JSON API (see src/api.lisp server-side).
;;;
;;; The GUI and live memory reading require LispWorks (CAPI + FLI); the
;;; pure-CL core (trigger definitions, detection state machine, API
;;; client) also loads on SBCL for testing.
(defsystem "ephinea-ta-client"
  :description "Auto-submitting time attack client for ephinea-ta"
  :author "teapot"
  :license "MIT"
  :depends-on ("com.inuoe.jzon")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "version")
                             (:file "i18n")
                             (:file "config")
                             (:file "credentials")
                             (:file "memory")
                             (:file "win32" :if-feature :lispworks)
                             (:file "winhttp" :if-feature :lispworks)
                             (:file "psobb")
                             (:file "quests")
                             (:file "telemetry")
                             (:file "detect")
                             (:file "recording")
                             (:file "audio-win32" :if-feature :lispworks)
                             (:file "wgc-win32" :if-feature :lispworks)
                             (:file "ffmpeg-win32" :if-feature :lispworks)
                             (:file "trigger-log")
                             (:file "api-client")
                             ;; Pure logic of the GUI's quest-rule form,
                             ;; portable so client-tests can reach it.
                             (:file "rule-form")
                             (:file "updater")
                             (:file "store")
                             ;; After store (format-run-time), before gui
                             ;; (the status-line suffix).
                             (:file "ghost")
                             (:file "gui" :if-feature :lispworks)
                             (:file "tray-win32" :if-feature :lispworks)
                             ;; After tray-win32: shares its window-class /
                             ;; message-loop FLI bindings.
                             (:file "overlay-win32" :if-feature :lispworks)
                             (:file "autostart-win32" :if-feature :lispworks)
                             (:file "main")))))
