(in-package :ephinea-ta-client-tests)

;;; ------------------------------------------------------------------
;;; Recorder: capture-backend mock and state machine tests
;;; ------------------------------------------------------------------

(defclass mock-backend ()
  ((events :initform '() :accessor mock-events
           :documentation "Chronological list of side-effect events.")
   (alive :initform t :accessor mock-alive)
   (start-result :initform :ok :accessor mock-start-result)
   (remux-alive :initform nil :accessor mock-remux-alive
                :documentation "NIL: the remux exits as soon as spawned.")
   (remux-ok :initform t :accessor mock-remux-ok)
   (remux-start-result :initform :ok :accessor mock-remux-start-result)
   (stale :initform '() :accessor mock-stale)
   (recordings :initform '() :accessor mock-recordings
               :documentation "(namestring size write-date) triples.")
   (capture-monitor :initform nil :accessor mock-capture-monitor)))

(defun record-event (backend &rest event)
  (setf (mock-events backend)
        (append (mock-events backend) (list event))))

(defun events-of (backend kind)
  (remove kind (mock-events backend) :key #'first :test-not #'eq))

(defmethod backend-start-capture ((backend mock-backend) ffmpeg-path args
                                  output-path &key audio-pipe audio-pid
                                                   wgc-session)
  (declare (ignore wgc-session))
  (record-event backend :start ffmpeg-path args output-path
                audio-pipe audio-pid)
  (if (eq (mock-start-result backend) :ok)
      :mock-capture
      (values nil "mock start failure")))

(defmethod backend-capture-alive-p ((backend mock-backend) capture)
  (if (eq capture :mock-remux)
      (mock-remux-alive backend)
      (mock-alive backend)))

(defmethod backend-start-remux ((backend mock-backend) ffmpeg-path args)
  (record-event backend :remux ffmpeg-path args)
  (if (eq (mock-remux-start-result backend) :ok)
      :mock-remux
      (values nil "mock remux failure")))

(defmethod backend-capture-succeeded-p ((backend mock-backend) capture)
  (declare (ignore capture))
  (mock-remux-ok backend))

(defmethod backend-request-stop ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :stop))

(defmethod backend-kill-capture ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :kill))

(defmethod backend-close-capture ((backend mock-backend) capture)
  (declare (ignore capture))
  (record-event backend :close))

(defmethod backend-rename-file ((backend mock-backend) from to)
  (record-event backend :rename from to))

(defmethod backend-delete-file ((backend mock-backend) path)
  (record-event backend :delete path))

(defmethod backend-list-stale-files ((backend mock-backend) dir)
  (declare (ignore dir))
  (mock-stale backend))

(defmethod backend-list-recordings ((backend mock-backend) dir)
  (declare (ignore dir))
  (mock-recordings backend))

(defmethod backend-capture-monitor ((backend mock-backend))
  (mock-capture-monitor backend))

(defmacro with-recording-config ((&rest overrides) &body body)
  "Run BODY with an in-memory config; OVERRIDES are plist entries laid
over the defaults. Restores the global config afterwards (it is bound)."
  `(let ((ephinea-ta-client::*config*
           (append (list ,@overrides)
                   (copy-list ephinea-ta-client::*default-config*))))
     ,@body))

(defun make-test-run (&key (slug "ep1-test-quest") (time-ms 599123) aborted)
  (append (list :quest-slug slug
                :time-ms time-ms
                :finished-at (encode-universal-time 0 30 21 4 7 2026))
          (when aborted (list :aborted t))))

(defun make-test-recorder ()
  (let ((backend (make-instance 'mock-backend)))
    (values (make-recorder :backend backend) backend)))

(defun run-recorder-tests ()
  (format t "~&--- recorder ---~%")
  (with-recording-config (:record-enabled t)
    ;; Happy path: quest completes, video kept under the run's name.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "recording starts on :idle -> :in-quest"
             (eq (recorder-state rec) :recording))
      (let ((start (first (events-of backend :start))))
        (check "ffmpeg args capture the window title"
               (member "title=Ephinea PSOBB" (third start) :test #'equal))
        (check "ffmpeg writes to a rec-tmp file"
               (search "rec-tmp-" (fourth start))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      ;; The full clear completes and the detector flips to :idle on the
      ;; same frame; the run must still be credited to this capture.
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (check "stop is requested when the detector goes idle"
             (and (eq (recorder-state rec) :stopping)
                  (= 1 (length (events-of backend :stop)))))
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (let ((remux (first (events-of backend :remux))))
        (check "kept capture is remuxed with the moov up front"
               (and (eq (recorder-state rec) :remuxing)
                    remux
                    (member "+faststart" (third remux) :test #'equal)))
        (check "remux reads the tmp file"
               (and remux (search "rec-tmp-" (nth 4 (third remux)))))
        (check "remux writes the final name with quest, time and date"
               (and remux
                    (search "ep1-test-quest 9'59.123 (2026-07-04 2130).mp4"
                            (first (last (third remux)))))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "successful remux deletes the tmp and skips the rename"
             (let ((delete (first (events-of backend :delete))))
               (and delete
                    (search "rec-tmp-" (second delete))
                    (null (events-of backend :rename)))))
      (check "recorder returns to idle after finalize"
             (and (eq (recorder-state rec) :idle)
                  (= 2 (length (events-of backend :close))))))
    ;; Completed runs are stamped with the video offset (capture start
    ;; -> run start); the uploader forwards it as video_offset_ms so
    ;; telemetry seeks land on the right video time.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (declare (ignorable backend))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      ;; Pretend the capture has been running for 700 s.
      (setf (ephinea-ta-client::recorder-capture-start-real rec)
            (- (get-internal-real-time)
               (* 700 internal-time-units-per-second)))
      (let ((run (make-test-run :time-ms 599123)))
        (recorder-step rec :in-quest (list run) "Ephinea PSOBB")
        (check "completed runs carry the video offset of their start"
               (let ((offset (getf run :video-offset-ms)))
                 ;; 700s elapsed - 599.123s run = ~100.9s into the video
                 (and (integerp offset) (<= 100000 offset 102000)))))
      (let ((run (append (make-test-run :time-ms 1)
                         (list :video-offset-ms 42))))
        (recorder-step rec :in-quest (list run) "Ephinea PSOBB")
        (check "an existing video offset is left alone"
               (eql 42 (getf run :video-offset-ms)))))
    ;; Abandoned quest: no completed runs, file deleted.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "abandoned quest video is deleted"
             (and (= 1 (length (events-of backend :delete)))
                  (null (events-of backend :rename))
                  (eq (recorder-state rec) :idle))))
    ;; Segment completed, then the player leaves before the full clear.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep1-seg" :time-ms 120500))
                     "Ephinea PSOBB")
      (check "segment completion does not stop the capture"
             (eq (recorder-state rec) :recording))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "segment-only capture is kept under the segment name"
             (search "ep1-seg 2'00.500"
                     (first (last (third (first (events-of backend :remux))))))))
    ;; Full clear + segment: the longest run names the file.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep1-seg" :time-ms 120500))
                     "Ephinea PSOBB")
      (recorder-step rec :idle
                     (list (make-test-run :slug "ep1-full" :time-ms 599123))
                     "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "full clear (longest run) names the video"
             (search "ep1-full 9'59.123"
                     (first (last (third (first (events-of backend :remux))))))))
    ;; Completed segment inside an aborted quest (a GDV reset): the
    ;; segment is the only run that can take the video on the site, so
    ;; it must outrank the slightly longer aborted stay.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest
                     (list (make-test-run :slug "ep2-gdv-reset"
                                          :time-ms 145160))
                     "Ephinea PSOBB")
      (recorder-step rec :idle
                     (list (make-test-run :slug "ep2-gdv" :time-ms 148594
                                          :aborted t))
                     "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "a completed segment outranks a longer aborted run"
             (search "ep2-gdv-reset 2'25.160"
                     (first (last (third (first (events-of backend :remux))))))))
    ;; ffmpeg fails to start: error surfaced, retried on the NEXT quest.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-start-result backend) :fail)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start leaves the recorder idle with an error"
             (and (eq (recorder-state rec) :idle)
                  (recorder-last-error rec)))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start is not retried mid-quest"
             (= 1 (length (events-of backend :start))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "failed start is retried on the next quest"
             (= 2 (length (events-of backend :start)))))
    ;; ffmpeg dies mid-recording: cleanup + error, detection unaffected.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (check "ffmpeg dying mid-quest deletes the file and reports"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :delete)))
                  (recorder-last-error rec))))
    ;; "q" ignored: killed after the grace period, file still kept.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (ephinea-ta-client::recorder-stop-deadline rec)
            (1- (get-internal-real-time)))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "unresponsive ffmpeg is killed after the grace period"
             (= 1 (length (events-of backend :kill))))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "kill happens only once" (= 1 (length (events-of backend :kill))))
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "killed capture is still kept (remuxed from the fragments)"
             (= 1 (length (events-of backend :remux)))))
    ;; Remux fails: partial output dropped, fragmented original renamed.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-ok backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (let ((rename (first (events-of backend :rename))))
        (check "failed remux falls back to the fragmented original"
               (and rename
                    (search "rec-tmp-" (second rename))
                    (eq (recorder-state rec) :idle)))
        (check "failed remux drops its partial output first"
               (equal (third rename)
                      (second (first (events-of backend :delete)))))))
    ;; Remux cannot even start (ffmpeg gone): immediate rename fallback.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-start-result backend) :fail)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "unstartable remux falls back to a rename at once"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :rename))))))
    ;; Remux hangs: killed after the grace period, fallback still kept.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-remux-alive backend) t
            (mock-remux-ok backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "healthy remux is not killed"
             (null (events-of backend :kill)))
      (setf (ephinea-ta-client::recorder-remux-deadline rec)
            (1- (get-internal-real-time)))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "hung remux is killed after the grace period"
             (= 1 (length (events-of backend :kill))))
      (setf (mock-remux-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "killed remux still keeps the fragmented original"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :rename))))))
    ;; Shutdown mid-recording finishes capture and remux synchronously.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :in-quest (list (make-test-run)) "Ephinea PSOBB")
      (recorder-shutdown rec :timeout 0)
      (check "shutdown mid-recording kills and keeps the completed run"
             (and (eq (recorder-state rec) :idle)
                  (= 1 (length (events-of backend :kill)))
                  (= 1 (length (events-of backend :remux)))
                  (null (events-of backend :rename)))))
    ;; No window title (mock reader / not attached): no capture.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() nil)
      (check "no window title means no capture"
             (and (eq (recorder-state rec) :idle)
                  (null (mock-events backend)))))
    ;; Stale tmp files from a crashed session are removed at startup.
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-stale backend) '("a/rec-tmp-1.mp4" "a/rec-tmp-2.mp4"))
      (cleanup-stale-recordings rec)
      (check "stale recordings are deleted at startup"
             (equal '("a/rec-tmp-1.mp4" "a/rec-tmp-2.mp4")
                    (mapcar #'second (events-of backend :delete))))))
  ;; Recording disabled: the poll loop feeds frames but nothing happens.
  (with-recording-config (:record-enabled nil)
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "disabled recorder does nothing"
             (and (eq (recorder-state rec) :idle)
                  (null (mock-events backend))))))
  ;; Tracking-only mode suppresses recording even though :record-enabled
  ;; is a forced-on key.
  (with-recording-config (:record-enabled t :tracking-only t)
    (check "tracking-only mode turns recording off"
           (not (ephinea-ta-client::recording-enabled-p)))
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "tracking-only recorder never starts a capture"
             (and (eq (recorder-state rec) :idle)
                  (null (mock-events backend))))))
  (with-recording-config (:record-enabled t :tracking-only nil)
    (check "recording stays on with tracking-only off"
           (ephinea-ta-client::recording-enabled-p)))
  ;; Pure helpers.
  (check "sanitize-filename strips reserved characters"
         (string= "a-b-c-d" (sanitize-filename "a:b/c\"d")))
  (check "video filename prefers the in-game quest name"
         (search "Towards the Future 9'59.123"
                 (run-video-filename
                  (list :quest-slug "ep1-towards-the-future"
                        :quest-name "Towards the Future"
                        :time-ms 599123
                        :finished-at (encode-universal-time 0 30 21 4 7 2026)))))
  (check "best-session-run picks the longest run"
         (string= "long"
                  (getf (best-session-run
                         (list (list :quest-slug "short" :time-ms 10)
                               (list :quest-slug "long" :time-ms 20)))
                        :quest-slug)))
  (check "best-session-run prefers a completed run over a longer aborted one"
         (string= "seg"
                  (getf (best-session-run
                         (list (list :quest-slug "seg" :time-ms 10)
                               (list :quest-slug "abort" :time-ms 20
                                     :aborted t)))
                        :quest-slug)))
  (check "best-session-run falls back to the longest aborted run"
         (string= "ab2"
                  (getf (best-session-run
                         (list (list :quest-slug "ab1" :time-ms 10 :aborted t)
                               (list :quest-slug "ab2" :time-ms 20 :aborted t)))
                        :quest-slug)))
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4")))
    (check "ffmpeg args use fragmented mp4"
           (member "+frag_keyframe+empty_moov" args :test #'equal))
    (check "ffmpeg args set the poll framerate"
           (member "30" args :test #'equal))
    (check "video input probes minimally (A/V sync anchor)"
           (let ((probe (position "-probesize" args :test #'equal))
                 (grab (position "gdigrab" args :test #'equal)))
             (and probe grab
                  (equal "32" (nth (1+ probe) args))
                  (< probe grab))))
    (check "ffmpeg args encode at crf 29"
           (let ((crf (position "-crf" args :test #'equal)))
             (and crf (equal "29" (nth (1+ crf) args)))))
    (check "ffmpeg args cap the encoder threads (game shares the CPU)"
           (let ((threads (position "-threads" args :test #'equal)))
             (and threads
                  (equal (princ-to-string
                          (ephinea-ta-client::encoder-thread-count))
                         (nth (1+ threads) args)))))
    (check "ffmpeg args disable B-frames (zero-based video timestamps)"
           (let ((bf (position "-bf" args :test #'equal)))
             (and bf (equal "0" (nth (1+ bf) args)))))
    (check "ffmpeg args cap the height at 1080 without upscaling"
           (let ((vf (position "-vf" args :test #'equal)))
             (and vf
                  (search "scale=-2" (nth (1+ vf) args))
                  (search "min(1080" (nth (1+ vf) args)))))
    ;; The run-1368 color fix: the scale itself converts to YUV with an
    ;; explicit bt709 matrix (a YUV format right after it, or swscale's
    ;; untagged BT.601 default comes back), and setparams stamps the
    ;; rest of the VUI color metadata.
    (check "x264 args convert to yuv420p on the scale with bt709 tags"
           (let* ((vf (position "-vf" args :test #'equal))
                  (filter (and vf (nth (1+ vf) args))))
             (and filter
                  (search "out_color_matrix=bt709" filter)
                  (search "out_range=tv" filter)
                  (search ",format=yuv420p," filter)
                  (search "setparams=color_primaries=bt709:color_trc=iec61966-2-1"
                          filter)
                  (< (search "scale" filter)
                     (search ",format=yuv420p," filter))
                  (< (search ",format=yuv420p," filter)
                     (search "setparams=" filter)))))
    (check "ffmpeg output path is the last argument"
           (equal "out.mp4" (first (last args))))
    (check "video-only args carry no audio input"
           (not (member "s16le" args :test #'equal))))
  ;; Hardware-encoder argv: the GPU encoder replaces libx264, keeps
  ;; -bf 0 (the run-92 A/V sync lesson) and feeds it nv12 frames.
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                 :video-encoder "h264_amf")))
    (check "hw args use the requested encoder, not libx264"
           (and (member "h264_amf" args :test #'equal)
                (not (member "libx264" args :test #'equal))
                (not (member "-crf" args :test #'equal))))
    (check "hw args set the VBR bitrate target"
           (let ((rate (position "-b:v" args :test #'equal)))
             (and rate (equal ephinea-ta-client::+hw-record-bitrate+
                              (nth (1+ rate) args)))))
    (check "hw args still disable B-frames"
           (let ((bf (position "-bf" args :test #'equal)))
             (and bf (equal "0" (nth (1+ bf) args)))))
    (check "hw args convert to nv12 after the scale"
           (let ((vf (position "-vf" args :test #'equal)))
             (and vf
                  (let ((filter (nth (1+ vf) args)))
                    (and (search "scale" filter)
                         (search "format=nv12" filter)
                         (< (search "scale" filter)
                            (search "format=nv12" filter)))))))
    (check "hw fullscreen args keep the nv12 tail after hwdownload"
           (let* ((fs (build-ffmpeg-args :window-title "T"
                                         :output-path "out.mp4"
                                         :capture-monitor
                                         (list :output-idx 0
                                               :width 1920 :height 1080)
                                         :video-encoder "h264_nvenc"))
                  (vf (position "-vf" fs :test #'equal))
                  (filter (and vf (nth (1+ vf) fs))))
             (and filter
                  (search "hwdownload" filter)
                  (search "format=nv12" filter)
                  (< (search "hwdownload" filter)
                     (search "format=nv12" filter))))))
  (check "no video-encoder keeps the libx264 argv unchanged"
         (let ((args (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :video-encoder nil)))
           (and (member "libx264" args :test #'equal)
                (not (member "-b:v" args :test #'equal)))))
  (check "probe args test the encoder against the null muxer"
         (let ((args (ephinea-ta-client::hw-encoder-probe-args "h264_qsv")))
           (and (member "h264_qsv" args :test #'equal)
                (member "null" args :test #'equal)
                (equal "-" (first (last args))))))
  ;; The gdigrab window probe: a windowed ddagrab capture records
  ;; whatever overlaps the game, so a probe-verified gdigrab wins the
  ;; window back; black/failed/stale verdicts keep ddagrab (safe
  ;; direction).
  (let ((args (ephinea-ta-client::gdigrab-probe-args "My Game")))
    (check "gdigrab probe grabs the window through blackdetect into null"
           (and (member "gdigrab" args :test #'equal)
                (member "title=My Game" args :test #'equal)
                (find-if (lambda (arg) (search "blackdetect" arg)) args)
                (member "null" args :test #'equal)
                (equal "-" (first (last args)))))
    (check "gdigrab probe logs at info (blackdetect reports there)"
           (let ((level (position "-loglevel" args :test #'equal)))
             (and level (equal "info" (nth (1+ level) args)))))
    (check "gdigrab probe grabs a bounded frame count"
           (let ((frames (position "-frames:v" args :test #'equal)))
             (and frames
                  (equal (princ-to-string
                          ephinea-ta-client::+gdigrab-probe-frames+)
                         (nth (1+ frames) args))))))
  (check "a blackdetect report on stderr reads as black"
         (ephinea-ta-client::blackdetect-reports-black-p
          "[blackdetect @ 0x1] black_start:0 black_end:0.266 black_duration:0.266"))
  (check "stream chatter without a report reads as non-black"
         (not (ephinea-ta-client::blackdetect-reports-black-p
               "Input #0, gdigrab, from 'title=T': Stream #0:0: Video: bmp")))
  (check "missing stderr reads as non-black (the caller must fail it)"
         (not (ephinea-ta-client::blackdetect-reports-black-p nil)))
  (check "a matching usable verdict enables gdigrab"
         (ephinea-ta-client::gdigrab-verdict-usable-p
          '(:hwnd 42 :size (1280 960) :result :usable) 42 '(1280 960)))
  (check "an hwnd mismatch (window recreated) keeps ddagrab"
         (not (ephinea-ta-client::gdigrab-verdict-usable-p
               '(:hwnd 42 :size (1280 960) :result :usable) 43 '(1280 960))))
  (check "a size mismatch (display mode switch) keeps ddagrab"
         (not (ephinea-ta-client::gdigrab-verdict-usable-p
               '(:hwnd 42 :size (1280 960) :result :usable) 42 '(1920 1080))))
  (check "a black verdict keeps ddagrab"
         (not (ephinea-ta-client::gdigrab-verdict-usable-p
               '(:hwnd 42 :size (1280 960) :result :black) 42 '(1280 960))))
  (check "a failed verdict keeps ddagrab"
         (not (ephinea-ta-client::gdigrab-verdict-usable-p
               '(:hwnd 42 :size (1280 960) :result :failed) 42 '(1280 960))))
  (check "no verdict yet keeps ddagrab"
         (not (ephinea-ta-client::gdigrab-verdict-usable-p
               nil 42 '(1280 960))))
  ;; Secondary-adapter monitors (hybrid laptops, a display on the
  ;; second GPU): ddagrab needs its device created on that adapter
  ;; explicitly; adapter 0 must keep the probe-verified argv untouched.
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                 :capture-monitor
                                 '(:output-idx 1 :adapter 1
                                   :width 1920 :height 1080))))
    (check "secondary-adapter capture creates ddagrab's device explicitly"
           (let ((init (position "-init_hw_device" args :test #'equal)))
             (and init
                  (equal "d3d11va=dda:1" (nth (1+ init) args))
                  (equal "dda" (nth (1+ (position "-filter_hw_device" args
                                                  :test #'equal))
                                    args)))))
    (check "the device args precede the lavfi input"
           (< (position "-init_hw_device" args :test #'equal)
              (position "lavfi" args :test #'equal))))
  (check "adapter 0 leaves the ddagrab argv byte-for-byte unchanged"
         (equal (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                   :capture-monitor
                                   '(:output-idx 0 :adapter 0
                                     :width 1920 :height 1080))
                (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                   :capture-monitor
                                   '(:output-idx 0
                                     :width 1920 :height 1080))))
  (check "a legacy plist without :adapter creates no explicit device"
         (not (member "-init_hw_device"
                      (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                         :capture-monitor
                                         '(:output-idx 0
                                           :width 1920 :height 1080))
                      :test #'equal)))
  (check "a secondary adapter keeps hwdownload even with the QSV chain"
         (let* ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                         :capture-monitor
                                         '(:output-idx 0 :adapter 1
                                           :width 1920 :height 1080)
                                         :video-encoder "h264_qsv"
                                         :gpu-chain t))
                (vf (position "-vf" args :test #'equal))
                (filter (and vf (nth (1+ vf) args))))
           (and filter
                (search "hwdownload" filter)
                (not (search "hwmap" filter)))))
  ;; WGC window capture: the client feeds raw BGRA frames over a named
  ;; pipe (overlap-proof windowed path); the argv reads them as
  ;; rawvideo and crops the client area out of the whole-window frame.
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                 :wgc-capture
                                 '(:pipe "\\\\.\\pipe\\ephinea-ta-video"
                                   :width 1286 :height 993
                                   :crop (3 26 1280 960)))))
    (check "wgc argv reads raw bgra frames off the pipe"
           (let ((f (position "rawvideo" args :test #'equal)))
             (and f
                  (equal "bgra" (nth (1+ (position "-pixel_format" args
                                                   :test #'equal))
                                     args))
                  (equal "1286x993" (nth (1+ (position "-video_size" args
                                                       :test #'equal))
                                         args))
                  (member "\\\\.\\pipe\\ephinea-ta-video" args
                          :test #'equal))))
    (check "wgc argv crops the client area before the scale"
           (let* ((vf (position "-vf" args :test #'equal))
                  (filter (and vf (nth (1+ vf) args))))
             (and filter
                  (search "crop=1280:960:3:26" filter)
                  (< (search "crop=" filter) (search "scale" filter)))))
    (check "wgc argv never touches the GPU grab chains"
           (let* ((vf (position "-vf" args :test #'equal))
                  (filter (nth (1+ vf) args)))
             (and (not (member "gdigrab" args :test #'equal))
                  (not (search "ddagrab" (format nil "~{~a ~}" args)))
                  (not (search "hwdownload" filter))
                  (not (search "hwmap" filter)))))
    (check "wgc argv keeps the bt709 color tags"
           (let* ((vf (position "-vf" args :test #'equal))
                  (filter (nth (1+ vf) args)))
             (and (search "out_color_matrix=bt709" filter)
                  (search "setparams=" filter)))))
  (check "wgc without a crop records the whole frame"
         (let* ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                         :wgc-capture
                                         '(:pipe "p" :width 1280 :height 960)))
                (vf (position "-vf" args :test #'equal))
                (filter (nth (1+ vf) args)))
           (and (search "scale" filter)
                (not (search "crop=" filter)))))
  (check "wgc with a hw encoder converts to nv12"
         (let* ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                         :wgc-capture
                                         '(:pipe "p" :width 1280 :height 960)
                                         :video-encoder "h264_amf"))
                (vf (position "-vf" args :test #'equal))
                (filter (nth (1+ vf) args)))
           (and (member "h264_amf" args :test #'equal)
                (search ",format=nv12," filter))))
  ;; WGC-CROP-RECT: the client area inside the whole-window frame.
  (check "wgc crop offsets the client rect into the window"
         (equal '(8 31 1280 960)
                (ephinea-ta-client::wgc-crop-rect
                 '(108 131 1388 1091)   ; client on screen
                 '(100 100 1396 1099)   ; window on screen
                 1296 999)))
  (check "wgc crop is clamped to the frame and floored even"
         (equal '(8 31 1280 960)
                (ephinea-ta-client::wgc-crop-rect
                 '(108 131 1389 1092)   ; odd-sized client area
                 '(100 100 1396 1099)
                 1296 999)))
  (check "a degenerate client area yields no wgc crop"
         (null (ephinea-ta-client::wgc-crop-rect
                '(100 100 130 130) '(100 100 140 140) 40 40)))
  (check "the default adapter still gets the zero-copy QSV chain"
         (let* ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                         :capture-monitor
                                         '(:output-idx 0 :adapter 0
                                           :width 1920 :height 1080)
                                         :video-encoder "h264_qsv"
                                         :gpu-chain t))
                (vf (position "-vf" args :test #'equal))
                (filter (and vf (nth (1+ vf) args))))
           (and filter (search "hwmap" filter))))
  ;; The x264 thread cap: half the logical processors, floor 2, cap 8.
  (check "encoder threads: half the cores"
         (= 4 (ephinea-ta-client::encoder-thread-count 8)))
  (check "encoder threads floor at 2 on small machines"
         (= 2 (ephinea-ta-client::encoder-thread-count 2)))
  (check "encoder threads cap at +record-max-threads+"
         (= ephinea-ta-client::+record-max-threads+
            (ephinea-ta-client::encoder-thread-count 64)))
  (let ((args (build-remux-args "in.mp4" "out.mp4")))
    (check "remux args stream-copy the video with faststart"
           (and (member "copy" args :test #'equal)
                (member "+faststart" args :test #'equal)
                (not (member "libx264" args :test #'equal))))
    (check "remux args loudness-normalize the audio"
           (let ((af (position "-af" args :test #'equal)))
             (and af
                  (search "loudnorm" (nth (1+ af) args))
                  (member "aac" args :test #'equal))))
    (check "remux loudness target matches +record-loudness-lufs+ (issue 84)"
           (let ((af (position "-af" args :test #'equal)))
             (and af (search (format nil "loudnorm=I=~d:"
                                     ephinea-ta-client::+record-loudness-lufs+)
                             (nth (1+ af) args)))))
    (check "remux applies no timestamp correction (sync fixed at the source)"
           (let ((af (position "-af" args :test #'equal)))
             (and af
                  (not (search "atrim" (nth (1+ af) args)))
                  (not (member "-itsoffset" args :test #'equal)))))
    (check "remux reads the input and writes the output last"
           (and (member "in.mp4" args :test #'equal)
                (equal "out.mp4" (first (last args))))))
  ;; Audio arguments and their video-only fallback.
  (let* ((pipe (ephinea-ta-client::audio-pipe-name))
         (with-audio (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :audio-pipe pipe)))
    (check "audio args add the pipe input and aac"
           (and (member pipe with-audio :test #'equal)
                (member "aac" with-audio :test #'equal)))
    (check "live capture args carry no loudnorm (it throttles the video)"
           (notany (lambda (arg) (search "loudnorm" arg)) with-audio))
    (check "stripping audio args restores the video-only argv"
           (equal (build-ffmpeg-args :window-title "T" :output-path "out.mp4")
                  (ephinea-ta-client::strip-audio-args with-audio pipe)))
    (let ((retargeted (ephinea-ta-client::retarget-audio-args
                       with-audio :sample-format "f32le"
                       :rate 44100 :channels 2)))
      (check "retargeting rewrites the audio format tokens"
             (and (member "f32le" retargeted :test #'equal)
                  (member "44100" retargeted :test #'equal)
                  (not (member "s16le" retargeted :test #'equal))
                  (not (member "48000" retargeted :test #'equal))))
      (check "retargeting keeps the video tokens intact"
             (and (member "gdigrab" retargeted :test #'equal)
                  (member "30" retargeted :test #'equal)))))
  ;; Fullscreen capture: a game window covering its whole monitor is
  ;; grabbed with ddagrab (gdigrab records black over exclusive
  ;; fullscreen Direct3D).
  (check "a window spanning its monitor exactly is fullscreen"
         (ephinea-ta-client::rect-covers-p '(0 0 1920 1080)
                                           '(0 0 1920 1080)))
  (check "a fullscreen window on a secondary monitor is fullscreen"
         (ephinea-ta-client::rect-covers-p '(1920 0 3840 1080)
                                           '(1920 0 3840 1080)))
  (check "a maximized window (work area, above the taskbar) is not"
         (not (ephinea-ta-client::rect-covers-p '(0 0 1920 1032)
                                                '(0 0 1920 1080))))
  (check "an ordinary window is not fullscreen"
         (not (ephinea-ta-client::rect-covers-p '(100 100 1124 868)
                                                '(0 0 1920 1080))))
  (let ((args (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                 :capture-monitor
                                 (list :output-idx 1
                                       :width 1920 :height 1080))))
    (check "fullscreen args capture via ddagrab, not gdigrab"
           (and (not (member "gdigrab" args :test #'equal))
                (member "lavfi" args :test #'equal)
                (find-if (lambda (arg)
                           (search "ddagrab=output_idx=1" arg))
                         args)))
    (check "fullscreen args keep the framerate and hide the mouse"
           (find-if (lambda (arg)
                      (and (search "framerate=30" arg)
                           (search "draw_mouse=0" arg)))
                    args))
    (check "fullscreen args download GPU frames before the scale cap"
           (let ((vf (position "-vf" args :test #'equal)))
             (and vf
                  (let ((filter (nth (1+ vf) args)))
                    (and (search "hwdownload" filter)
                         (search "min(1080" filter)
                         (< (search "hwdownload" filter)
                            (search "scale" filter)))))))
    (check "stripping audio args restores the fullscreen video-only argv"
           (let ((pipe (ephinea-ta-client::audio-pipe-name)))
             (equal args
                    (ephinea-ta-client::strip-audio-args
                     (build-ffmpeg-args :window-title "T"
                                        :output-path "out.mp4"
                                        :audio-pipe pipe
                                        :capture-monitor
                                        (list :output-idx 1
                                              :width 1920 :height 1080))
                     pipe)))))
  ;; The zero-copy Intel chain: with the probe-verified GPU chain the
  ;; frames never touch system memory - hwmap hands ddagrab's D3D11
  ;; frames to vpp_qsv, sized from the monitor rect (2560x1600 Retina
  ;; -> 1728x1080), and no hwdownload appears.
  (let* ((monitor (list :output-idx 0 :width 2560 :height 1600))
         (args (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                  :capture-monitor monitor
                                  :video-encoder "h264_qsv"
                                  :gpu-chain t))
         (vf (position "-vf" args :test #'equal))
         (filter (and vf (nth (1+ vf) args))))
    (check "qsv chain keeps every frame on the GPU"
           (and filter
                (search "hwmap=derive_device=qsv" filter)
                (not (search "hwdownload" filter))))
    (check "qsv chain scales on the GPU to literal capped dimensions"
           (and filter
                (search "vpp_qsv=w=1728:h=1080:format=nv12" filter)))
    (check "qsv chain converts with the explicit bt709 matrix and tags"
           (and filter
                (search "out_color_matrix=bt709:out_range=tv" filter)
                (search "setparams=color_primaries=bt709:color_trc=iec61966-2-1"
                        filter)))
    (check "qsv chain still encodes with h264_qsv at the VBR target"
           (and (member "h264_qsv" args :test #'equal)
                (member "-b:v" args :test #'equal)))
    (check "qsv chain still disables B-frames"
           (let ((bf (position "-bf" args :test #'equal)))
             (and bf (equal "0" (nth (1+ bf) args)))))
    (check "an unverified chain keeps the hwdownload fallback"
           (let* ((fallback (build-ffmpeg-args :window-title "T"
                                               :output-path "out.mp4"
                                               :capture-monitor monitor
                                               :video-encoder "h264_qsv"))
                  (vf (position "-vf" fallback :test #'equal))
                  (filter (nth (1+ vf) fallback)))
             (and (search "hwdownload" filter)
                  (not (search "hwmap" filter))))))
  (check "the chain flag never rewires a monitor-less capture"
         (equal (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                   :video-encoder "h264_qsv" :gpu-chain t)
                (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                   :video-encoder "h264_qsv")))
  ;; Windowed captures: the monitor frame cropped to the client area
  ;; (GDI cannot read a flip-model-composited window - run 949's
  ;; all-black recordings - so ddagrab covers windowed games too).
  (check "crop rect: a window inside its monitor maps to monitor coords"
         (multiple-value-bind (x y w h)
             (ephinea-ta-client::capture-crop-rect
              '(160 90 1760 990) '(0 0 1920 1080))
           (and (= x 160) (= y 90) (= w 1600) (= h 900))))
  (check "crop rect: secondary-monitor origins subtract away"
         (multiple-value-bind (x y w h)
             (ephinea-ta-client::capture-crop-rect
              '(2080 90 3680 990) '(1920 0 3840 1080))
           (and (= x 160) (= y 90) (= w 1600) (= h 900))))
  (check "crop rect clamps a half-dragged-off window to the monitor"
         (multiple-value-bind (x y w h)
             (ephinea-ta-client::capture-crop-rect
              '(-100 -50 924 718) '(0 0 1920 1080))
           (and (= x 0) (= y 0) (= w 924) (= h 718))))
  (check "crop rect floors odd sizes to even"
         (multiple-value-bind (x y w h)
             (ephinea-ta-client::capture-crop-rect
              '(100 100 1123 867) '(0 0 1920 1080))
           (and (= x 100) (= y 100) (= w 1022) (= h 766))))
  (check "crop rect rejects a sliver (minimized/degenerate window)"
         (null (ephinea-ta-client::capture-crop-rect
                '(0 0 32 32) '(0 0 1920 1080))))
  (let* ((monitor (list :output-idx 1 :width 1920 :height 1080
                        :crop (list 160 90 1600 900)))
         (args (build-ffmpeg-args :window-title "T" :output-path "out.mp4"
                                  :capture-monitor monitor))
         (vf (position "-vf" args :test #'equal))
         (filter (and vf (nth (1+ vf) args))))
    (check "windowed capture rides ddagrab with a crop before the scale"
           (and (not (member "gdigrab" args :test #'equal))
                (find-if (lambda (arg) (search "ddagrab=output_idx=1" arg))
                         args)
                filter
                (search "crop=1600:900:160:90" filter)
                (< (search "hwdownload" filter) (search "crop=" filter))
                (< (search "crop=" filter) (search "scale" filter))))
    (check "a crop suppresses the zero-copy chain (no crop_qsv here)"
           (let* ((qsv (build-ffmpeg-args :window-title "T"
                                          :output-path "o.mp4"
                                          :capture-monitor monitor
                                          :video-encoder "h264_qsv"
                                          :gpu-chain t))
                  (vf (position "-vf" qsv :test #'equal))
                  (filter (nth (1+ vf) qsv)))
             (and (search "hwdownload" filter)
                  (search "crop=1600:900:160:90" filter)
                  (not (search "hwmap" filter))
                  (search "format=nv12" filter)))))
  ;; The GPU scale's literal dimensions: 1080-capped, aspect kept, even.
  (check "scale dimensions cap at 1080 keeping aspect"
         (multiple-value-bind (w h)
             (ephinea-ta-client::record-scale-dimensions 3200 1800)
           (and (= w 1920) (= h 1080))))
  (check "scale dimensions leave small sources alone"
         (multiple-value-bind (w h)
             (ephinea-ta-client::record-scale-dimensions 1440 900)
           (and (= w 1440) (= h 900))))
  (check "scale dimensions stay even"
         (multiple-value-bind (w h)
             (ephinea-ta-client::record-scale-dimensions 1367 899)
           (and (= w 1366) (= h 898))))
  (check "gpu chain probe args exercise ddagrab through h264_qsv"
         (let ((args (ephinea-ta-client::hw-gpu-chain-probe-args)))
           (and (find-if (lambda (arg) (search "ddagrab" arg)) args)
                (find-if (lambda (arg) (search "hwmap" arg)) args)
                (member "h264_qsv" args :test #'equal)
                (member "null" args :test #'equal)
                (equal "-" (first (last args))))))
  (check "gpu chain probe exercises the capture chain's color options"
         (find-if (lambda (arg)
                    (search "vpp_qsv=w=1280:h=720:format=nv12:out_color_matrix=bt709:out_range=tv"
                            arg))
                  (ephinea-ta-client::hw-gpu-chain-probe-args)))
  ;; The low-memory profile: an 8 GB machine records at a lower rate so
  ;; the capture tmp + remux + upload churn less of its file cache.
  (check "8 GB machines are low-memory, 16 GB and unknown are not"
         (and (ephinea-ta-client::low-memory-machine-p (* 8 (expt 2 30)))
              (not (ephinea-ta-client::low-memory-machine-p (* 16 (expt 2 30))))
              (not (ephinea-ta-client::low-memory-machine-p nil))))
  (check "low-memory hw args use the reduced VBR profile"
         (let ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                        :video-encoder "h264_qsv"
                                        :low-memory t)))
           (let ((rate (position "-b:v" args :test #'equal)))
             (and rate (equal ephinea-ta-client::+hw-record-bitrate-low+
                              (nth (1+ rate) args))
                  (member ephinea-ta-client::+hw-record-maxrate-low+
                          args :test #'equal)))))
  (check "low-memory x264 args raise the CRF"
         (let ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                        :low-memory t)))
           (let ((crf (position "-crf" args :test #'equal)))
             (and crf (equal (princ-to-string
                              ephinea-ta-client::+record-crf-low+)
                             (nth (1+ crf) args))))))
  (check "roomy machines keep the standard rates"
         (let ((args (build-ffmpeg-args :window-title "T" :output-path "o.mp4"
                                        :video-encoder "h264_qsv")))
           (member ephinea-ta-client::+hw-record-bitrate+ args :test #'equal)))
  ;; The recorder asks the backend for the capture monitor at start.
  (with-recording-config (:record-enabled t)
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-capture-monitor backend)
            (list :output-idx 0 :width 1920 :height 1080))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (let ((args (third (first (events-of backend :start)))))
        (check "recorder records a fullscreen game via ddagrab"
               (find-if (lambda (arg) (search "ddagrab=output_idx=0" arg))
                        args))))
    (multiple-value-bind (rec backend) (make-test-recorder)
      (setf (mock-capture-monitor backend)
            (list :output-idx 1 :width 1920 :height 1080
                  :crop (list 152 90 1600 900)))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (let ((args (third (first (events-of backend :start)))))
        (check "recorder records a windowed game via ddagrab + crop"
               (and (find-if (lambda (arg)
                               (search "ddagrab=output_idx=1" arg))
                             args)
                    (find-if (lambda (arg)
                               (search "crop=1600:900:152:90" arg))
                             args)))))
    (multiple-value-bind (rec backend) (make-test-recorder)
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (let ((args (third (first (events-of backend :start)))))
        (check "recorder falls back to gdigrab without a capture monitor"
               (member "gdigrab" args :test #'equal)))))
  ;; The recorder passes the audio pipe and target pid to the backend.
  (with-recording-config (:record-enabled t :record-audio t)
    (let ((ephinea-ta-client::*audio-target-pid* 1234))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (let ((start (first (events-of backend :start))))
          (check "recorder hands the backend the audio pipe and pid"
                 (and (equal (fifth start) (ephinea-ta-client::audio-pipe-name))
                      (eql (sixth start) 1234))))))
    (let ((ephinea-ta-client::*audio-target-pid* nil))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (check "no attached game pid means no audio pipe"
               (null (fifth (first (events-of backend :start))))))))
  (with-recording-config (:record-enabled t :record-audio nil)
    (let ((ephinea-ta-client::*audio-target-pid* 1234))
      (multiple-value-bind (rec backend) (make-test-recorder)
        (recorder-step rec :in-quest '() "Ephinea PSOBB")
        (check "audio can be disabled in config"
               (null (fifth (first (events-of backend :start)))))))))

;;; ------------------------------------------------------------------
;;; Video attach flow: recordings linked to queue entries
;;; ------------------------------------------------------------------

(defmacro with-test-store ((&rest initial-runs) &body body)
  "Run BODY against a private *RUNS* list and a throwaway queue file, so
store functions that persist never touch the real %APPDATA% queue."
  `(let ((ephinea-ta-client::*runs* (list ,@initial-runs))
         (ephinea-ta-client::*queue-path*
           (merge-pathnames (format nil "eta-test-queue-~d.sexp"
                                    (get-internal-real-time))
                            (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ephinea-ta-client::*queue-path*)))))

(defun run-video-flow-tests ()
  (format t "~&--- video attach flow ---~%")
  ;; Recorder: ON-KEEP fires exactly when a video is saved.
  (with-recording-config (:record-enabled t)
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "on-keep waits for the remux" (null kept))
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "on-keep is called once with the final path and best run"
             (and (= 1 (length kept))
                  (search "9'59.123" (first (first kept)))
                  (equal "ep1-test-quest"
                         (getf (second (first kept)) :quest-slug)))))
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB") ; abandoned
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "on-keep is not called for abandoned captures" (null kept)))
    (let* ((kept '())
           (backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (push (list path run) kept)))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :in-quest '() "Ephinea PSOBB") ; ffmpeg died
      (check "on-keep is not called when the capture aborts" (null kept)))
    (let* ((backend (make-instance 'mock-backend))
           (rec (make-recorder :backend backend
                               :on-keep (lambda (path run)
                                          (declare (ignore path run))
                                          (error "callback boom")))))
      (recorder-step rec :in-quest '() "Ephinea PSOBB")
      (recorder-step rec :idle (list (make-test-run)) "Ephinea PSOBB")
      (setf (mock-alive backend) nil)
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (recorder-step rec :idle '() "Ephinea PSOBB")
      (check "an erroring on-keep neither sticks nor reports"
             (and (eq (recorder-state rec) :idle)
                  (null (recorder-last-error rec))))))
  ;; Submission updates carry the server id for later video attachment.
  (let ((payload (make-hash-table :test 'equal)))
    (setf (gethash "id" payload) 42
          (gethash "url" payload) "https://x/runs/42")
    (check "created runs remember their server id"
           (equal '(:status :submitted :url "https://x/runs/42" :server-id 42)
                  (ephinea-ta-client::submission-updates :created payload)))
    (check "duplicate runs remember their server id too"
           (eql 42 (getf (ephinea-ta-client::submission-updates
                          :duplicate payload)
                         :server-id))))
  ;; A fresh submission carries the board standing for the reward hint.
  (let ((payload (make-hash-table :test 'equal))
        (standing (make-hash-table :test 'equal)))
    (setf (gethash "id" payload) 7
          (gethash "rank" standing) 2
          (gethash "parties" standing) 5
          (gethash "previous_best_ms" standing) 605000
          (gethash "delta_ms" standing) -3210
          (gethash "standing" payload) standing)
    (let ((updates (ephinea-ta-client::submission-updates :created payload)))
      (check "created runs carry the board rank and party count"
             (and (eql 2 (getf updates :standing-rank))
                  (eql 5 (getf updates :standing-parties))))
      (check "created runs carry the personal-best delta and previous time"
             (and (eql -3210 (getf updates :standing-delta-ms))
                  (eql 605000 (getf updates :standing-prev-ms))))))
  (check "a submission with no standing block adds no standing keys"
         (let ((payload (make-hash-table :test 'equal)))
           (setf (gethash "id" payload) 7)
           (null (getf (ephinea-ta-client::submission-updates :created payload)
                       :standing-rank))))
  (check "a first-time standing carries a rank but no delta"
         (let ((payload (make-hash-table :test 'equal))
               (standing (make-hash-table :test 'equal)))
           (setf (gethash "rank" standing) 1
                 (gethash "parties" standing) 1
                 (gethash "standing" payload) standing)
           (let ((updates (ephinea-ta-client::submission-updates :created payload)))
             (and (eql 1 (getf updates :standing-rank))
                  (null (getf updates :standing-delta-ms))))))
  (check "rejected runs carry a reason, not a server id"
         (let ((payload (make-hash-table :test 'equal)))
           (setf (gethash "message" payload) "nope")
           (let ((updates (ephinea-ta-client::submission-updates
                           :rejected payload)))
             (and (null (getf updates :server-id))
                  (search "nope" (getf updates :reason))))))
  ;; Linking a saved video to its (copy-replaced) queue entry.
  (with-test-store ()
    (let* ((run (make-test-run))
           (entry (enqueue-run! run)))
      (ephinea-ta-client::update-run! entry :status :submitted :server-id 7)
      (let ((linked (ephinea-ta-client::link-video-file! run "C:/v/run.mp4")))
        (check "link-video-file! matches by natural key after updates"
               (and linked (search "run.mp4" (getf linked :video-path))))
        (check "linked entry still carries its server id"
               (eql 7 (getf linked :server-id))))
      (check "link-video-file! returns NIL for unknown runs"
             (null (ephinea-ta-client::link-video-file!
                    (make-test-run :slug "ep1-other") "C:/v/x.mp4")))))
  ;; Active entries survive trimming and restarts; attached ones do not.
  (let* ((unattached (list :status :submitted :server-id 1 :video-path "v.mp4"))
         (attached (list :status :submitted :server-id 2 :video-path "w.mp4"
                         :video-attached t))
         ;; Newest first; the attached entry is the oldest of 61 finished.
         (runs (cons unattached
                     (append (loop :for i :below 60
                                   :collect (list :status :submitted :n i))
                             (list attached))))
         (trimmed (ephinea-ta-client::trim-finished-runs runs 50)))
    (check "unattached video survives the finished-run cap"
           (member unattached trimmed))
    (check "attached video counts as finished and trims away"
           (not (member attached trimmed))))
  (check "attached entries are not active"
         (not (ephinea-ta-client::entry-active-p
               (list :status :submitted :server-id 2 :video-path "w.mp4"
                     :video-attached t))))
  (check "rejected runs without a server id are not kept for video"
         (not (ephinea-ta-client::entry-active-p
               (list :status :rejected :video-path "v.mp4"))))
  (with-test-store ((list :status :submitted :server-id 1 :video-path "v.mp4"
                          :telemetry '(:frames ()))
                    (list :status :queued :telemetry '(:frames ())))
    (ephinea-ta-client::save-queue!)
    (let ((saved (ephinea-ta-client::read-sexp-file
                  ephinea-ta-client::*queue-path*)))
      (check "queue file keeps both active entries" (= 2 (length saved)))
      (check "persisted video entry drops its telemetry"
             (null (getf (first saved) :telemetry)))
      (check "persisted queued entry keeps its telemetry"
             (getf (second saved) :telemetry))))
  ;; The in-memory copy sheds a finished submission's telemetry too:
  ;; megabytes of frames per long run, times ~50 kept entries, once
  ;; grew the heap a lap's worth every lap (the 8 GB machine felt it).
  (with-test-store ((list :status :queued :telemetry '(:frames (1))))
    (let* ((entry (first (queued-runs)))
           (submitted (ephinea-ta-client::update-run!
                       entry :status :submitted :server-id 9)))
      (check "a submitted entry releases its telemetry in memory"
             (and (null (getf submitted :telemetry))
                  (null (getf (first (queued-runs)) :telemetry))))))
  (with-test-store ((list :status :queued :telemetry '(:frames (1))))
    (let ((failed (ephinea-ta-client::update-run!
                   (first (queued-runs)) :status :failed :reason "net")))
      (check "a failed entry keeps its telemetry for the retry"
             (getf failed :telemetry))))
  ;; Clearing the list keeps only unsent runs - the one thing that
  ;; exists nowhere else. Drafts with a pending video go too (their
  ;; recording and server draft survive elsewhere), so recordings the
  ;; user never means to upload cannot haunt the list forever.
  (with-test-store ((list :status :submitted :server-id 1)
                    (list :status :queued)
                    (list :status :failed :reason "boom")
                    (list :status :submitted :server-id 2 :video-path "v.mp4")
                    (list :status :submitted :server-id 3 :video-path "w.mp4"
                          :video-attached t)
                    (list :status :duplicate :server-id 4)
                    (list :status :rejected :reason "nope"))
    (check "clear-runs! reports the removed count"
           (= 5 (ephinea-ta-client::clear-runs!)))
    (check "clear keeps only queued and failed entries, in order"
           (equal '(:queued :failed)
                  (mapcar (lambda (entry) (getf entry :status))
                          (queued-runs))))
    (check "clear persists the surviving queue"
           (= 2 (length (ephinea-ta-client::read-sexp-file
                         ephinea-ta-client::*queue-path*)))))
  ;; Labels for the new Video column and statuses.
  (check "video label: saved recording"
         (equal "saved" (ephinea-ta-client::run-video-label
                         (list :video-path "v.mp4"))))
  (check "video label: attached"
         (equal "attached" (ephinea-ta-client::run-video-label
                            (list :video-path "v.mp4" :video-attached t))))
  (check "video label: no recording"
         (equal "" (ephinea-ta-client::run-video-label (list :status :queued))))
  (check "status label: saved video announces the automatic upload"
         (with-recording-config (:video-upload t)
           (search "automatically"
                   (ephinea-ta-client::run-status-label
                    (list :status :submitted :video-path "v.mp4")))))
  (check "status label: saved video points at the Upload button when auto-upload is off"
         (with-recording-config (:video-upload nil)
           (search "Upload" (ephinea-ta-client::run-status-label
                             (list :status :submitted :video-path "v.mp4")))))
  (check "status label: a given-up upload points back at the Upload button"
         (with-recording-config (:video-upload t)
           (search "Upload" (ephinea-ta-client::run-status-label
                             (list :status :submitted :video-path "v.mp4"
                                   :upload-given-up t)))))
  (check "status label: attached video says awaiting review"
         (search "awaiting review"
                 (ephinea-ta-client::run-status-label
                  (list :status :submitted :video-attached t))))
  ;; A hosted upload lands 'held' server-side (issue 105): its label
  ;; must point at the browser publish step, not promise a review.
  (check "status label: a held upload points at the browser, not review"
         (let ((label (ephinea-ta-client::run-status-label
                       (list :status :submitted :video-attached t :held t))))
           (and (search "publish" label)
                (not (search "awaiting review" label)))))
  ;; A :duplicate reply can report the run already approved (issue 100),
  ;; so its attached label reports approval, never "awaiting review".
  (check "status label: an approved attached video is not awaiting review"
         (let ((label (ephinea-ta-client::run-status-label
                       (list :status :submitted :video-attached t :approved t))))
           (and (search "approved" label)
                (not (search "awaiting review" label)))))
  ;; An aborted run's link never enters review, so its label must not
  ;; promise one - it just reports the attached, private video.
  (check "status label: an aborted attached video is not awaiting review"
         (let ((label (ephinea-ta-client::run-status-label
                       (list :status :submitted :video-attached t :aborted t))))
           (and (search "aborted" label)
                (not (search "awaiting review" label))))))

