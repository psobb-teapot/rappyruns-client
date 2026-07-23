(in-package :ephinea-ta-client)

;;; Self-update via the public releases repository on GitHub.
;;;
;;; The flow (gui.lisp drives it: STARTUP-AUTO-UPDATE before the main
;;; window exists, CHECK-FOR-UPDATES behind the Settings button): fetch
;;; /releases/latest, compare tags against the baked-in *CLIENT-VERSION*
;;; (version.lisp), download the zip to %TEMP%, then hand over to a
;;; PowerShell helper script that waits for this process to exit, swaps
;;; the exe (staging the zip first, with an .old rollback) and restarts.
;;; The pure-CL parts - release JSON parsing, zip sanity checks and the
;;; helper script text - load on SBCL and are covered by the tests.

(defparameter *update-repo* "psobb-teapot/rappyruns-client"
  "owner/name of the public repository whose releases carry the client -
since 2026-07-24 the same repo also carries the client source. Renamed
from rappyruns-client-releases (2026-07-24) and ephinea-ta-client-releases
(2026-07-06); GitHub redirects both old names, so clients built before
each rename keep updating. Those names must never be reused for new
repositories or the redirects break.")

(defparameter +update-asset-name+ "RappyRunsClient.zip"
  "The release asset the site's download button also points at.")

(defparameter +client-exe-name+ "RappyRunsClient.exe"
  "The canonical exe name. The helper script always installs the update
under this name, whatever the running exe is called.")

(defun resolve-update-repo ()
  "A :update-repo config override (a power-user/testing key, like
:ffmpeg-path) or the real releases repository."
  (let ((configured (config-value :update-repo)))
    (if (and (stringp configured) (plusp (length configured)))
        configured
        *update-repo*)))

(defun update-release-page-url ()
  (format nil "https://github.com/~a/releases/latest" (resolve-update-repo)))

(defun parse-release-json (body)
  "GitHub /releases/latest response body -> (:tag \"v0.6.0\" :asset-url
... :asset-size ...), or NIL when the tag or the client zip asset is
missing. The download URL comes from the same response as the tag, so a
release published between check and download cannot mismatch them."
  (let ((release (ignore-errors (jzon:parse body))))
    (when (hash-table-p release)
      (let ((tag (gethash "tag_name" release))
            (assets (gethash "assets" release)))
        (when (and (stringp tag) (vectorp assets))
          (loop :for asset :across assets
                :when (and (hash-table-p asset)
                           (equal (gethash "name" asset) +update-asset-name+))
                  :return (let ((url (gethash "browser_download_url" asset))
                                (size (gethash "size" asset)))
                            (when (stringp url)
                              (list :tag tag
                                    :asset-url url
                                    :asset-size (and (integerp size)
                                                     (plusp size)
                                                     size))))))))))

(defun startup-update-decision (release current-version writable-p)
  "What the pre-GUI update pass (STARTUP-AUTO-UPDATE in gui.lisp) should
do: :APPLY (download and hand over before any window shows), :UP-TO-DATE,
:NOT-WRITABLE (the main window offers the manual download instead) or
:CHECK-FAILED. Pure so the tests can pin the boundaries."
  (cond ((null release) :check-failed)
        ((not (update-available-p current-version (getf release :tag)))
         :up-to-date)
        ((not writable-p) :not-writable)
        (t :apply)))

(defun valid-update-zip-p (path expected-size)
  "Cheap corruption check on a downloaded zip: the byte size the API
promised (when it supplied one) and the PK local-file-header magic."
  (with-open-file (in path :element-type '(unsigned-byte 8)
                           :if-does-not-exist nil)
    (and in
         (or (null expected-size) (eql (file-length in) expected-size))
         (let ((magic (make-array 4 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
           (and (= (read-sequence magic in) 4)
                (equalp magic #(80 75 3 4)))))))   ; "PK\3\4"

(defun ps-quote (string)
  "STRING as a single-quoted PowerShell literal (no interpolation;
embedded quotes doubled), so paths with spaces survive."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop :for char :across string
          :do (when (char= char #\') (write-char #\' out))
              (write-char char out))
    (write-char #\' out)))

(defun updater-script-text (&key pid exe-path target-exe-path install-dir
                                 zip-path stage-dir log-path)
  "The helper script that performs the actual swap, as a string (pure,
so the tests can check its shape). It stages the zip and verifies the
new exe BEFORE touching the install, so a bad download can never break
the existing client; any later failure rolls the .old exe back.
TARGET-EXE-PATH is where the new exe lands (+CLIENT-EXE-NAME+ in the
install dir): when the running exe still carries the pre-rename name,
the update doubles as the rename migration."
  (format nil "$ErrorActionPreference = 'Stop'~%~
Start-Transcript -Path ~a -Force | Out-Null~%~
$exe = ~a~%~
$target = ~a~%~
$installDir = ~a~%~
$zip = ~a~%~
$stage = ~a~%~
$old = \"$exe.old\"~%~
$stillRunning = $false~%~
try {~%~
    # The client launched us right before quitting; wait it out.~%~
    $proc = Get-Process -Id ~d -ErrorAction SilentlyContinue~%~
    if ($proc) {~%~
        Wait-Process -Id ~:*~d -Timeout 60 -ErrorAction SilentlyContinue~%~
        $proc.Refresh()~%~
        if (-not $proc.HasExited) { $stillRunning = $true; throw \"the old client (pid ~:*~d) did not exit\" }~%~
    }~%~
    if (Test-Path $stage) { Remove-Item -Recurse -Force $stage }~%~
    Expand-Archive -Path $zip -DestinationPath $stage -Force~%~
    $newExe = Join-Path $stage 'RappyRunsClient.exe'~%~
    if (-not (Test-Path $newExe)) { throw \"no RappyRunsClient.exe in the update zip\" }~%~
    # A file of the running image may linger locked briefly; retry the move.~%~
    $moved = $false~%~
    for ($i = 0; $i -lt 10; $i++) {~%~
        try { Move-Item -Force $exe $old -ErrorAction Stop; $moved = $true; break }~%~
        catch { Start-Sleep -Seconds 1 }~%~
    }~%~
    if (-not $moved) { throw \"could not move the old exe aside\" }~%~
    Copy-Item $newExe $target -Force~%~
    $newData = Join-Path $stage 'data'~%~
    if (Test-Path $newData) {~%~
        New-Item -ItemType Directory -Force (Join-Path $installDir 'data') | Out-Null~%~
        Copy-Item (Join-Path $newData '*') (Join-Path $installDir 'data') -Recurse -Force~%~
    }~%~
    # Best effort: a leftover recording process may still hold ffmpeg.exe.~%~
    $newFfmpeg = Join-Path $stage 'ffmpeg'~%~
    if (Test-Path $newFfmpeg) {~%~
        try {~%~
            New-Item -ItemType Directory -Force (Join-Path $installDir 'ffmpeg') | Out-Null~%~
            Copy-Item (Join-Path $newFfmpeg '*') (Join-Path $installDir 'ffmpeg') -Recurse -Force~%~
        } catch { Write-Output \"ffmpeg update skipped: $_\" }~%~
    }~%~
    Start-Process -FilePath $target -WorkingDirectory $installDir~%~
    Remove-Item -Force $zip -ErrorAction SilentlyContinue~%~
    Remove-Item -Recurse -Force $stage -ErrorAction SilentlyContinue~%~
} catch {~%~
    Write-Output \"update failed: $_\"~%~
    if ((Test-Path $old) -and -not (Test-Path $exe)) {~%~
        # When the old exe carried a different name, drop the half-installed new-name exe too.~%~
        if ($target -ne $exe) { Remove-Item -Force $target -ErrorAction SilentlyContinue }~%~
        Move-Item $old $exe~%~
    }~%~
    if ((-not $stillRunning) -and (Test-Path $exe)) {~%~
        Start-Process -FilePath $exe -WorkingDirectory $installDir~%~
    }~%~
} finally {~%~
    Stop-Transcript | Out-Null~%~
}~%"
          (ps-quote log-path)
          (ps-quote exe-path)
          (ps-quote target-exe-path)
          (ps-quote install-dir)
          (ps-quote zip-path)
          (ps-quote stage-dir)
          pid))

;;; ------------------------------------------------------------------
;;; LispWorks side: HTTP, filesystem and the actual handover.
;;; ------------------------------------------------------------------

(defvar *stop-requested* nil
  "Set to stop the poll loop (main.lisp); the updater sets it before
quitting so the recorder shuts down cleanly.")

(defvar *really-quitting* nil
  "Set by QUIT-APP so CLIENT-CONFIRM-DESTROY lets the window close for a
genuine quit instead of hiding it to the tray (gui.lisp/tray-win32.lisp).")

(defvar *poll-process* nil
  "The poll-loop mp:process, kept so the updater can join it (running
the recorder shutdown) before quitting.")

(defvar *poll-busy-p* nil
  "T while a quest run or recording is in flight; updates are never
applied then (poll-loop maintains this).")

(defvar *update-ready-zip* nil
  "(zip-pathname . tag) of a verified download waiting for the client
to go idle before the automatic restart.")

(defvar *startup-update-note* nil
  "Outcome of the pre-GUI update pass (a STARTUP-UPDATE-DECISION value,
or :DOWNLOAD-FAILED), surfaced on the settings pane once the main
window exists. NIL when the pass did not run (auto-update off, dev
build).")

#+lispworks
(defun windows-temp-dir ()
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "TEMP") (uiop:getenv "TMP")
       (namestring (user-homedir-pathname)))))

#+lispworks
(defun update-zip-path ()
  (merge-pathnames "RappyRunsClient-update.zip" (windows-temp-dir)))

#+lispworks
(defun client-exe-path ()
  "The delivered image's own exe. LW:LISP-IMAGE-NAME is authoritative;
argv[0] (used elsewhere for data lookups) can be a bare relative name."
  (lw:lisp-image-name))

#+lispworks
(defun install-dir ()
  (uiop:pathname-directory-pathname (client-exe-path)))

#+lispworks
(defun install-dir-writable-p ()
  "Whether the folder next to the exe accepts writes (it will not under
Program Files without elevation); checked before downloading so the
failure mode is a manual-download hint, not a broken helper run."
  (handler-case
      (let ((probe (merge-pathnames "eta-write-probe.tmp" (install-dir))))
        (with-open-file (out probe :direction :output :if-exists :supersede)
          (write-char #\x out))
        (delete-file probe)
        t)
    (error () nil)))

#+lispworks
(defun fetch-latest-release ()
  "The latest release plist from the GitHub API, or NIL when there is
none (404 before the first release, 403 when rate limited) or the
network is down. NIL simply means \"no update today\"."
  (handler-case
      (multiple-value-bind (status body)
          (http-request "GET"
                        (format nil "https://api.github.com/repos/~a/releases/latest"
                                (resolve-update-repo))
                        :headers '(("Accept" . "application/vnd.github+json")))
        (when (eql status 200)
          (parse-release-json body)))
    (api-error () nil)))

#+lispworks
(defun download-update! (release target &key on-progress)
  "Download RELEASE's zip asset to TARGET. Returns TARGET on success;
NIL (with no leftover file) when the download or its verification
fails."
  (multiple-value-bind (scheme host port path)
      (parse-url (getf release :asset-url))
    (handler-case
        (let ((status (winhttp-download "GET" scheme host port path target
                                        :on-progress on-progress)))
          (if (and (eql status 200)
                   (valid-update-zip-p target (getf release :asset-size)))
              target
              (progn (ignore-errors (delete-file target)) nil)))
      (error ()
        (ignore-errors (delete-file target))
        nil))))

#+lispworks
(defun cleanup-old-update-files ()
  "Sweep the leftovers of a previous self-update (the .old exe the
helper cannot delete itself, plus anything a failed run left in %TEMP%).
Safe to call on every startup."
  (ignore-errors
    (let ((old (merge-pathnames "RappyRunsClient.exe.old" (install-dir))))
      (when (probe-file old) (delete-file old))))
  (let ((temp (windows-temp-dir)))
    (dolist (name '("rappyruns-update.ps1" "RappyRunsClient-update.zip"))
      (ignore-errors
        (let ((path (merge-pathnames name temp)))
          (when (probe-file path) (delete-file path)))))
    (ignore-errors
      (uiop:delete-directory-tree
       (merge-pathnames "rappyruns-update-stage/" temp)
       :validate t :if-does-not-exist :ignore))))

#+lispworks
(defun launch-updater-and-quit (interface zip-path)
  "Hand over to the helper script and exit: the script waits for this
process to die, swaps the exe and restarts the new build."
  (let* ((temp (windows-temp-dir))
         (script-path (merge-pathnames "rappyruns-update.ps1" temp))
         (text (updater-script-text
                :pid (%get-current-process-id)
                :exe-path (namestring (client-exe-path))
                :target-exe-path (namestring
                                  (merge-pathnames +client-exe-name+
                                                   (install-dir)))
                :install-dir (namestring (install-dir))
                :zip-path (namestring zip-path)
                :stage-dir (namestring
                            (merge-pathnames "rappyruns-update-stage/" temp))
                :log-path (namestring
                           (merge-pathnames "rappyruns-update.log" temp)))))
    (with-open-file (out script-path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
      ;; BOM first: PowerShell 5.1 reads a BOM-less .ps1 as ANSI (cp932
      ;; on Japanese Windows), garbling non-ASCII install paths - e.g.
      ;; a OneDrive desktop folder - so every path check in the script
      ;; missed and the swap failed.
      (write-char (code-char #xfeff) out)
      (write-string text out))
    (close-capture-handles
     (spawn-process "powershell.exe"
                    (list "-NoProfile" "-ExecutionPolicy" "Bypass"
                          "-File" (namestring script-path))))
    ;; Stop the poll loop first so its unwind-protect shuts the
    ;; recorder down before the process dies.
    (setf *stop-requested* t)
    (let ((poll *poll-process*))
      (when poll
        (ignore-errors (mp:process-join poll :timeout 10))))
    ;; Quit unconditionally: the helper is already waiting on our PID,
    ;; so lingering here would only make it abort after its timeout.
    (capi:execute-with-interface-if-alive
     interface
     (lambda () (capi:destroy interface)))
    (lw:quit :status 0)))
