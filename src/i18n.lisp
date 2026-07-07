(in-package :ephinea-ta-client)

;;; UI strings in English and Japanese. Pure CL so the labels used by
;;; store.lisp/api-client.lisp stay testable on SBCL. The GUI rebuilds
;;; its window on a language switch (see LANGUAGE-CHANGED-CALLBACK), so
;;; everything here is looked up at pane-creation or format time.
;;;
;;; This file is UTF-8; ASDF3 compiles with UTF-8 by default on both
;;; SBCL and LispWorks.

(defparameter *languages* '(:en :ja))

(defvar *language* :en
  "Current UI language; set from config at startup and by the toggle.")

(defun valid-language (value)
  (if (member value *languages*) value :en))

(defun language-label (language)
  "Label for the language toggle, always in its own language."
  (ecase language
    (:en "English")
    (:ja "日本語")))

(defparameter *strings*
  '(;; Status panes
    :game-searching
    ("Game: searching..."
     "ゲーム: 探索中...")
    :game-attached
    ("Game: attached"
     "ゲーム: 接続済み")
    :game-status-with-error
    ("~a / recording error: ~a"
     "~a / 録画エラー: ~a")
    :game-signature-refused
    ("Game: not the official Ephinea client (~a) - recording refused"
     "ゲーム: Ephinea 公式クライアントではないため記録しません (~a)")
    :signature-unsigned
    ("no signature"
     "署名なし")
    :signature-invalid
    ("signature could not be verified"
     "署名を検証できません")
    :signature-untrusted-signer
    ("signed by \"~a\""
     "署名者 \"~a\"")
    :server-not-checked
    ("Server: not checked"
     "サーバー: 未確認")
    :server-ok
    ("Server: OK (~d quests, ~d timed category~:p~@[; ~d local trigger~:p unknown~])"
     "サーバー: OK (クエスト ~d 件、計測カテゴリ ~d 件~@[、不明なローカルトリガー ~d 件~])")
    :token-not-checked
    ("Token: not checked"
     "トークン: 未確認")
    :token-not-set
    ("Token: not set"
     "トークン: 未設定")
    :token-checking
    ("Token: checking..."
     "トークン: 確認中...")
    :token-ok
    ("Token: OK (~a)"
     "トークン: OK (~a)")
    :token-invalid
    ("Token: invalid or revoked"
     "トークン: 無効または失効済み")
    :file-login-checking
    ("Token: logging in with login.txt..."
     "トークン: login.txt でログイン中...")
    :file-login-invalid
    ("Token: login.txt username or password rejected"
     "トークン: login.txt のユーザー名またはパスワードが正しくありません")
    :file-login-bad-file
    ("Token: login.txt is unreadable (needs username= and password= lines, UTF-8)"
     "トークン: login.txt を読み取れません (username= と password= の行が必要・UTF-8)")
    :file-login-failed
    ("Token: login.txt login failed (~a)"
     "トークン: login.txt ログイン失敗 (~a)")
    :no-active-quest
    ("No active quest"
     "実行中のクエストなし")
    :quest-waiting
    ("~a (waiting for start)"
     "~a (開始待ち)")

    ;; Runs tab
    :tab-runs
    ("Runs"
     "記録")
    :tab-settings
    ("Settings"
     "設定")
    :col-quest
    ("Quest"
     "クエスト")
    :col-time
    ("Time"
     "タイム")
    :col-party
    ("Party"
     "人数")
    :col-video
    ("Video"
     "動画")
    :col-status
    ("Status"
     "状態")
    :upload-button
    ("Upload to YouTube"
     "YouTube にアップロード")
    :recordings-folder-button
    ("Open recordings folder"
     "録画フォルダを開く")
    :my-runs-button
    ("Open My Runs (add videos)"
     "My Runs を開く (動画の追加)")
    :retry-button
    ("Submit pending runs"
     "未送信の記録を送信")
    :clear-list-tooltip
    ("Clear list"
     "一覧をクリア")

    ;; Settings tab
    :group-language
    ("Language"
     "言語")
    :group-connection
    ("Connection"
     "接続")
    :group-completion
    ("When a run completes"
     "計測完了時")
    :group-recording
    ("Recording"
     "録画")
    :group-advanced
    ("Advanced"
     "上級者向け")
    :server-url-label
    ("Server URL"
     "サーバー URL")
    :api-token-label
    ("API token"
     "API トークン")
    :save-button
    ("Save & verify"
     "保存して確認")
    :auto-submit-label
    ("Submit automatically on quest completion"
     "クエスト完了時に記録を自動送信する")
    :submit-aborted-label
    ("Record abandoned quests too (private, only you can see them)"
     "中断したクエストも記録する (非公開・自分だけが見られます)")
    :completion-sound-label
    ("Play a sound when a run completes"
     "計測完了時にサウンドを鳴らす")
    :record-label
    ("Record quest videos automatically"
     "クエスト動画を自動で録画する")
    :record-audio-label
    ("Record game audio (only the game is heard, not Discord etc.)"
     "ゲーム音声を録音する (ゲームの音のみ。Discord などの音は入りません)")
    :video-upload-label
    ("Upload recordings to the site automatically"
     "録画をサイトへ自動アップロードする")
    :delete-after-upload-label
    ("Delete the local file once the site has it"
     "アップロード完了後にローカルの動画を削除する")
    :record-storage-note
    ("The recordings folder is capped at ~a GB; the oldest videos are removed past that."
     "録画フォルダは最大 ~a GB までで、超過分は古い動画から自動削除されます。")
    :video-retention-note
    ("Top-10 videos are kept; others are deleted after 90 days (see the site's Client page)."
     "上位10位の動画は保持され、それ以外は90日で削除されます (詳細はサイトの Client ページ)。")
    :record-dir-label
    ("Recordings folder: ~a"
     "録画フォルダ: ~a")
    :change-folder-button
    ("Change folder..."
     "フォルダを変更...")
    :trigger-log-label
    ("Log trigger changes (for finding switch IDs of new categories)"
     "トリガーの変化をログに記録する (新カテゴリのスイッチ ID 調査用)")

    ;; Updates group (self-update; updater.lisp + gui.lisp)
    :group-updates
    ("Updates"
     "アップデート")
    :version-status
    ("Version: ~a~@[ - ~a~]"
     "バージョン: ~a~@[ - ~a~]")
    :auto-update-label
    ("Update automatically at startup"
     "起動時に自動でアップデートする")
    :check-updates-button
    ("Check for updates now"
     "今すぐアップデートを確認")
    :update-checking
    ("checking for updates..."
     "アップデートを確認中...")
    :update-check-failed
    ("update check failed"
     "アップデートの確認に失敗しました")
    :update-up-to-date
    ("up to date"
     "最新です")
    :update-downloading
    ("downloading ~a... ~d~@[ / ~d~] MB"
     "~a をダウンロード中... ~d~@[ / ~d~] MB")
    :update-download-failed
    ("download failed"
     "ダウンロードに失敗しました")
    :update-not-writable
    ("install folder not writable"
     "インストール先フォルダに書き込めません")
    :update-after-run
    ("~a downloaded - installs after this run"
     "~a ダウンロード済み - このランの終了後にインストールします")
    :update-restarting
    ("installing ~a - restarting..."
     "~a をインストール中 - 再起動します...")

    ;; Dialogs
    :clear-list-confirm
    ("Clear the list?~%~%Runs not submitted yet are kept. Recordings stay on disk and drafts stay on the server (videos can still be added on the site), but this client forgets which recording belongs to which draft.~%~%Nothing is deleted on the server."
     "一覧をクリアしますか?~%~%未送信の記録は残ります。録画ファイルとサーバー上の下書きもそのまま残ります (動画の追加はサイトからできます) が、どの録画がどの下書きのものかの対応はこのクライアントから消えます。~%~%サーバー上のデータは削除されません。")
    :choose-record-dir
    ("Choose the recordings folder"
     "録画フォルダを選択")
    :trigger-log-on
    ("Trigger logging is on. Play the segment, then open:~%~%~a~%~%The floor switch (or register) that flips when the room is cleared is your end trigger."
     "トリガーログを有効にしました。対象の区間をプレイしてから、次のファイルを開いてください:~%~%~a~%~%部屋のクリア時に変化するフロアスイッチ (またはレジスタ) が終了トリガーです。")
    :ffmpeg-missing
    ("ffmpeg was not found, so recording stays off.~%~%Use the client zip that bundles it (ffmpeg\\ffmpeg.exe next to the exe), or install ffmpeg so it is on PATH."
     "ffmpeg が見つからないため、録画は無効のままです。~%~%ffmpeg を同梱したクライアント zip (exe の隣の ffmpeg\\ffmpeg.exe) を使うか、PATH の通る場所に ffmpeg をインストールしてください。")
    :no-recording-for-run
    ("This run has no saved recording.~%~%Videos are only saved when recording is enabled while the quest is played."
     "この記録には保存された録画がありません。~%~%動画は、クエストのプレイ中に録画が有効だった場合にのみ保存されます。")
    :no-recordings-yet
    ("No saved recordings to upload yet.~%~%Videos are saved automatically when a recorded quest completes."
     "アップロードできる録画はまだありません。~%~%録画中のクエストが完了すると、動画は自動で保存されます。")
    :recording-file-missing
    ("The recording file is missing:~%~%~a"
     "録画ファイルが見つかりません:~%~%~a")
    :attach-choose
    ("A YouTube link was copied:~%~a~%~%Attach it to which run?"
     "YouTube のリンクがコピーされました:~%~a~%~%どの記録に紐付けますか?")
    :attach-confirm
    ("Attach the copied YouTube link to this run?~%~%~a~%~a"
     "コピーされた YouTube のリンクをこの記録に紐付けますか?~%~%~a~%~a")
    :attach-confirm-replace
    ("Replace this run's auto-uploaded video with the copied YouTube link?~%The hosted copy will be deleted and your link goes to review instead.~%~%~a~%~a"
     "この記録の自動アップロードされた動画を、コピーされた YouTube のリンクで差し替えますか?~%ホストされている動画は削除され、代わりにこのリンクが審査対象になります。~%~%~a~%~a")
    :attach-failed
    ("Could not attach the video:~%~%~a"
     "動画を紐付けられませんでした:~%~%~a")
    :attach-already-submitted
    ("The video was not changed: the site already has a video on file for this run (it may have been approved in the meantime)."
     "動画は変更されませんでした: この記録にはサイト側で既に動画が登録されています (その間に承認された可能性があります)。")
    :pairing-waiting
    ("Token: approve the connection in your browser..."
     "トークン: ブラウザで接続を承認してください...")
    :pairing-expired
    ("Token: pairing expired - restart the client, or paste a token in Settings"
     "トークン: 接続の有効期限が切れました。クライアントを再起動するか、設定タブでトークンを貼り付けてください")
    :pairing-failed
    ("Token: pairing could not start (~a)"
     "トークン: 接続を開始できません (~a)")
    :token-ok-dialog
    ("Token OK - authenticated as ~a."
     "トークン OK - ~a として認証されました。")
    :token-rejected-dialog
    ("The server rejected the API token (unauthorized).~%~%Paste a fresh one from the site's token page."
     "サーバーが API トークンを拒否しました (認証エラー)。~%~%サイトのトークンページから新しいトークンを貼り付けてください。")
    :update-dev-build
    ("This is a dev build (no version baked in), so there is nothing to compare against.~%~%Releases live at:~%~a"
     "これは開発ビルドです (バージョン情報が埋め込まれていません)。比較対象がないため確認できません。~%~%リリースはこちら:~%~a")
    :update-check-failed-dialog
    ("Could not check for updates - network trouble, GitHub rate limiting, or no release published yet."
     "アップデートを確認できませんでした - ネットワークの問題、GitHub のレート制限、またはリリースが未公開の可能性があります。")
    :update-latest-dialog
    ("You are on the latest version (~a)."
     "最新バージョン (~a) を使用しています。")
    :update-not-writable-confirm
    ("The client's folder is not writable, so the update cannot be applied automatically.~%~%Open the download page to update by hand?"
     "クライアントのフォルダに書き込めないため、アップデートを自動で適用できません。~%~%ダウンロードページを開いて手動で更新しますか?")
    :update-download-failed-dialog
    ("The update download failed or did not verify. Nothing was changed; try again later."
     "アップデートのダウンロードに失敗したか、検証を通りませんでした。何も変更されていません。後でもう一度お試しください。")

    ;; Runs list labels (store.lisp)
    :status-video-attached
    ("video attached - awaiting review"
     "動画紐付け済み - 承認待ち")
    :status-video-approved
    ("video attached - approved"
     "動画紐付け済み - 承認済み")
    :status-aborted-video-attached
    ("aborted - video attached, private"
     "中断 - 動画紐付け済み・非公開")
    :status-queued
    ("queued"
     "送信待ち")
    :status-draft-upload
    ("draft - use Upload to YouTube"
     "下書き - 「YouTube にアップロード」を使ってください")
    :status-draft-auto-upload
    ("draft - video uploads automatically"
     "下書き - 動画は自動でアップロードされます")
    :status-draft-add
    ("draft - double-click to add video"
     "下書き - ダブルクリックで動画を追加")
    :status-draft-aborted
    ("aborted - private, recording kept locally"
     "中断 - 非公開・録画はローカル保存のみ")
    :status-duplicate
    ("duplicate (already on server)"
     "重複 (サーバーに登録済み)")
    :status-rejected
    ("rejected: ~a"
     "拒否されました: ~a")
    :status-failed
    ("failed: ~a"
     "失敗しました: ~a")
    :video-attached
    ("attached"
     "紐付け済み")
    :video-saved
    ("saved"
     "保存済み")
    :video-uploading
    ("uploading ~d%"
     "アップロード中 ~d%")
    :video-uploaded
    ("uploaded"
     "アップロード済み")
    :video-upload-failed
    ("upload failed"
     "アップロード失敗")

    ;; Connection error texts (api-client.lisp)
    :hint-address
    ("server address not found - check the Server URL"
     "サーバーアドレスが見つかりません - サーバー URL を確認してください")
    :hint-connect
    ("could not connect - server down, or no internet?"
     "接続できませんでした - サーバー停止中またはインターネット未接続?")
    :hint-timeout
    ("connection timed out"
     "接続がタイムアウトしました")
    :hint-tls
    ("secure connection (https) failed"
     "セキュア接続 (https) に失敗しました")
    :server-error-prefix
    ("Server: ~a"
     "サーバー: ~a")
    :server-bad-url
    ("Server: the Server URL looks wrong - fix it and press Save & verify"
     "サーバー: サーバー URL が正しくないようです - 修正して「保存して確認」を押してください")
    :server-unexpected
    ("Server: unexpected response (~a) - is the URL right?"
     "サーバー: 予期しない応答 (~a) - URL は正しいですか?")
    :server-check-failed
    ("Server: check failed (~a)"
     "サーバー: 確認に失敗しました (~a)")
    :token-could-not-verify
    ("Token: could not verify (~a)"
     "トークン: 確認できませんでした (~a)"))
  "Plist: key -> (english japanese). Entries are FORMAT control strings;
TR always formats, so ~% and friends are processed even without args.")

(defun tr (key &rest args)
  "The UI string for KEY in *LANGUAGE*, run through FORMAT with ARGS."
  (let ((entry (getf *strings* key)))
    (unless entry (error "No UI string for ~s" key))
    (apply #'format nil
           (ecase *language*
             (:en (first entry))
             (:ja (second entry)))
           args)))

(defun signature-status-label (rejection)
  "Human-readable reason the PSOBB exe failed Authenticode
verification, from a REJECTION plist (:status :signer ...)."
  (case (getf rejection :status)
    (:unsigned (tr :signature-unsigned))
    ;; A valid signature that still got rejected means the signer is
    ;; not on +TRUSTED-PSOBB-SIGNERS+ - show who actually signed it.
    (:valid (tr :signature-untrusted-signer (or (getf rejection :signer) "?")))
    (t (tr :signature-invalid))))
