# ephinea-ta client

Desktop companion app for the ephinea-ta leaderboard site. It attaches to
a running Ephinea PSOBB client (read-only, via `ReadProcessMemory`),
detects quest starts and completions from quest registers / floor switches
(trigger data transcribed from [psostats-client], MIT), and auto-submits
finished runs to the site as **drafts** via `POST /api/runs`. The video
then goes to the moderation queue with two clicks and no site visit:
"Upload to YouTube" opens the upload page plus an Explorer window with
the run's recording selected; once YouTube's share URL is copied, the
client notices it on the clipboard, asks which run it belongs to and
attaches it via `POST /api/runs/:id/video`. (Attaching the URL by hand
on the site - My Runs → Drafts - still works as a fallback.)

[psostats-client]: https://github.com/phelix-/psostats-client

## Requirements

- LispWorks 8.x (64-bit, Windows) for the GUI, FLI memory reading and
  `deliver`. The pure-CL core (detection, HTTP, API client) also runs on
  SBCL for tests.
- Quicklisp (for the `com.inuoe.jzon` dependency).

## Running from source (LispWorks listener)

```lisp
(load "~/quicklisp/setup.lisp")
(push #p"C:/Users/g23tl/src/ephinea-ta/client/" asdf:*central-registry*)
(ql:quickload :ephinea-ta-client)
(ephinea-ta-client:main)
```

First-time setup in the GUI:

1. Create an API token on the site (`/my/tokens`, requires Discord login).
2. Paste the server URL and token into the settings row, "Save settings".
3. Leave "Submit automatically" checked; finished quests appear in the
   list and are uploaded as drafts.

Config and the offline retry queue live in `%APPDATA%/ephinea-ta-client/`.

## Building the exe

```
"C:/Program Files/LispWorks/lispworks-8-1-0-x64-windows.exe" -build client/deliver.lisp
```

Output: `client/dist/EphineaTAClient.exe`. Distribute together with
`data/quest-triggers.sexp` (looked up next to the exe). `https` needs no
extra DLLs: on LispWorks the client speaks HTTP(S) through the Windows
WinHTTP API (`src/winhttp.lisp`), so TLS comes from the OS. (LispWorks'
own COMM SSL is *not* used - it requires OpenSSL 1.1 DLLs that end-user
machines don't have.)

## Recording quest videos

With "Record quest videos automatically" checked, the client records the
game window with ffmpeg (gdigrab window capture): recording starts when
a quest's start trigger fires and stops when the run completes or the
quest is abandoned. Game audio is included by default via the Windows
process-loopback API (Windows 10 2004+) scoped to the PSOBB process:
**only the game is heard** - Discord, notifications etc. never end up
in the video. On older Windows it falls back to endpoint loopback (all
desktop audio). The client captures the PCM itself
(`src/audio-win32.lisp`) and serves it to ffmpeg over a named pipe,
since ffmpeg has no native Windows loopback input; if the audio path
fails entirely the video is still recorded (with silence).

Two non-obvious details, learned the hard way: loopback observes the
signal AFTER the Windows volume mixer, so a low per-app slider would
make recordings inaudible - recordings are therefore captured in
float32 and loudness-normalized (ffmpeg loudnorm) to stay audible
regardless of mixer settings. And when debugging "no audio", check the
mixer first: a working capture of a 5% slider looks exactly like a
broken capture. Disable with "Record game audio" in Settings. Completed runs are saved under the
in-game quest name, e.g.
`Towards the Future 9'59.123 (2026-07-04 2130).mp4`, into
`Videos\EphineaTA\` (configurable, and one click away via "Open
recordings folder"); abandoned/failed captures are deleted
automatically. Recording never interferes with detection or
submission - if ffmpeg fails, the run is still timed and uploaded.

ffmpeg is found in this order: the `:ffmpeg-path` key in
`%APPDATA%\ephinea-ta-client\config.sexp` (power-user override, not
shown in the GUI), then `ffmpeg\ffmpeg.exe` next to the client exe (the
bundled copy), then `ffmpeg.exe` on `PATH`. Toggling the checkbox on
verifies ffmpeg starts and explains what to do when it doesn't. The
recordings folder is picked with a directory dialog ("Change
folder..."), applied immediately.

Implementation: `src/recording.lisp` is the pure state machine (tested
on SBCL against a mock backend); `src/ffmpeg-win32.lisp` spawns
ffmpeg.exe via `CreateProcessW` with a stdin pipe (`q` = graceful stop,
`TerminateProcess` after 5 s as fallback). Output is fragmented MP4
(`-movflags +frag_keyframe+empty_moov`), so even a killed capture stays
playable.

Known limits: capture starts a few hundred ms after the start trigger
(ffmpeg spin-up), gdigrab can't capture a minimized window, and the
recording gets a ~3 s video tail after the run ends (ffmpeg drains the
buffered audio before quitting).

## Packaging

```
powershell -File client/package.ps1
```

Bundles the exe and `data/quest-triggers.sexp` (from `client/data/`, the
source of truth) into `client/dist/EphineaTAClient.zip`, plus
`ffmpeg/ffmpeg.exe` when `client/vendor/ffmpeg/` is populated (see
below); without it the zip is built with a warning and recording needs a
user-installed ffmpeg.

### Bundling ffmpeg

`client/vendor/` is not committed. To ship ffmpeg in the zip, download a
**GPL win64** build from [BtbN/FFmpeg-Builds][btbn] (asset
`ffmpeg-master-latest-win64-gpl.zip`) and copy into
`client/vendor/ffmpeg/`:

- `bin/ffmpeg.exe` → `client/vendor/ffmpeg/ffmpeg.exe`
- `LICENSE.txt` → `client/vendor/ffmpeg/LICENSE.txt` (required:
  ffmpeg GPL builds must ship with their license text; it is copied
  into the zip next to the exe)

ffmpeg runs as a separate process, so bundling it does not affect the
client's own MIT license.

[btbn]: https://github.com/BtbN/FFmpeg-Builds/releases

## Releasing

Releases live on a separate **public** repo so the main repo can stay
private; the site links to the `latest` asset URL, so publishing a
release is all it takes - no site change or redeploy. Uses the [GitHub
CLI](https://cli.github.com/) (`gh`); the web UI works too (create the
release on the releases repo and upload the zip as an asset).

One-time setup:

```
gh repo create psobb-teapot/ephinea-ta-client-releases --public \
  --description "Binary releases of the Ephinea TA desktop client"
```

Give it a single README commit (releases need at least one commit); no
source goes there.

Per release (tag `vX.Y.Z`), `release.ps1` builds the exe, packages the
zip and publishes it in one go:

```
.\client\release.ps1 v0.2.0 -NotesFile notes.md   # new release
.\client\release.ps1 v0.2.0 -Prerelease           # test build, excluded from `latest`
.\client\release.ps1 v0.1.0 -Clobber              # replace the asset on an existing release
```

Without `-NotesFile` the notes are just the version; write real notes
in a file (passing quotes on the `gh` command line is unreliable).

The asset must be named `EphineaTAClient.zip` - the site's download
button points at
`https://github.com/psobb-teapot/ephinea-ta-client-releases/releases/latest/download/EphineaTAClient.zip`
(override with `ETA_CLIENT_DOWNLOAD_URL` on the server). Pre-releases
are excluded from `latest`, so they are safe for test builds.

## Quest coverage

`data/quest-triggers.sexp` defines the start/end trigger per quest and
maps it to the site's quest slug. Quests without an entry are ignored by
the detector (they show as "waiting" but never start a run). To add one,
copy its definition from psostats-client's `questDefinitions.go` and
derive the slug from the site's `src/seed.lisp` naming (`epN-<name>`,
lowercased, non-alphanumerics collapsed to `-`). The client cross-checks
all slugs against `GET /api/quests` at startup and reports unknown ones
in the server status line.

## Segment categories (e.g. "GDV reset")

Multiple trigger entries may share one in-game quest: the detector
tracks each matching definition in parallel, so a single full run
produces the full-clear draft *and* every segment draft. There are two
ways to define a segment category; the site form is the normal one.

### On the site (no client config, no rebuild)

A moderator defines the whole thing on `/mod/quests`:

1. Enter the name (e.g. "GDV reset"), episode and category.
2. Under **Detection**, fill in the game quest number (from psostats or
   in-game, e.g. 944 for GDV), the start trigger (usually the parent
   quest's start) and the end trigger (what fires when the segment is
   done). Save.

The client fetches these categories from `GET /api/quests` at startup
(and whenever you press Save settings), converts their triggers and
times them automatically alongside the full clear. If you get a switch
number wrong, fix it with the inline "Edit detection" form on the same
page - the client picks up the change on its next server check.

To discover a trigger number, enable **Log trigger changes** in the
client settings, play the segment once, and read
`%APPDATA%/ephinea-ta-client/trigger-log.txt`: the floor switch (or
register) that flips the moment the room's last enemy dies is the end
trigger. In gated TA quests the door-opening switch is exactly "room
cleared".

For quests psostats already documents, room switches are known: e.g.
MAE Forest rooms are floor 2 switches 1..3 (room N cleared = switch N),
MAE Caves floor 4 switches 1..6, Sweep-up #8 floor 10 and #9 floor 17
(see psostats `questDefinitions.go` Splits).

### In the builtin file (shipped defaults)

`data/quest-triggers.sexp` holds the builtin full-clear definitions and
can also carry segment entries. Add one with the same `:number`/`:names`
as the parent quest, the parent's `:start`, the discovered `:end`, and a
slug matching a quest created on the site. Server-defined categories win
over builtin ones on a slug collision.

## Tests

Pure-CL unit tests (no game, no server):

```lisp
(ql:quickload :ephinea-ta-client)
(load "client/tests/client-tests.lisp")
;; prints PASS/FAIL lines, returns the failure count
```

They cover memory decoding against synthetic memory images, snapshot
parsing, and the full detection state machine (start/end triggers,
warp-in quests, PB category flags, mid-quest attach guard).
