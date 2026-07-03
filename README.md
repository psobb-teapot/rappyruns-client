# ephinea-ta client

Desktop companion app for the ephinea-ta leaderboard site. It attaches to
a running Ephinea PSOBB client (read-only, via `ReadProcessMemory`),
detects quest starts and completions from quest registers / floor switches
(trigger data transcribed from [psostats-client], MIT), and auto-submits
finished runs to the site as **drafts** via `POST /api/runs`. You then
attach the video URL on the site (My Runs → Drafts) to send the run to
the moderation queue.

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

## Packaging

```
powershell -File client/package.ps1
```

Bundles the exe and `data/quest-triggers.sexp` (from `client/data/`, the
source of truth) into `client/dist/EphineaTAClient.zip`.

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
