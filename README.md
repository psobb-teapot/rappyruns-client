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
`data/quest-triggers.sexp` (looked up next to the exe). For an `https`
server URL, LispWorks' SSL support must be available at runtime; if your
LispWorks build uses OpenSSL, ship the matching OpenSSL DLLs next to the
exe (LW 8.1 on Windows can also use the native SChannel backend, in which
case no extra DLLs are needed).

## Quest coverage

`data/quest-triggers.sexp` defines the start/end trigger per quest and
maps it to the site's quest slug. Quests without an entry are ignored by
the detector (they show as "waiting" but never start a run). To add one,
copy its definition from psostats-client's `questDefinitions.go` and
derive the slug from the site's `src/seed.lisp` naming (`epN-<name>`,
lowercased, non-alphanumerics collapsed to `-`). The client cross-checks
all slugs against `GET /api/quests` at startup and reports unknown ones
in the server status line.

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
