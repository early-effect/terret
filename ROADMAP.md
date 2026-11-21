# Terret roadmap

Decisions live in [DESIGN.md](DESIGN.md). This file is status only. The change that finishes a phase updates this file in the same change.

## Where we are

Phase 5, more adapters. The dsh headless adapter is in. The process defaults to Grok and selects dsh with `harness`. `0.0.1` is published. The open follow-up is the 0.0.1 trial: the run command, tool output, and per-token events. A push to `main` publishes that site at `/terret`.

## Phases

| Phase | Status | Ships | Proves |
| --- | --- | --- | --- |
| 1. Core | Done | Library with no network and no Grok: harness, session, prompt, streamed events, workspace, ceiling, hooks | Tests against an in-memory harness |
| 2. Grok Build adapter | Done | One adapter, three transports: `grok -p` / `--prompt-file`, ACP stdio, WebSocket ACP | The adapter contract; the user picks the transport |
| 3. MCP inbound | Done | A heddle `Api`. `Mcp.from` serves `open`, `run`, `status`, `resume`, `export`, and `cancel` on `mcp.routes` and `mcp.stdio()`. `follow` is a subscription on the same API, not a tool | Hosts attach without grok-specific tool names |
| 4. Packaging | Process landed; `main` publishes the site | A runnable process, Warp's `.warp/.mcp.json`, and DocSpecs for attach, jobs, ceiling, and config | Config picks HTTP or stdio; HTTP is the documented attach path; snippets are proven by `./histos test` |
| 5. More adapters | dsh headless landed | The dsh adapter speaks the headless `--json` stream. Later harnesses use the same traits | The MCP jobs do not change |

Phase 1 landed the `core` module. Phase 2 landed the `grok` module. Phase 3 landed the `mcp` module. Phase 4 landed the `app` module (`terret.app.Main`) and the `docs` module. Phase 5 landed the `dsh` module. The default listen is HTTP on `127.0.0.1:8080/mcp`. Warp reads `.warp/.mcp.json`. There is no Warp.local library. `./histos test` covers the ceiling, workspace confinement, run, status, resume, transcript, cancel, hooks, the three Grok transports, the MCP jobs on `mcp.routes` and an in-memory pipe, the process config, and the docs pages. CLI and ACP stdio run against stand-ins. The WebSocket transport runs against a heddle server. A local `./histos test` runs the live grok smoke test when `grok` is on `PATH` and authenticated, and skips it in CI or when `grok` is missing or logged out, with a printed reason. A live `dsh` prompt stays out unless `TERRET_LIVE_DSH` is set. The dsh adapter runs against a stand-in. The first prompt omits `--session-id`. The next prompt adopts the id from the `session` event. The export job's Scala method is `transcript`, because `export` is a keyword. The job name stays `export`. `export` returns a cursor. `follow` streams that log. Closing the stream does not cancel the turn. `open` is the session start a host calls before `run`. The site task pins specular-histos 0.0.2. That jar carries the API digest, so the plugin loads from Central. A push to `main` publishes the site at `https://www.earlyeffect.rocks/terret/`.
