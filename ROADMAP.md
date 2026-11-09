# Terret roadmap

Decisions live in [DESIGN.md](DESIGN.md). This file is status only. The change that finishes a phase updates this file in the same change.

## Where we are

Phase 5, more adapters. The dsh headless adapter is in. The process still wires Grok. Phase 4's site task is still blocked: the published specular-histos jar has no API digest resource, and this histos refuses that jar.

## Phases

| Phase | Status | Ships | Proves |
| --- | --- | --- | --- |
| 1. Core | Done | Library with no network and no Grok: harness, session, prompt, streamed events, workspace, ceiling, hooks | Tests against an in-memory harness |
| 2. Grok Build adapter | Done | One adapter, three transports: `grok -p` / `--prompt-file`, ACP stdio, WebSocket ACP | The adapter contract; the user picks the transport |
| 3. MCP inbound | Done | A heddle `Api`. `Mcp.from` serves `open`, `run`, `resume`, `export`, and `cancel` on `mcp.routes` and `mcp.stdio()` | Hosts attach without grok-specific tool names |
| 4. Packaging | Process landed; site task blocked | A runnable process, Warp's `.warp/.mcp.json`, and DocSpecs for attach, jobs, ceiling, and config | Config picks HTTP or stdio; HTTP is the documented attach path; snippets are proven by `./histos test` |
| 5. More adapters | dsh headless landed | The dsh adapter speaks the headless `--json` stream. Later harnesses use the same traits | The MCP jobs do not change |

Phase 1 landed the `core` module. Phase 2 landed the `grok` module. Phase 3 landed the `mcp` module. Phase 4 landed the `app` module (`terret.app.Main`) and the `docs` module. Phase 5 landed the `dsh` module. The default listen is HTTP on `127.0.0.1:8080/mcp`. Warp reads `.warp/.mcp.json`. There is no Warp.local library. `./histos test` covers the ceiling, workspace confinement, run, resume, transcript, cancel, hooks, the three Grok transports, the MCP jobs on `mcp.routes` and an in-memory pipe, the process config, and the docs pages. CLI and ACP stdio run against stand-ins. The WebSocket transport runs against a heddle server. A live `grok` prompt stays out of that run unless `TERRET_LIVE_GROK` is set. A live `dsh` prompt stays out unless `TERRET_LIVE_DSH` is set. The dsh adapter runs against a stand-in. The first prompt omits `--session-id`. The next prompt adopts the id from the `session` event. The export job's Scala method is `transcript`, because `export` is a keyword. The job name stays `export`. `open` is the session start a host calls before `run`. The site task is not wired. The published plugin jar is missing the API digest this histos requires.
