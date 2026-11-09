# Terret roadmap

Decisions live in [DESIGN.md](DESIGN.md). This file is status only. The change that finishes a phase updates this file in the same change.

## Where we are

Phase 4, packaging.

Next slice: a runnable process, Warp.local defaults, and the specular docs site. Confirm Warp.local before writing its config.

## Phases

| Phase | Status | Ships | Proves |
| --- | --- | --- | --- |
| 1. Core | Done | Library with no network and no Grok: harness, session, prompt, streamed events, workspace, ceiling, hooks | Tests against an in-memory harness |
| 2. Grok Build adapter | Done | One adapter, three transports: `grok -p` / `--prompt-file`, ACP stdio, WebSocket ACP | The adapter contract; the user picks the transport |
| 3. MCP inbound | Done | A heddle `Api`. `Mcp.from` serves `open`, `run`, `resume`, `export`, and `cancel` on `mcp.routes` and `mcp.stdio()` | Hosts attach without grok-specific tool names |
| 4. Packaging | Not started | A runnable process, Warp.local defaults, and the specular docs site | Config picks HTTP or stdio; HTTP is the documented attach path; snippets are proven by the build |
| 5. More adapters | Not started | dsh and later harnesses implement the same traits | The MCP jobs do not change |

Phase 1 landed the `core` module. Phase 2 landed the `grok` module. Phase 3 landed the `mcp` module. `./histos test` covers the ceiling, workspace confinement, run, resume, transcript, cancel, hooks, the three Grok transports, and the MCP jobs on `mcp.routes` and an in-memory pipe. CLI and ACP stdio run against stand-ins. The WebSocket transport runs against a heddle server. A live `grok` prompt stays out of that run unless `TERRET_LIVE_GROK` is set. The export job's Scala method is `transcript`, because `export` is a keyword. The job name stays `export`. `open` is the session start a host calls before `run`.
