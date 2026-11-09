# Terret roadmap

Decisions live in [DESIGN.md](DESIGN.md). This file is status only. The change that finishes a phase updates this file in the same change.

## Where we are

Phase 3, MCP inbound.

Next slice: a heddle `Api` whose jobs `Mcp.from` serves on `mcp.routes` and `mcp.stdio()`. While `run` is in flight, publish each `Event`. Prove it with `mcp.routes` and an in-memory pipe over the in-memory harness.

## Phases

| Phase | Status | Ships | Proves |
| --- | --- | --- | --- |
| 1. Core | Done | Library with no network and no Grok: harness, session, prompt, streamed events, workspace, ceiling, hooks | Tests against an in-memory harness |
| 2. Grok Build adapter | Done | One adapter, three transports: `grok -p` / `--prompt-file`, ACP stdio, WebSocket ACP | The adapter contract; the user picks the transport |
| 3. MCP inbound | Not started | A heddle `Api` whose jobs `Mcp.from` serves on `mcp.routes` and `mcp.stdio()` | Hosts attach without grok-specific tool names |
| 4. Packaging | Not started | A runnable process, Warp.local defaults, and the specular docs site | Config picks HTTP or stdio; HTTP is the documented attach path; snippets are proven by the build |
| 5. More adapters | Not started | dsh and later harnesses implement the same traits | The MCP jobs do not change |

Phase 1 landed the `core` module. Phase 2 landed the `grok` module. `./histos test` covers the ceiling, workspace confinement, run, resume, transcript, cancel, hooks, and the three Grok transports. CLI and ACP stdio run against stand-ins. The WebSocket transport runs against a heddle server. A live `grok` prompt stays out of that run unless `TERRET_LIVE_GROK` is set. The export job's Scala method is `transcript`, because `export` is a keyword. The job name stays `export`.
