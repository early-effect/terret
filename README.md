# Terret

Terret lets coding hosts talk to coding harnesses. Hosts attach over MCP. Harnesses attach through adapters. The core stays harness-agnostic.

This tree has the core, the Grok Build adapter, the dsh headless adapter, MCP inbound, and a runnable process. The core is sessions, a permission ceiling, streamed events, and an in-memory harness. The Grok adapter speaks `grok -p`, ACP stdio, and WebSocket ACP. The dsh adapter speaks the headless `--json` stream. The process defaults to Grok. `harness` selects dsh. Hosts call `open`, `run`, `resume`, `export`, and `cancel`. `terret.app.Main` listens on HTTP by default. A local Warp agent attaches through `.warp/.mcp.json` at `http://127.0.0.1:8080/mcp`. The docs pages live under `docs/` and `./histos test` proves them. The site task is not wired yet. See [DESIGN.md](DESIGN.md) for the decisions and [ROADMAP.md](ROADMAP.md) for which phase is open.
