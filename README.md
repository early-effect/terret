# Terret

Terret lets coding hosts talk to coding harnesses. Hosts attach over MCP. Harnesses attach through adapters. The core stays harness-agnostic.

This tree has the core, the Grok Build adapter, and MCP inbound. The core is sessions, a permission ceiling, streamed events, and an in-memory harness. The adapter speaks `grok -p`, ACP stdio, and WebSocket ACP. Hosts call `open`, `run`, `resume`, `export`, and `cancel`. The docs site is still ahead. See [DESIGN.md](DESIGN.md) for the decisions and [ROADMAP.md](ROADMAP.md) for which phase is open.
