package terret.app.watch

/** The only scripts and styles the watch page loads. Both are same-origin and fixed. There is no directory listing. */
object WatchAssets:
  val js: String =
    s"""(function () {
      var feed = document.getElementById("feed");
      var scroller = feed || document.body;
      var pinned = true;
      var pending = 0;
      function atBottom() {
        return scroller.scrollHeight - scroller.scrollTop - scroller.clientHeight < 48;
      }
      function mark() {
        var node = document.querySelector("[data-follow]");
        if (node) node.setAttribute("data-follow", pinned ? "live" : "paused");
      }
      function setPending(n) {
        pending = n;
        var nodes = document.querySelectorAll("[data-pending]");
        for (var i = 0; i < nodes.length; i++) {
          nodes[i].setAttribute("data-pending", String(n));
          nodes[i].textContent = n === 0 ? "" : n + " new";
        }
      }
      if (feed) {
        feed.addEventListener("scroll", function () {
          pinned = atBottom();
          mark();
          if (pinned) setPending(0);
        });
      }
      var jumps = document.querySelectorAll("[data-jump='latest']");
      for (var j = 0; j < jumps.length; j++) {
        jumps[j].addEventListener("click", function () {
          scroller.scrollTop = scroller.scrollHeight;
          pinned = true;
          mark();
          setPending(0);
        });
      }
      var picker = document.querySelector("[data-chrome-part='picker']");
      if (picker) {
        picker.addEventListener("change", function () {
          var next = picker.value || "";
          if (next.indexOf("/watch") === 0) location.assign(next);
        });
      }
      ${WatchKeys.dispatch}
      var search = document.querySelector("[data-search='q']");
      if (search) {
        search.addEventListener("change", function () {
          var url = new URL(location.href);
          if (search.value) url.searchParams.set("q", search.value);
          else url.searchParams.delete("q");
          location.assign(url.toString());
        });
      }
      document.addEventListener("keydown", function (ev) { watchKey(ev); });
      if (!feed) return;
      var source = feed.getAttribute("data-stream");
      if (!source) return;
      var es = new EventSource(source);
      es.addEventListener("datastar-patch-elements", function (ev) {
        var parsed = read(ev.data || "");
        var html = parsed.elements;
        if (!html) return;
        var target = document.querySelector(parsed.selector || "#feed");
        if (!target) return;
        var before = target.querySelectorAll("[data-row]").length;
        var doc = new DOMParser().parseFromString(html, "text/html");
        var fresh = doc.body.firstElementChild;
        if (!fresh) return;
        if (parsed.mode === "inner") {
          target.replaceChildren();
          while (doc.body.firstChild) target.appendChild(doc.body.firstChild);
        } else target.replaceWith(fresh);
        var host = document.querySelector(parsed.selector || "#feed") || fresh;
        var after = host.querySelectorAll("[data-row]").length;
        if (!pinned) setPending(pending + Math.max(0, after - before));
        else host.scrollTop = host.scrollHeight;
      });
      function read(data) {
        var out = { elements: "", selector: "", mode: "outer" };
        var parts = [];
        var lines = data.split("\n");
        for (var i = 0; i < lines.length; i++) {
          var line = lines[i];
          if (!line) continue;
          var sp = line.indexOf(" ");
          var field = sp < 0 ? line : line.slice(0, sp);
          var value = sp < 0 ? "" : line.slice(sp + 1);
          if (field === "elements") parts.push(value);
          else if (field === "selector") out.selector = value.trim();
          else if (field === "mode") out.mode = value.trim();
        }
        out.elements = parts.join("\n");
        return out;
      }
    })();
    """

  val css: String =
    """:root {
      color-scheme: light;
      --sans: ui-sans-serif, system-ui, -apple-system, "Segoe UI", sans-serif;
      --mono: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
      --bg: #f6f3ec;
      --raised: #fffdf8;
      --ink: #1c1917;
      --muted: #44403c;
      --line: #e7e0d6;
      --accent: #9a3412;
      --accent-ink: #fff7ed;
      --accent-soft: #f8e6d4;
      --focus: #c2410c;
      --danger: #9f1239;
      --danger-soft: #fff1f2;
      --ok: #166534;
      --ok-soft: #f0fdf4;
      --warn: #92400e;
      --warn-soft: #fffbeb;
      --thought: #3730a3;
      --thought-soft: #eef2ff;
      --tool: #115e59;
      --tool-soft: #f0fdfa;
      --plan: #334155;
      --plan-soft: #f1f5f9;
      --shadow: 0 1px 2px rgb(28 25 23 / 0.06), 0 12px 32px rgb(28 25 23 / 0.05);
    }
    @media (prefers-color-scheme: dark) {
      :root {
        color-scheme: dark;
        --bg: #12110f;
        --raised: #1c1a17;
        --ink: #f5f0e8;
        --muted: #a8a29e;
        --line: #3f3a34;
        --accent: #fdba74;
        --accent-ink: #1c1917;
        --accent-soft: #3b2414;
        --focus: #fdba74;
        --danger: #fda4af;
        --danger-soft: #3f1d24;
        --ok: #86efac;
        --ok-soft: #14291c;
        --warn: #fcd34d;
        --warn-soft: #3b2a0c;
        --thought: #c7d2fe;
        --thought-soft: #1e1b4b;
        --tool: #99f6e4;
        --tool-soft: #132e2b;
        --plan: #e2e8f0;
        --plan-soft: #1e293b;
        --shadow: 0 1px 2px rgb(0 0 0 / 0.35), 0 16px 40px rgb(0 0 0 / 0.28);
      }
      html { -webkit-font-smoothing: antialiased; }
    }
    *, *::before, *::after { box-sizing: border-box; }
    html, body { margin: 0; }
    body {
      min-height: 100dvh;
      color: var(--ink);
      background:
        radial-gradient(880px 420px at 0% -10%, color-mix(in srgb, var(--accent) 14%, transparent), transparent 68%),
        var(--bg);
      font: 15px/1.5 var(--sans);
    }
    body:has(#page) { height: 100dvh; overflow: hidden; }
    ::selection { background: var(--accent-soft); color: var(--ink); }
    :focus { outline: none; }
    :focus-visible {
      outline: 2px solid var(--focus);
      outline-offset: 2px;
    }
    a { color: var(--accent); }
    button, input, textarea, select { font: inherit; color: inherit; }
    button { cursor: pointer; }
    #page {
      display: grid;
      grid-template-columns: 18rem minmax(0, 1fr);
      grid-template-rows: auto minmax(0, 1fr) auto;
      height: 100dvh;
    }
    [data-chrome="phone"] { display: none; }
    [data-chrome="desk"] { display: contents; }
    [data-chrome-part="sidebar"] {
      grid-column: 1;
      grid-row: 1 / span 3;
      display: flex;
      flex-direction: column;
      gap: 0.35rem;
      min-height: 0;
      overflow: auto;
      padding: 1.1rem 0.85rem 1.25rem;
      border-right: 1px solid var(--line);
      background: color-mix(in srgb, var(--raised) 78%, var(--bg));
    }
    [data-chrome-part="main"] {
      grid-column: 2;
      grid-row: 1;
      display: flex;
      flex-direction: column;
      gap: 0.55rem;
      min-width: 0;
      padding: 0.9rem 1.15rem 0.35rem;
    }
    #feed {
      grid-column: 2;
      grid-row: 2;
      min-width: 0;
      min-height: 0;
      overflow: auto;
      padding: 0.35rem 1.15rem 1rem;
    }
    #feed[data-follow="paused"] { box-shadow: inset 0 2px 0 var(--warn); }
    [data-composer="live"] {
      grid-column: 2;
      grid-row: 3;
      display: grid;
      gap: 0.7rem;
      min-width: 0;
      max-height: 42dvh;
      overflow: auto;
      padding: 0.6rem 1.15rem 0.75rem;
      border-top: 1px solid var(--line);
      background: color-mix(in srgb, var(--raised) 94%, var(--bg));
    }
    [data-brand="mark"] {
      display: flex;
      align-items: baseline;
      gap: 0.4rem;
      margin: 0 0 0.35rem;
      font-size: 1.05rem;
      font-weight: 700;
      letter-spacing: -0.03em;
    }
    [data-brand="mark"]::before {
      content: "";
      width: 0.55rem;
      height: 0.55rem;
      border-radius: 2px;
      background: var(--accent);
      align-self: center;
    }
    [data-brand="sub"] {
      color: var(--accent);
      font-weight: 600;
      letter-spacing: 0.01em;
    }
    [data-empty] {
      margin: 0;
      color: var(--muted);
      font-size: 0.92rem;
    }
    [data-chrome-part="sidebar"] [data-empty] { padding: 0.35rem 0.45rem; }
    #feed > [data-empty] {
      margin: auto;
      padding: 2rem 1rem;
      text-align: center;
    }
    a[data-session] {
      display: grid;
      gap: 0.12rem;
      padding: 0.55rem 0.7rem;
      border-radius: 10px;
      color: inherit;
      text-decoration: none;
      min-height: 2.75rem;
    }
    a[data-session][data-selected="true"] { background: var(--accent-soft); }
    a[data-session][data-state="ended"] { opacity: 0.72; }
    [data-session-state] {
      font-size: 0.68rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: var(--muted);
    }
    [data-state="running"] [data-session-state] { color: var(--ok); }
    [data-state="open"] [data-session-state] { color: var(--warn); }
    [data-state="ended"] [data-session-state] { color: var(--muted); }
    [data-session][data-state="running"] [data-session-state]::before {
      content: "";
      display: inline-block;
      width: 0.45rem;
      height: 0.45rem;
      margin-right: 0.35rem;
      border-radius: 50%;
      background: var(--ok);
      vertical-align: 0.05em;
    }
    [data-session-id] {
      font-family: var(--mono);
      font-size: 0.72rem;
      overflow-wrap: anywhere;
    }
    a[data-session][data-selected="true"] [data-session-id] { color: var(--accent); }
    [data-session-meta] {
      color: var(--muted);
      font-size: 0.75rem;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    [data-chrome-part="toolbar"],
    [data-chrome-part="chips"] {
      display: flex;
      flex-wrap: wrap;
      gap: 0.4rem;
      align-items: center;
    }
    [data-chrome-part="chips"] {
      flex-wrap: nowrap;
      overflow-x: auto;
      scrollbar-width: thin;
    }
    [data-chrome-part="chips"] > a { flex: none; }
    [data-chrome-part="toolbar"] [data-jump] { margin-left: auto; flex: none; }
    [data-segment],
    [data-keys="switch"] {
      display: inline-flex;
      align-items: stretch;
      border: 1px solid var(--line);
      border-radius: 999px;
      background: var(--raised);
    }
    [data-segment] a,
    [data-keys="switch"] a {
      display: inline-flex;
      align-items: center;
      padding: 0.28rem 0.7rem;
      color: var(--muted);
      text-decoration: none;
      font-size: 0.75rem;
      border-right: 1px solid var(--line);
    }
    [data-segment] a:first-child,
    [data-keys="switch"] a:first-child { border-radius: 999px 0 0 999px; }
    [data-segment] a:last-child,
    [data-keys="switch"] a:last-child { border-radius: 0 999px 999px 0; }
    [data-segment] a:first-child:last-child,
    [data-keys="switch"] a:first-child:last-child { border-radius: 999px; }
    [data-segment] a:last-child,
    [data-keys="switch"] a:last-child { border-right: 0; }
    [data-segment] a[data-on="true"],
    [data-keys="switch"] a[data-on="true"] {
      background: var(--ink);
      color: var(--bg);
    }
    a[data-chip],
    a[data-control="clear-inactive"] {
      display: inline-flex;
      align-items: center;
      min-height: 1.85rem;
      padding: 0.15rem 0.65rem;
      border: 1px solid var(--line);
      border-radius: 999px;
      background: var(--raised);
      color: var(--ink);
      text-decoration: none;
      font-family: var(--mono);
      font-size: 0.72rem;
    }
    a[data-chip][data-on="true"] {
      background: var(--accent-soft);
      border-color: transparent;
      color: var(--accent);
      font-weight: 700;
    }
    a[data-chip="Refused"][data-on="true"] {
      background: var(--danger-soft);
      color: var(--danger);
    }
    a[data-chip="tool"]::before,
    a[data-chip="status"]::before {
      margin-right: 0.35rem;
      color: var(--muted);
      font-family: var(--sans);
      font-size: 0.62rem;
      font-weight: 700;
      letter-spacing: 0.04em;
      text-transform: uppercase;
    }
    a[data-chip="tool"]::before { content: "tool"; }
    a[data-chip="status"]::before { content: "status"; }
    [data-search="q"] {
      width: 11rem;
      margin: 0;
      padding: 0.4rem 0.7rem;
      border: 1px solid var(--line);
      border-radius: 999px;
      background: var(--raised);
      color: var(--ink);
    }
    [data-jump] {
      min-height: 1.85rem;
      padding: 0.2rem 0.75rem;
      border: 1px solid var(--line);
      border-radius: 999px;
      background: var(--raised);
      color: var(--muted);
    }
    [data-jump]:not([data-pending="0"]) {
      background: var(--warn-soft);
      border-color: var(--warn);
      color: var(--warn);
      font-weight: 700;
    }
    #feed[data-layout="tree"],
    #feed[data-layout="timeline"] {
      display: flex;
      flex-direction: column;
      gap: 0.35rem;
    }
    [data-row] {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 0.15rem 0.75rem;
      margin: 0;
      padding: 0.45rem 0.75rem;
      border: 1px solid var(--line);
      border-left: 3px solid var(--kind, var(--line));
      border-radius: 12px;
      background: var(--kind-bg, var(--raised));
    }
    [data-row] > header {
      margin: 0;
      min-width: 0;
      font-size: 0.68rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      color: var(--kind, var(--muted));
      overflow-wrap: anywhere;
    }
    [data-row][data-tool]:not([data-tool=""]) > header::after {
      content: "  " attr(data-tool);
      font-family: var(--mono);
      font-size: 0.75rem;
      font-weight: 600;
      letter-spacing: 0;
      text-transform: none;
      color: var(--ink);
    }
    [data-detail-toggle] {
      align-self: start;
      color: var(--muted);
      font-size: 0.75rem;
      text-decoration: none;
    }
    [data-row] > [data-detail] {
      grid-column: 1 / -1;
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
      overflow: hidden;
      margin: 0;
      color: var(--ink);
      font-size: 0.92rem;
      line-height: 1.45;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
    }
    [data-row][data-open="true"] > [data-detail] {
      display: block;
      overflow: visible;
      -webkit-line-clamp: unset;
    }
    [data-row] > [data-row] { grid-column: 1 / -1; margin-top: 0.35rem; margin-left: 0.85rem; }
    [data-kind="Message"] { --kind: var(--accent); --kind-bg: var(--raised); }
    [data-kind="Message"] > [data-detail] { font-size: 1rem; line-height: 1.55; }
    [data-kind="Thought"] { --kind: var(--thought); --kind-bg: var(--thought-soft); }
    [data-kind="Plan"] { --kind: var(--plan); --kind-bg: var(--plan-soft); }
    [data-kind="Refused"] { --kind: var(--danger); --kind-bg: var(--danger-soft); }
    [data-kind="ToolStarted"],
    [data-kind="ToolFinished"] { --kind: var(--tool); --kind-bg: var(--tool-soft); }
    [data-kind="Output"],
    [data-kind="TestLine"] { --kind: var(--muted); --kind-bg: var(--bg); }
    [data-kind="Output"] > [data-detail],
    [data-kind="TestLine"] > [data-detail] {
      font-family: var(--mono);
      font-size: 0.82rem;
    }
    [data-kind="Output"],
    [data-kind="TestLine"],
    [data-kind="ToolStarted"],
    [data-kind="ToolFinished"] {
      padding: 0.3rem 0.65rem;
      gap: 0.05rem;
      border-radius: 8px;
    }
    [data-kind="Output"] > header,
    [data-kind="TestLine"] > header,
    [data-kind="Output"] > [data-detail-toggle],
    [data-kind="TestLine"] > [data-detail-toggle],
    [data-kind="ToolStarted"] > [data-detail],
    [data-kind="ToolStarted"] > [data-detail-toggle],
    [data-kind="ToolFinished"] > [data-detail],
    [data-kind="ToolFinished"] > [data-detail-toggle] { display: none; }
    [data-row][data-kind="Turn"] {
      display: flex;
      flex-direction: column;
      gap: 0.45rem;
      margin-top: 0.35rem;
      padding: 0.7rem 0 0.15rem;
      border: 0;
      border-top: 1px solid var(--line);
      border-radius: 0;
      background: transparent;
    }
    [data-row][data-kind="Turn"]:first-child { margin-top: 0; padding-top: 0.15rem; border-top: 0; }
    [data-row][data-kind="Turn"] > header { color: var(--muted); }
    [data-row][data-kind="Turn"] > [data-detail],
    [data-row][data-kind="Turn"] > [data-detail-toggle] { display: none; }
    #feed[data-layout="panel"] {
      display: grid;
      grid-template-columns: minmax(0, 16rem) minmax(0, 1fr);
      gap: 0.75rem;
      align-content: start;
    }
    [data-row-link] {
      display: block;
      padding: 0.45rem 0.6rem;
      border-radius: 8px;
      color: var(--ink);
      text-decoration: none;
    }
    [data-panel="detail"] {
      min-height: 8rem;
      padding: 1rem;
      border: 1px solid var(--line);
      border-radius: 12px;
      background: var(--raised);
      white-space: pre-wrap;
      overflow-wrap: anywhere;
    }
    [data-steer="turn"] { display: grid; gap: 0.55rem; }
    [data-actions] { display: flex; flex-wrap: wrap; gap: 0.45rem; align-items: center; }
    [data-actions="send"] { justify-content: flex-end; }
    [data-actions] form { display: inline-flex; margin: 0; }
    [data-steer="turn"] > form { display: grid; gap: 0.45rem; margin: 0; }
    [data-field-label] {
      display: flex;
      flex-direction: column;
      gap: 0.3rem;
      color: var(--muted);
      font-size: 0.68rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
    }
    [data-field-label] input,
    [data-field-label] textarea,
    select {
      padding: 0.55rem 0.7rem;
      border: 1px solid var(--line);
      border-radius: 10px;
      background: var(--bg);
      color: var(--ink);
      font-size: 0.95rem;
      font-weight: 450;
      letter-spacing: normal;
      text-transform: none;
    }
    textarea { width: 100%; min-height: 2.75rem; resize: vertical; }
    [data-field-label] input { width: 100%; }
    button {
      min-height: 2.25rem;
      padding: 0.4rem 0.9rem;
      border: 1px solid var(--line);
      border-radius: 10px;
      background: var(--raised);
      color: var(--ink);
    }
    button[name="mode"][value="send"],
    [data-control="new-session"] {
      background: var(--accent);
      border-color: transparent;
      color: var(--accent-ink);
      font-weight: 700;
    }
    [data-control="cancel"],
    [data-control="stop-send"] {
      background: transparent;
      border-color: var(--danger);
      color: var(--danger);
    }
    [data-control="resume"] {
      background: transparent;
      border-color: var(--ok);
      color: var(--ok);
    }
    [data-queue="prompts"]:not(:empty) { display: grid; gap: 0.4rem; }
    [data-queue="prompts"]:not(:empty)::before {
      content: "Queued";
      color: var(--warn);
      font-size: 0.68rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
    }
    [data-queue-item] {
      display: flex;
      gap: 0.5rem;
      align-items: center;
      padding: 0.55rem;
      border: 1px dashed var(--warn);
      border-radius: 12px;
      background: var(--warn-soft);
    }
    [data-queue-item] form:first-child {
      flex: 1;
      display: flex;
      gap: 0.45rem;
      align-items: center;
      min-width: 0;
    }
    [data-queue-item] textarea {
      flex: 1;
      min-width: 0;
      min-height: 2.5rem;
      background: var(--raised);
    }
    [data-queue-item] button { flex: none; }
    [data-steer="open"] {
      border: 1px solid var(--line);
      border-radius: 12px;
      background: color-mix(in srgb, var(--bg) 55%, var(--raised));
      padding: 0.2rem 0.75rem 0.75rem;
    }
    [data-steer="open"]:not([open]) { padding-bottom: 0.2rem; }
    [data-steer="open"] > summary {
      display: flex;
      align-items: center;
      min-height: 2.25rem;
      color: var(--muted);
      font-size: 0.72rem;
      font-weight: 700;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      cursor: pointer;
      list-style: none;
    }
    [data-steer="open"] > summary::-webkit-details-marker { display: none; }
    [data-steer="open"]:not([open]) > form { display: none; }
    [data-steer="open"][open] > form {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 0.65rem 0.75rem;
      align-items: end;
      margin: 0.2rem 0 0;
    }
    [data-field-label="workspace"],
    [data-field-label="mcp"] { grid-column: 1 / -1; }
    [data-steer="open"] [data-control="new-session"] { justify-self: start; }
    body:has(main[data-state="notice"]) {
      display: grid;
      place-items: center;
      min-height: 100dvh;
    }
    main[data-state="notice"] {
      width: min(28rem, calc(100% - 2.5rem));
      margin: 0;
      padding: 1.75rem 1.5rem 1.6rem;
      border: 1px solid var(--line);
      border-radius: 16px;
      background: var(--raised);
      box-shadow: var(--shadow);
      text-align: center;
    }
    main[data-state="notice"] h1 {
      margin: 0.85rem 0 0;
      font-size: 1.45rem;
      font-weight: 650;
      letter-spacing: -0.03em;
    }
    @media (max-width: 720px) {
      [data-chrome="phone"] {
        display: flex;
        flex-direction: column;
        gap: 0.55rem;
        flex: none;
        padding: 0.85rem 0.85rem 0.55rem;
        border-bottom: 1px solid var(--line);
        background: color-mix(in srgb, var(--raised) 70%, var(--bg));
      }
      [data-chrome="desk"] { display: none; }
      #page {
        display: flex;
        flex-direction: column;
        height: 100dvh;
      }
      #feed {
        order: 2;
        flex: 1 1 auto;
        min-height: 0;
        padding: 0.65rem 0.75rem 0.85rem;
      }
      [data-composer="live"] {
        order: 3;
        flex: none;
        max-height: 46dvh;
        padding: 0.7rem 0.75rem calc(0.7rem + env(safe-area-inset-bottom));
        border-radius: 16px 16px 0 0;
      }
      textarea { min-height: 3.25rem; }
      [data-control="new-session"] { position: sticky; bottom: 0; }
      [data-chrome-part="picker"] { width: 100%; min-height: 2.75rem; }
      [data-chrome-part="toolbar"] { flex-wrap: wrap; }
      [data-chrome-part="chips"] {
        flex-wrap: nowrap;
        overflow-x: auto;
        padding-bottom: 0.15rem;
        scrollbar-width: none;
      }
      a[data-chip],
      a[data-control="clear-inactive"],
      [data-keys="switch"] a,
      [data-jump],
      button,
      select {
        min-height: 2.75rem;
        flex: none;
      }
      [data-actions="send"] button,
      [data-actions="turn"] button { flex: 1 1 auto; }
      input, textarea, select { font-size: 1rem; }
      [data-search="q"] { width: 9.5rem; }
      [data-steer="open"] > summary { min-height: 2.75rem; }
      [data-steer="open"][open] > form { grid-template-columns: 1fr; }
      [data-field-label="workspace"],
      [data-field-label="mcp"] { grid-column: auto; }
      [data-layout="panel"] { display: flex; flex-direction: column; }
      [data-layout="panel"] [data-panel="detail"] {
        position: fixed;
        inset: 0;
        z-index: 3;
        overflow: auto;
        padding: 1rem;
        border: 0;
        border-radius: 0;
        background: var(--bg);
      }
      [data-queue-item] form:first-child { flex-wrap: wrap; }
      [data-queue-item] textarea { flex: 1 1 8rem; }
    }
    @media (hover: hover) and (pointer: fine) {
      a[data-session]:hover { background: color-mix(in srgb, var(--accent-soft) 65%, transparent); }
      a[data-chip]:not([data-on="true"]):hover,
      a[data-control="clear-inactive"]:hover { border-color: var(--accent); }
      [data-detail-toggle]:hover { color: var(--accent); }
      button:hover { border-color: var(--accent); }
      [data-control="cancel"]:hover,
      [data-control="stop-send"]:hover { border-color: var(--danger); }
      [data-control="resume"]:hover { border-color: var(--ok); }
      button[name="mode"][value="send"]:hover,
      [data-control="new-session"]:hover { filter: brightness(1.05); }
    }
    @media (prefers-reduced-motion: no-preference) {
      [data-session][data-state="running"] [data-session-state]::before {
        animation: terret-pulse 1.8s ease-in-out infinite;
      }
      a, button, [data-row] { transition: background-color 0.15s ease, border-color 0.15s ease, color 0.15s ease; }
    }
    @keyframes terret-pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.35; }
    }
    """
end WatchAssets
