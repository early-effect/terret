package terret.app.watch

/** The only scripts and styles the watch page loads. Both are same-origin and fixed. There is no directory listing. */
object WatchAssets:
  val js: String =
    """(function () {
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
      var search = document.querySelector("[data-search='q']");
      if (search) {
        search.addEventListener("change", function () {
          var url = new URL(location.href);
          if (search.value) url.searchParams.set("q", search.value);
          else url.searchParams.delete("q");
          location.assign(url.toString());
        });
      }
      document.addEventListener("keydown", function (ev) {
        var mode = document.documentElement.getAttribute("data-keys");
        var tag = ev.target && ev.target.tagName;
        if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT") return;
        if (mode === "vim" && ev.key === "j") scroller.scrollTop += 48;
        if (mode === "vim" && ev.key === "k") scroller.scrollTop -= 48;
        if (mode === "plain" && ev.key === "ArrowDown") scroller.scrollTop += 48;
        if (mode === "plain" && ev.key === "ArrowUp") scroller.scrollTop -= 48;
      });
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
    """html,body{margin:0;font:15px/1.4 ui-sans-serif,system-ui,sans-serif;background:#0f1419;color:#e7ecf1}
      a{color:#8ec5ff}
      [data-chrome="phone"]{display:none}
      [data-chrome="desk"]{display:flex;gap:1rem;align-items:flex-start}
      [data-chrome-part="sidebar"]{width:16rem;flex:none}
      [data-chrome-part="toolbar"],[data-chrome-part="chips"]{display:flex;flex-wrap:wrap;gap:.4rem;align-items:center}
      [data-chip]{border:1px solid #2c3844;border-radius:999px;padding:.15rem .55rem;text-decoration:none}
      #feed{overflow:auto;max-height:70vh;border:1px solid #2c3844;padding:.5rem}
      [data-row]{border-top:1px solid #24303a;padding:.35rem 0}
      [data-row] [data-detail]{display:none;white-space:pre-wrap}
      [data-row][data-open="true"] [data-detail]{display:block}
      [data-follow="paused"]{color:#e6c07b}
      [data-composer="inert"]{opacity:.55;border:1px dashed #2c3844;padding:.6rem;margin-top:.6rem}
      [data-control="new-session"]{margin-top:.4rem}
      @media (max-width:720px){
        [data-chrome="phone"]{display:flex;flex-direction:column;gap:.4rem}
        [data-chrome="desk"]{display:none}
        [data-chrome-part="picker"]{width:100%}
        [data-layout="panel"] [data-panel="detail"]{position:fixed;inset:0;background:#0f1419;z-index:2;overflow:auto;padding:1rem}
        [data-composer="inert"]{position:sticky;bottom:0;background:#0f1419}
      }
    """
end WatchAssets
