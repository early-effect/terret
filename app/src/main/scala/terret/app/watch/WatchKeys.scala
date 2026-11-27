package terret.app.watch

import zio.Chunk

/** What a key does on the watch page when it is not typing into a field. */
enum KeyAction:
  case ScrollDown
  case ScrollUp
  case Open
  case Search
  case Insert
  case Blur

enum KeyFocus:
  case Field
  case Page

/** One binding. `ctrl` is the visible chord. Meta and alt never match. */
final case class KeyBinding(keys: Keys, key: String, ctrl: Boolean, action: KeyAction)

/** The keyboard law. The page script is generated from `bindings`, so the chords in the browser are these ones. */
object WatchKeys:
  val bindings: Chunk[KeyBinding] = Chunk(
    KeyBinding(Keys.Vim, "j", false, KeyAction.ScrollDown),
    KeyBinding(Keys.Vim, "k", false, KeyAction.ScrollUp),
    KeyBinding(Keys.Vim, "Enter", false, KeyAction.Open),
    KeyBinding(Keys.Vim, "/", false, KeyAction.Search),
    KeyBinding(Keys.Vim, "i", false, KeyAction.Insert),
    KeyBinding(Keys.Vim, "Escape", false, KeyAction.Blur),
    KeyBinding(Keys.Plain, "ArrowDown", false, KeyAction.ScrollDown),
    KeyBinding(Keys.Plain, "ArrowUp", false, KeyAction.ScrollUp),
    KeyBinding(Keys.Plain, "Enter", false, KeyAction.Open),
    KeyBinding(Keys.Plain, "f", true, KeyAction.Search),
    KeyBinding(Keys.Plain, "F", true, KeyAction.Search),
    KeyBinding(Keys.Plain, "i", true, KeyAction.Insert),
    KeyBinding(Keys.Plain, "I", true, KeyAction.Insert),
  )

  def decide(
      keys: Keys,
      focus: KeyFocus,
      key: String,
      ctrl: Boolean,
      meta: Boolean = false,
      alt: Boolean = false,
  ): Option[KeyAction] =
    if meta || alt then None
    else if focus == KeyFocus.Field then
      if key == "Escape" && !ctrl then Some(KeyAction.Blur) else None
    else
      bindings.find(binding => binding.keys == keys && binding.key == key && binding.ctrl == ctrl).map(_.action)

  /** The key handler the page installs. Field focus yields every key except Escape. */
  def dispatch: String =
    val arms = bindings.map { binding =>
      val ctrl =
        if binding.ctrl then "ev.ctrlKey && !ev.metaKey && !ev.altKey"
        else "!ev.ctrlKey && !ev.metaKey && !ev.altKey"
      s"""if (mode === "${binding.keys.wire}" && ev.key === "${binding.key}" && $ctrl) { ${effect(binding.action)} return; }"""
    }.mkString("\n        ")
    s"""function watchKey(ev) {
        var mode = document.documentElement.getAttribute("data-keys") || "vim";
        var tag = ev.target && ev.target.tagName;
        if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT") {
          if (ev.key === "Escape" && !ev.ctrlKey && !ev.metaKey && !ev.altKey) ev.target.blur();
          return;
        }
        $arms
      }
      function watchOpen() {
        var link = document.querySelector("[data-selected='true']");
        if (link && link.getAttribute("href")) location.assign(link.getAttribute("href"));
      }
      function watchSearch() {
        var node = document.querySelector("[data-search='q']");
        if (node) node.focus();
      }
      function watchInsert() {
        var node = document.querySelector("[data-composer] textarea");
        if (node) node.focus();
      }
      function watchBlur() {
        if (document.activeElement && document.activeElement.blur) document.activeElement.blur();
      }
      function watchScroll(delta) {
        scroller.scrollTop += delta;
      }
    """

  private def effect(action: KeyAction): String =
    action match
      case KeyAction.ScrollDown => "watchScroll(48);"
      case KeyAction.ScrollUp   => "watchScroll(-48);"
      case KeyAction.Open       => "watchOpen();"
      case KeyAction.Search     => "watchSearch();"
      case KeyAction.Insert     => "watchInsert();"
      case KeyAction.Blur       => "watchBlur();"
end WatchKeys
