package terret.app.watch

import zio.test.*

object WatchKeysSpec extends ZIOSpecDefault:
  def spec = suite("watch keys")(
    test("vim scrolls, opens, searches, inserts, and blurs") {
      assertTrue(
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "j", ctrl = false).contains(KeyAction.ScrollDown),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "k", ctrl = false).contains(KeyAction.ScrollUp),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "Enter", ctrl = false).contains(KeyAction.Open),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "/", ctrl = false).contains(KeyAction.Search),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "i", ctrl = false).contains(KeyAction.Insert),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "Escape", ctrl = false).contains(KeyAction.Blur),
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "ArrowDown", ctrl = false).isEmpty,
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "j", ctrl = true).isEmpty,
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "j", ctrl = false, meta = true).isEmpty,
        WatchKeys.decide(Keys.Vim, KeyFocus.Page, "j", ctrl = false, alt = true).isEmpty,
      )
    },
    test("plain uses arrows and the visible chords") {
      assertTrue(
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "ArrowDown", ctrl = false).contains(KeyAction.ScrollDown),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "ArrowUp", ctrl = false).contains(KeyAction.ScrollUp),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "Enter", ctrl = false).contains(KeyAction.Open),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "f", ctrl = true).contains(KeyAction.Search),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "F", ctrl = true).contains(KeyAction.Search),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "i", ctrl = true).contains(KeyAction.Insert),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "I", ctrl = true).contains(KeyAction.Insert),
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "j", ctrl = false).isEmpty,
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "f", ctrl = false).isEmpty,
        WatchKeys.decide(Keys.Plain, KeyFocus.Page, "ArrowDown", ctrl = false, meta = true).isEmpty,
      )
    },
    test("a field yields every key except escape") {
      assertTrue(
        WatchKeys.decide(Keys.Vim, KeyFocus.Field, "j", ctrl = false).isEmpty,
        WatchKeys.decide(Keys.Plain, KeyFocus.Field, "ArrowDown", ctrl = false).isEmpty,
        WatchKeys.decide(Keys.Vim, KeyFocus.Field, "Escape", ctrl = false).contains(KeyAction.Blur),
        WatchKeys.decide(Keys.Plain, KeyFocus.Field, "Escape", ctrl = false).contains(KeyAction.Blur),
        WatchKeys.decide(Keys.Vim, KeyFocus.Field, "Escape", ctrl = true).isEmpty,
      )
    },
    test("the page script is the same table and does not name a mutation") {
      val script = WatchAssets.js
      assertTrue(
        script.contains("""ev.key === "j""""),
        script.contains("""ev.key === "k""""),
        script.contains("""ev.key === "/""""),
        script.contains("""ev.key === "ArrowDown""""),
        script.contains("""ev.key === "ArrowUp""""),
        script.contains("ev.ctrlKey && !ev.metaKey && !ev.altKey"),
        script.contains("""mode === "vim""""),
        script.contains("""mode === "plain""""),
        script.contains("watchScroll(48)"),
        script.contains("watchScroll(-48)"),
        !script.contains("cancel"),
      )
    },
  )
end WatchKeysSpec
