package terret.app.watch

import ascent.ast.{Attr, UI}
import ascent.domtypes.AttrValue
import ascent.html.Html
import terret.Terret
import terret.session.{Session, SessionState}
import zio.{Chunk, UIO, ZIO}

import java.time.Instant

/** One session in the picker, with the time of its newest journal entry. */
final case class Listed(session: Session, at: Instant)

/** The read-only watch document. Controls are GET links. The composer does not call a write route. */
object WatchPage:
  def listed(terret: Terret): UIO[Chunk[Listed]] =
    terret.list
      .flatMap { sessions =>
        ZIO.foreach(sessions) { session =>
          terret
            .page(session.id, None)
            .map(page => Listed(session, page.events.lastOption.fold(Instant.EPOCH)(_.at)))
            .orElseSucceed(Listed(session, Instant.EPOCH))
        }
      }
      .map(sort)

  def sort(rows: Chunk[Listed]): Chunk[Listed] =
    rows.sortBy(row => (rank(row.session.state), -row.at.getEpochSecond, row.session.id.value))

  def visible(rows: Chunk[Listed], query: WatchQuery): Chunk[Listed] =
    rows.filter { row =>
      row.session.state match
        case SessionState.Ended(_) =>
          query.inactive == Inactive.Show && !query.gone.contains(row.session.id.value)
        case SessionState.Running | SessionState.Open => true
    }

  def endedIds(rows: Chunk[Listed]): Set[String] =
    rows.flatMap { row =>
      row.session.state match
        case SessionState.Ended(_)                    => Chunk(row.session.id.value)
        case SessionState.Running | SessionState.Open => Chunk.empty
    }.toSet

  def document(body: String, nonce: String, query: WatchQuery): String =
    val style = WatchQuery.searchCss(query).fold("")(css => s"""<style nonce="$nonce">$css</style>""")
    s"""<!DOCTYPE html><html lang="en" data-keys="${query.keys.wire}"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>terret watch</title><link rel="stylesheet" href="/watch.css">$style</head><body>$body<script src="/watch.js" defer></script></body></html>"""

  def render(
      rows: Chunk[Listed],
      selected: Option[String],
      feedRows: Chunk[WatchRow],
      query: WatchQuery,
      stream: Option[String],
  ): UIO[String] =
    Html.render(page(rows, selected, feedRows, query, stream))

  def feedHtml(
      feedRows: Chunk[WatchRow],
      selected: Option[String],
      query: WatchQuery,
      stream: Option[String],
  ): UIO[String] =
    Html.render(feed(feedRows, selected, query, stream))

  private def page(
      rows: Chunk[Listed],
      selected: Option[String],
      feedRows: Chunk[WatchRow],
      query: WatchQuery,
      stream: Option[String],
  ): UI[Any] =
    val shown    = visible(rows, query)
    val ended    = endedIds(rows)
    val tools    = feedRows.flatMap(_.tool).distinct
    val statuses = feedRows.flatMap(_.status).distinct
    el("div", "id" -> "page")(
      phone(shown, selected, query, tools, statuses, ended),
      desk(shown, selected, query, tools, statuses, ended),
      feed(feedRows, selected, query, stream),
    )

  private def phone(
      rows: Chunk[Listed],
      selected: Option[String],
      query: WatchQuery,
      tools: Chunk[String],
      statuses: Chunk[String],
      ended: Set[String],
  ): UI[Any] =
    el("div", "data-chrome" -> "phone")(
      picker(rows, selected, query),
      el("div", "data-chrome-part" -> "toolbar")(controls(selected, query, ended, compact = true)),
      el("div", "data-chrome-part" -> "chips")(chips(selected, query, tools, statuses)),
      jump,
      composer,
    )

  private def desk(
      rows: Chunk[Listed],
      selected: Option[String],
      query: WatchQuery,
      tools: Chunk[String],
      statuses: Chunk[String],
      ended: Set[String],
  ): UI[Any] =
    el("div", "data-chrome" -> "desk")(
      el("nav", "data-chrome-part" -> "sidebar")(sessionLinks(rows, selected, query)),
      el("div", "data-chrome-part" -> "main")(
        el("div", "data-chrome-part" -> "toolbar")(controls(selected, query, ended, compact = false)),
        el("div", "data-chrome-part" -> "chips")(chips(selected, query, tools, statuses)),
        jump,
        composer,
      ),
    )

  private def picker(rows: Chunk[Listed], selected: Option[String], query: WatchQuery): UI[Any] =
    el("select", "data-chrome-part" -> "picker")(
      rows.map { row =>
        val id = row.session.id.value
        option(WatchQuery.path(Some(id), query), id == selected.getOrElse(""), label(row))
      }*
    )

  private def sessionLinks(rows: Chunk[Listed], selected: Option[String], query: WatchQuery): UI[Any] =
    frag(rows.map { row =>
      val id = row.session.id.value
      el(
        "a",
        "href"          -> WatchQuery.path(Some(id), query),
        "data-session"  -> id,
        "data-selected" -> (if selected.contains(id) then "true" else "false"),
      )(t(label(row)))
    }*)

  private def controls(selected: Option[String], query: WatchQuery, ended: Set[String], compact: Boolean): UI[Any] =
    val layouts = Chunk(Layout.Tree, Layout.Timeline, Layout.Panel).map { layout =>
      link(selected, query.copy(layout = layout), "data-layout-choice" -> layout.wire, layout.wire)
    }
    val sizes = Chunk(OutputSize.Lines, OutputSize.Kb64, OutputSize.Full).map { size =>
      link(selected, query.copy(output = size), "data-output-choice" -> size.wire, size.wire)
    }
    val thinking = link(
      selected,
      WatchQuery.toggleKind(query, WatchKind.Thought),
      "data-chip" -> "thought",
      if query.kinds.contains(WatchKind.Thought) then "thinking on" else "thinking",
    )
    val inactive =
      val next = if query.inactive == Inactive.Show then Inactive.Hide else Inactive.Show
      link(selected, query.copy(inactive = next), "data-chip" -> "inactive", s"inactive ${next.wire}")
    val clear = link(
      selected,
      WatchQuery.withGone(query, ended),
      "data-control" -> "clear-inactive",
      "clear inactive",
    )
    val keys = frag(
      link(selected, query.copy(keys = Keys.Vim), "data-keys-choice"   -> "vim", "vim"),
      link(selected, query.copy(keys = Keys.Plain), "data-keys-choice" -> "plain", "plain"),
    )
    val search = el(
      "input",
      "data-search" -> "q",
      "value"       -> query.q.getOrElse(""),
      "type"        -> "search",
    )()
    if compact then frag(thinking, inactive, clear, keys, search)
    else frag(frag(layouts*), frag(sizes*), thinking, inactive, clear, keys, search)

  private def chips(
      selected: Option[String],
      query: WatchQuery,
      tools: Chunk[String],
      statuses: Chunk[String],
  ): UI[Any] =
    val kinds = Chunk.from(WatchKind.values).filter(_ != WatchKind.Turn).map { kind =>
      link(selected, WatchQuery.toggleKind(query, kind), "data-chip" -> kind.wire, kind.wire)
    }
    val toolChips = tools.map { name =>
      link(selected, query.copy(tool = Some(name)), "data-chip" -> "tool", name)
    }
    val statusChips = statuses.map { name =>
      link(selected, query.copy(status = Some(name)), "data-chip" -> "status", name)
    }
    frag(frag(kinds*), frag(toolChips*), frag(statusChips*))

  private def feed(
      rows: Chunk[WatchRow],
      selected: Option[String],
      query: WatchQuery,
      stream: Option[String],
  ): UI[Any] =
    val attrs = List(
      "id"            -> "feed",
      "data-feed"     -> query.layout.wire,
      "data-layout"   -> query.layout.wire,
      "data-output"   -> query.output.wire,
      "data-keys"     -> query.keys.wire,
      "data-follow"   -> "live",
      "data-scroller" -> "feed",
    ) ++ stream.map(value => "data-stream" -> value)
    el("div", attrs*)(
      query.layout match
        case Layout.Tree     => tree(rows, selected, query)
        case Layout.Timeline => frag(rows.map(one(_, selected, query, frag()))*)
        case Layout.Panel    => panel(rows, selected, query)
    )

  private def tree(rows: Chunk[WatchRow], selected: Option[String], query: WatchQuery): UI[Any] =
    val ids   = rows.map(_.id).toSet
    val roots = rows.filter(row => row.parent.forall(parent => !ids.contains(parent)))
    frag(roots.map(node(rows, _, selected, query))*)

  private def node(rows: Chunk[WatchRow], row: WatchRow, selected: Option[String], query: WatchQuery): UI[Any] =
    val kids = rows.filter(_.parent.contains(row.id)).map(node(rows, _, selected, query))
    one(row, selected, query, frag(kids*))

  private def panel(rows: Chunk[WatchRow], selected: Option[String], query: WatchQuery): UI[Any] =
    val current = query.row.flatMap(id => rows.find(_.id == id)).orElse(rows.headOption)
    frag(
      frag(rows.map { row =>
        link(selected, query.copy(row = Some(row.id)), "data-row-link" -> row.id, row.kind.wire)
      }*),
      el("section", "data-panel" -> "detail")(
        current match
          case None      => t("Select an event")
          case Some(row) => frag(t(row.kind.wire), el("div", "data-detail" -> row.id)(t(row.text)))
      ),
    )

  private def one(row: WatchRow, selected: Option[String], query: WatchQuery, kids: UI[Any]): UI[Any] =
    el(
      "article",
      "data-row"    -> row.id,
      "data-kind"   -> row.kind.wire,
      "data-parent" -> row.parent.getOrElse(""),
      "data-text"   -> row.text,
      "data-open"   -> (if query.open.contains(row.id) then "true" else "false"),
      "data-tool"   -> row.tool.getOrElse(""),
      "data-status" -> row.status.getOrElse(""),
    )(
      el("header")(t(row.kind.wire)),
      el("a", "href" -> WatchQuery.path(selected, WatchQuery.toggleOpen(query, row.id)))(t("details")),
      el("div", "data-detail" -> row.id)(t(row.text)),
      kids,
    )

  private val jump: UI[Any] =
    el("button", "type" -> "button", "data-jump" -> "latest", "data-pending" -> "0")(t("latest"))

  /** Placeholder only. It has no href and it does not submit. */
  private val composer: UI[Any] =
    frag(
      el("div", "data-composer" -> "inert")(t("Composer is not live yet.")),
      el("button", "type" -> "button", "data-control" -> "new-session", "disabled" -> "disabled")(
        t("New session")
      ),
    )

  private def link(selected: Option[String], query: WatchQuery, attr: (String, String), label: String): UI[Any] =
    el("a", "href" -> WatchQuery.path(selected, query), attr._1 -> attr._2)(t(label))

  private def label(row: Listed): String =
    val session = row.session
    val model   = session.model.fold("default")(_.value)
    val allow   = session.effective.allow.toList.map(_.toString).sorted.mkString(",")
    val dropped = session.dropped.toList.map(_.toString).sorted.mkString(",")
    s"${session.id.value} ${state(session.state)} ${session.workspace.path} $allow dropped:$dropped $model"

  private def state(found: SessionState): String =
    found match
      case SessionState.Open           => "Open"
      case SessionState.Running        => "Running"
      case SessionState.Ended(outcome) => s"Ended:${outcome.productPrefix}"

  private def rank(state: SessionState): Int =
    state match
      case SessionState.Running  => 0
      case SessionState.Open     => 1
      case SessionState.Ended(_) => 2

  private def option(value: String, selected: Boolean, label: String): UI[Any] =
    val attrs =
      if selected then List("value" -> value, "selected" -> "selected")
      else List("value"             -> value)
    el("option", attrs*)(t(label))

  private def el(tag: String, attrs: (String, String)*)(kids: UI[Any]*): UI[Any] =
    UI.Element[Any](
      tag,
      attrs.toVector.map((name, value) => Attr.StaticAttr(name, AttrValue.Str(value))),
      kids.toVector,
    )

  private def t(value: String): UI[Any] = UI.Text(value)

  private def frag(kids: UI[Any]*): UI[Any] =
    UI.Fragment[Any](kids.toVector)
end WatchPage
