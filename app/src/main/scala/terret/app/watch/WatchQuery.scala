package terret.app.watch

import heddle.http.QueryParams

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** One shareable view of the watch page. Absent query keys mean the defaults. */
final case class WatchQuery(
    kinds: Set[WatchKind],
    tool: Option[String],
    status: Option[String],
    q: Option[String],
    layout: Layout,
    output: OutputSize,
    inactive: Inactive,
    keys: Keys,
    gone: Set[String],
    open: Set[String],
    row: Option[String],
)

enum WatchKind:
  case ToolStarted
  case ToolFinished
  case Output
  case TestLine
  case Plan
  case Message
  case Thought
  case Turn

  def wire: String = this match
    case WatchKind.ToolStarted  => "ToolStarted"
    case WatchKind.ToolFinished => "ToolFinished"
    case WatchKind.Output       => "Output"
    case WatchKind.TestLine     => "TestLine"
    case WatchKind.Plan         => "Plan"
    case WatchKind.Message      => "Message"
    case WatchKind.Thought      => "Thought"
    case WatchKind.Turn         => "Turn"
end WatchKind

enum Layout:
  case Tree
  case Timeline
  case Panel

  def wire: String = this match
    case Layout.Tree     => "tree"
    case Layout.Timeline => "timeline"
    case Layout.Panel    => "panel"
end Layout

enum OutputSize:
  case Lines
  case Kb64
  case Full

  def wire: String = this match
    case OutputSize.Lines => "20"
    case OutputSize.Kb64  => "64k"
    case OutputSize.Full  => "full"
end OutputSize

enum Inactive:
  case Show
  case Hide

  def wire: String = this match
    case Inactive.Show => "show"
    case Inactive.Hide => "hide"
end Inactive

enum Keys:
  case Vim
  case Plain

  def wire: String = this match
    case Keys.Vim   => "vim"
    case Keys.Plain => "plain"
end Keys

enum WatchQueryError:
  case Oversized(field: String)
  case Bad(field: String)

object WatchQuery:
  val DefaultKinds: Set[WatchKind] =
    Set(
      WatchKind.ToolStarted,
      WatchKind.ToolFinished,
      WatchKind.Output,
      WatchKind.TestLine,
      WatchKind.Plan,
      WatchKind.Message,
    )

  val default: WatchQuery =
    WatchQuery(
      kinds = DefaultKinds,
      tool = None,
      status = None,
      q = None,
      layout = Layout.Tree,
      output = OutputSize.Kb64,
      inactive = Inactive.Show,
      keys = Keys.Vim,
      gone = Set.empty,
      open = Set.empty,
      row = None,
    )

  /** `after` and `datastar` are read elsewhere. Any other unknown key is refused, and so is an oversized value. */
  def parse(params: QueryParams): Either[WatchQueryError, WatchQuery] =
    if params.toMap.size > 16 then Left(WatchQueryError.Bad("query"))
    else
      oversized(params).flatMap { _ =>
        unknown(params).flatMap { _ =>
          for
            kinds    <- kindsOf(params.get("kinds"))
            tool     <- optional("tool", params.get("tool"))
            status   <- optional("status", params.get("status"))
            q        <- optional("q", params.get("q"))
            layout   <- named("layout", params.get("layout"), Layout.values, _.wire, Layout.Tree)
            output   <- named("output", params.get("output"), OutputSize.values, _.wire, OutputSize.Kb64)
            inactive <- named("inactive", params.get("inactive"), Inactive.values, _.wire, Inactive.Show)
            keys     <- named("keys", params.get("keys"), Keys.values, _.wire, Keys.Vim)
            gone     <- ids("gone", params.get("gone"))
            open     <- ids("open", params.get("open"))
            row      <- optional("row", params.get("row"))
          yield WatchQuery(kinds, tool, status, q, layout, output, inactive, keys, gone, open, row)
        }
      }

  def render(query: WatchQuery): String =
    val parts = List(
      differ("kinds", query.kinds != DefaultKinds, query.kinds.toList.map(_.wire).sorted.mkString(",")),
      query.tool.map(value => pair("tool", value)),
      query.status.map(value => pair("status", value)),
      query.q.map(value => pair("q", value)),
      differ("layout", query.layout != Layout.Tree, query.layout.wire),
      differ("output", query.output != OutputSize.Kb64, query.output.wire),
      differ("inactive", query.inactive != Inactive.Show, query.inactive.wire),
      differ("keys", query.keys != Keys.Vim, query.keys.wire),
      differ("gone", query.gone.nonEmpty, query.gone.toList.sorted.mkString(",")),
      differ("open", query.open.nonEmpty, query.open.toList.sorted.mkString(",")),
      query.row.map(value => pair("row", value)),
    ).flatten
    parts.mkString("&")

  def path(id: Option[String], query: WatchQuery): String =
    val base     = id.fold("/watch")(value => s"/watch/${enc(value)}")
    val rendered = render(query)
    if rendered.isEmpty then base else s"$base?$rendered"

  def events(id: String, query: WatchQuery, after: Option[Long]): String =
    val base     = s"/watch/${enc(id)}/events"
    val rendered = render(query)
    val cursor   = after.filter(_ >= 0).map(value => s"after=$value")
    val joined   = (cursor.toList ++ Option(rendered).filter(_.nonEmpty)).mkString("&")
    if joined.isEmpty then base else s"$base?$joined"

  /** A nonce'd style that hides rows whose text does not contain `q`. The query is CSS-escaped, so it cannot close the
    * style.
    */
  def searchCss(query: WatchQuery): Option[String] =
    query.q.map(strip).filter(_.nonEmpty).map { raw =>
      val token = raw.map(css).mkString
      "#feed [data-row]{display:none}" +
        s"""#feed [data-row][data-text*="$token"]{display:block}""" +
        s"""#feed [data-row]:has([data-text*="$token"]){display:block}"""
    }

  def toggleKind(query: WatchQuery, kind: WatchKind): WatchQuery =
    val next = if query.kinds.contains(kind) then query.kinds - kind else query.kinds + kind
    query.copy(kinds = next)

  def toggleOpen(query: WatchQuery, id: String): WatchQuery =
    val next = if query.open.contains(id) then query.open - id else query.open + id
    query.copy(open = next, row = Some(id))

  def withGone(query: WatchQuery, ids: Set[String]): WatchQuery =
    query.copy(gone = query.gone ++ ids)

  private val limits: Map[String, Int] = Map(
    "kinds"    -> 256,
    "tool"     -> 64,
    "status"   -> 32,
    "q"        -> 200,
    "layout"   -> 16,
    "output"   -> 8,
    "inactive" -> 8,
    "keys"     -> 8,
    "gone"     -> 1024,
    "open"     -> 1024,
    "row"      -> 128,
    "after"    -> 32,
    "datastar" -> 2048,
  )

  private val ignored: Set[String] = Set("after", "datastar")

  private def oversized(params: QueryParams): Either[WatchQueryError, Unit] =
    val found = params.toMap.foldLeft(Option.empty[String]) { case (soFar, (name, values)) =>
      soFar.orElse {
        val cap = limits.getOrElse(name, 256)
        if values.length > 4 || values.exists(_.length > cap) then Some(name) else None
      }
    }
    found match
      case Some(name) => Left(WatchQueryError.Oversized(name))
      case None       => Right(())

  private def unknown(params: QueryParams): Either[WatchQueryError, Unit] =
    params.toMap.keys.find(name => !limits.contains(name)) match
      case Some(name) => Left(WatchQueryError.Bad(name))
      case None       => Right(())

  private def kindsOf(raw: Option[String]): Either[WatchQueryError, Set[WatchKind]] =
    raw match
      case None       => Right(DefaultKinds)
      case Some(text) =>
        val names = text.split(",", -1).toList.map(_.trim).filter(_.nonEmpty)
        names.foldLeft[Either[WatchQueryError, Set[WatchKind]]](Right(Set.empty)) { (acc, name) =>
          acc.flatMap { set =>
            WatchKind.values.find(_.wire == name) match
              case Some(kind) => Right(set + kind)
              case None       => Left(WatchQueryError.Bad("kinds"))
          }
        }

  private def optional(field: String, raw: Option[String]): Either[WatchQueryError, Option[String]] =
    raw.map(_.trim).filter(_.nonEmpty) match
      case None        => Right(None)
      case Some(value) =>
        if ignored.contains(field) then Right(None)
        else Right(Some(value))

  private def ids(field: String, raw: Option[String]): Either[WatchQueryError, Set[String]] =
    optional(field, raw).map {
      case None        => Set.empty
      case Some(value) => value.split(",", -1).toList.map(_.trim).filter(_.nonEmpty).toSet
    }

  private def named[A](
      field: String,
      raw: Option[String],
      values: Array[A],
      wire: A => String,
      default: A,
  ): Either[WatchQueryError, A] =
    raw.map(_.trim).filter(_.nonEmpty) match
      case None        => Right(default)
      case Some(value) =>
        values.find(item => wire(item) == value) match
          case Some(found) => Right(found)
          case None        => Left(WatchQueryError.Bad(field))

  private def differ(name: String, changed: Boolean, value: String): Option[String] =
    if changed then Some(pair(name, value)) else None

  private def pair(name: String, value: String): String =
    s"${enc(name)}=${enc(value)}"

  private def enc(raw: String): String =
    URLEncoder.encode(raw, StandardCharsets.UTF_8).replace("+", "%20")

  private def strip(raw: String): String =
    raw.filter(ch => ch >= ' ' && ch != '\u007f')

  private def css(ch: Char): String =
    if ch.isLetterOrDigit || ch == '-' || ch == '_' then ch.toString
    else f"\\${ch.toInt}%x "
end WatchQuery
