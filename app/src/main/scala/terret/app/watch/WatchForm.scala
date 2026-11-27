package terret.app.watch

import heddle.http.Form
import terret.grok.{McpGrantError, McpPolicy}
import terret.permission.{Ceiling, Permission, SessionTools, ToolGrant}

/** The watch form fields. A blank allow list is `ReadWorkspace` only. A blank MCP field refuses every server. */
object WatchForm:
  val GrantLimit: Int = 1024
  val AllowLimit: Int = 256

  def prompt(form: Form): Either[SteerError, String] =
    form.get("prompt").map(_.trim).filter(_.nonEmpty) match
      case None                                    => Left(SteerError.EmptyPrompt)
      case Some(text) if text.length > Steer.PromptLimit => Left(SteerError.Oversize)
      case Some(text)                              => Right(text)

  def keys(form: Form): Keys =
    form.get("keys") match
      case Some("plain") => Keys.Plain
      case _             => Keys.Vim

  def open(form: Form): Either[SteerError, OpenRequest] =
    val workspace = form.get("workspace").map(_.trim).getOrElse("")
    val model     = form.get("model").map(_.trim).filter(_.nonEmpty)
    for
      allow <- allowOf(form)
      tools <- toolsOf(form)
    yield OpenRequest(workspace, model, allow, tools)

  private def allowOf(form: Form): Either[SteerError, Ceiling] =
    form.get("allow").map(_.trim).filter(_.nonEmpty) match
      case None                                  => Right(Ceiling.closed)
      case Some(raw) if raw.length > AllowLimit => Left(SteerError.BadAllow("allow"))
      case Some(raw)                            =>
        raw
          .split(",", -1)
          .toList
          .map(_.trim)
          .filter(_.nonEmpty)
          .foldLeft[Either[SteerError, Set[Permission]]](Right(Set.empty)) { (acc, name) =>
            acc.flatMap { kept =>
              Permission.values.find(_.productPrefix == name) match
                case Some(permission) => Right(kept + permission)
                case None             => Left(SteerError.BadAllow(name))
            }
          }
          .map(Ceiling(_))

  private def toolsOf(form: Form): Either[SteerError, SessionTools] =
    val raw = form.get("mcp").map(_.trim).getOrElse("")
    if raw.length > GrantLimit then Left(SteerError.Mcp("too long"))
    else if raw.isEmpty then Right(SessionTools.refused)
    else
      McpPolicy.parse(raw) match
        case Left(error) => Left(SteerError.Mcp(spoken(error)))
        case Right(policy) =>
          Right(SessionTools.Chosen(policy.grants.map(grant => ToolGrant(grant.server, grant.tools))))

  private def spoken(error: McpGrantError): String =
    error match
      case McpGrantError.EmptyName      => "empty name"
      case McpGrantError.EmptyTool(_)   => "empty tool"
      case McpGrantError.BadToken(_)    => "bad token"
      case McpGrantError.Duplicate(_)   => "duplicate"
end WatchForm
