package terret.grok

import zio.Chunk

/** One opted-in server. `tools` empty of a value means every tool. `None` is that. `Some` is only those tools. */
final case class McpGrant(server: String, tools: Option[Set[String]]):
  def allows(tool: String): Boolean =
    tools match
      case None       => true
      case Some(kept) => kept.contains(tool)

/** Denied by default. An empty chunk grants nothing. */
final case class McpPolicy(grants: Chunk[McpGrant]):
  def grant(server: String): Option[McpGrant] =
    grants.find(_.server == server)

  def granted(servers: Chunk[McpServer]): Chunk[McpServer] =
    val names = grants.map(_.server).toSet
    servers.filter(server => names.contains(server.server))
end McpPolicy

object McpPolicy:
  val empty: McpPolicy = McpPolicy(Chunk.empty)

  def parse(raw: String): Either[McpGrantError, McpPolicy] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Right(empty)
    else
      val parts = trimmed.split(",", -1).toList
      parts
        .foldLeft[Either[McpGrantError, List[McpGrant]]](Right(Nil)): (acc, part) =>
          acc.flatMap: grants =>
            entry(part.trim).flatMap: grant =>
              if grants.exists(_.server == grant.server) then Left(McpGrantError.Duplicate(grant.server))
              else Right(grants :+ grant)
        .map(grants => McpPolicy(Chunk.fromIterable(grants)))

  private def entry(raw: String): Either[McpGrantError, McpGrant] =
    if raw.isEmpty then Left(McpGrantError.BadToken(raw))
    else
      val colon = raw.indexOf(':')
      if colon < 0 then token(raw).map(name => McpGrant(name, None))
      else if colon == 0 then Left(McpGrantError.EmptyName)
      else
        val name  = raw.take(colon)
        val tools = raw.drop(colon + 1)
        if tools.isEmpty then token(name).flatMap(server => Left(McpGrantError.EmptyTool(server)))
        else
          for
            server <- token(name)
            parsed <- toolList(tools)
          yield McpGrant(server, Some(parsed))

  private def toolList(raw: String): Either[McpGrantError, Set[String]] =
    val parts = raw.split("\\+", -1).toList
    parts.foldLeft[Either[McpGrantError, Set[String]]](Right(Set.empty)): (acc, part) =>
      acc.flatMap: tools =>
        token(part).map(tool => tools + tool)

  private def token(raw: String): Either[McpGrantError, String] =
    if raw.nonEmpty && head(raw.charAt(0)) && raw.forall(tail) then Right(raw)
    else Left(McpGrantError.BadToken(raw))

  private def head(char: Char): Boolean =
    char.isLetterOrDigit

  private def tail(char: Char): Boolean =
    char.isLetterOrDigit || char == '_' || char == '-'
end McpPolicy

enum McpGrantError:
  case EmptyName
  case EmptyTool(server: String)
  case BadToken(value: String)
  case Duplicate(server: String)

  def render: String = this match
    case EmptyName         => "an MCP grant needs a server name"
    case EmptyTool(server) => s"$server grants no tool. Omit the colon to grant the server"
    case BadToken(value)   => s"'$value' is not an MCP grant"
    case Duplicate(server) => s"$server is granted twice"
end McpGrantError

/** What a permission request may see: the grants, and the server names the catalog defined. */
final case class McpAccess(policy: McpPolicy, defined: Set[String])

object McpAccess:
  val closed: McpAccess = McpAccess(McpPolicy.empty, Set.empty)
