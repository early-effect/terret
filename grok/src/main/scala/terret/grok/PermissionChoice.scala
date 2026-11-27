package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.{Chunk, UIO, ZIO}

import java.nio.file.Path

enum PermissionAnswer:
  case Selected(optionId: String)
  case Refused(optionId: String, callId: String, reason: String)
  case Cancelled

object PermissionChoice:
  val readRootsKey  = "terret.readRoots"
  val writeRootsKey = "terret.writeRoots"

  def decide(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      readRoots: Chunk[Workspace],
      writeRoots: Chunk[Workspace],
      params: Json,
      files: FileRoots,
      access: McpAccess = McpAccess.closed,
      hosts: NetworkPolicy = NetworkPolicy.empty,
      home: Path = FileRoots.home,
  ): UIO[PermissionAnswer] =
    val tool = toolOf(params)
    val kind = JsonFields.str(tool, "kind").getOrElse("")
    allows(policy, ceiling, workspace, readRoots, writeRoots, tool, kind, access, hosts, files, home).map { decision =>
      pick(options(params), decision, callId(tool))
    }

  def statedName(params: Json): String =
    val tool = toolOf(params)
    label(tool, JsonFields.str(tool, "kind").getOrElse(""))

  def body(answer: PermissionAnswer): Json =
    answer match
      case PermissionAnswer.Selected(optionId) => selected(optionId)
      case PermissionAnswer.Refused(optionId, _, reason) =>
        Json.Obj(
          "outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str(optionId)),
          "_meta"   -> Json.Obj("followup_message" -> Json.Str(reason)),
        )
      case PermissionAnswer.Cancelled =>
        Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled")))

  private def selected(optionId: String): Json =
    Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str(optionId)))

  private def toolOf(params: Json): Json.Obj =
    JsonFields
      .obj(params)
      .flatMap(JsonFields.field(_, "toolCall"))
      .flatMap(JsonFields.obj)
      .getOrElse(Json.Obj())

  private def allows(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      readRoots: Chunk[Workspace],
      writeRoots: Chunk[Workspace],
      tool: Json.Obj,
      kind: String,
      access: McpAccess,
      hosts: NetworkPolicy,
      files: FileRoots,
      home: Path,
  ): UIO[Decision] =
    if networkKind(kind) || webTool(tool) then ZIO.succeed(networkDecision(policy, ceiling, tool, kind, hosts))
    else
      val call = mcpCall(tool)
      policy match
        case PermissionPolicy.Inherit =>
          call match
            case Some(found) => ZIO.succeed(mcpDecision(access, found))
            case None        =>
              route(policy, ceiling, workspace, readRoots, writeRoots, tool, kind, files, home, inherited = true)
        case _ =>
          call match
            case Some(found) => ZIO.succeed(mcpDecision(access, found))
            case None        =>
              route(policy, ceiling, workspace, readRoots, writeRoots, tool, kind, files, home, inherited = false)

  private def route(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      readRoots: Chunk[Workspace],
      writeRoots: Chunk[Workspace],
      tool: Json.Obj,
      kind: String,
      files: FileRoots,
      home: Path,
      inherited: Boolean,
  ): UIO[Decision] =
    val name = label(tool, kind)
    normalize(kind) match
      case "read" | "search" | "grep" | "glob" | "list" =>
        confine(
          policy,
          ceiling,
          Permission.ReadWorkspace,
          name,
          "reads",
          readRootsKey,
          locations(tool),
          workspace,
          readRoots,
          files,
          home,
        )
      case "edit" | "write" | "delete" | "move" | "rename" | "create" =>
        confine(
          policy,
          ceiling,
          Permission.WriteWorkspace,
          name,
          "writes",
          writeRootsKey,
          locations(tool),
          workspace,
          writeRoots,
          files,
          home,
        )
      case "execute" | "bash" | "terminal" =>
        shell(policy, ceiling, tool, workspace, writeRoots, files, home)
      case "think" | "thought" => ZIO.succeed(Decision.Allow)
      case _ if locations(tool).nonEmpty =>
        val mode = if readOnly(tool) then Permission.ReadWorkspace else Permission.WriteWorkspace
        val verb = if mode == Permission.ReadWorkspace then "reads" else "writes"
        val key  = if mode == Permission.ReadWorkspace then readRootsKey else writeRootsKey
        val extra = if mode == Permission.ReadWorkspace then readRoots else writeRoots
        confine(policy, ceiling, mode, name, verb, key, locations(tool), workspace, extra, files, home)
      case _ if inherited => ZIO.succeed(Decision.Allow)
      case _              => ZIO.succeed(Decision.Deny(s"$name needs a granted permission"))

  private def networkDecision(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      tool: Json.Obj,
      kind: String,
      hosts: NetworkPolicy,
  ): Decision =
    val name = label(tool, kind)
    policy match
      case PermissionPolicy.Inherit => hostDecision(hosts, tool, name)
      case _                        =>
        grant(policy, ceiling, Permission.Network, name) match
          case Decision.Allow        => hostDecision(hosts, tool, name)
          case denied: Decision.Deny => denied

  private def hostDecision(hosts: NetworkPolicy, tool: Json.Obj, name: String): Decision =
    hostOf(tool) match
      case Some(host) if hosts.allows(host) => Decision.Allow
      case Some(host)                       => Decision.Deny(s"$host needs a network grant in terret.network")
      case None                             => Decision.Deny(s"$name needs a host grant in terret.network")

  private def hostOf(tool: Json.Obj): Option[String] =
    JsonFields.field(tool, "rawInput").flatMap(JsonFields.obj).flatMap { raw =>
      urlHost(raw).orElse {
        JsonFields.field(raw, "tool_input").flatMap(JsonFields.obj).flatMap(urlHost)
      }
    }

  private def urlHost(raw: Json.Obj): Option[String] =
    JsonFields.str(raw, "url").filter(_.nonEmpty).flatMap(hostIn)

  private def hostIn(raw: String): Option[String] =
    try
      val host = java.net.URI.create(raw).getHost
      if host == null || host.isEmpty then None
      else
        val folded = host.toLowerCase.stripSuffix(".")
        if folded.isEmpty then None else Some(folded)
    catch case _: IllegalArgumentException => None

  private def shell(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      tool: Json.Obj,
      workspace: Workspace,
      writeRoots: Chunk[Workspace],
      files: FileRoots,
      home: Path,
  ): UIO[Decision] =
    val name = label(tool, "execute")
    if !ceiling.allow.contains(Permission.RunCommands) then
      ZIO.succeed(Decision.Deny(missing(policy, Permission.RunCommands, name)))
    else
      shellBase(tool, workspace, files, home).flatMap {
        case ShellBase.Off(raw) => ZIO.succeed(Decision.Deny(s"$name cwd is $raw, outside the workspace"))
        case ShellBase.Ok(base) =>
          val effect  = command(tool).fold(ShellEffect.Opaque)(ShellWrite.effect)
          val located = locations(tool)
          val writing = effect match
            case ShellEffect.ReadOnly  => located.nonEmpty
            case ShellEffect.Writes(_) => true
            case ShellEffect.Opaque    => true
          if !writing then ZIO.succeed(Decision.Allow)
          else if !ceiling.allow.contains(Permission.WriteWorkspace) then
            ZIO.succeed(Decision.Deny(missing(policy, Permission.WriteWorkspace, name)))
          else
            val targets = effect match
              case ShellEffect.Writes(paths) => paths ++ located
              case _                         => located
            each(
              name,
              "writes",
              writeRootsKey,
              targets,
              base,
              rootPaths(workspace, writeRoots),
              home,
              files,
            )
      }

  private def confine(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      permission: Permission,
      name: String,
      verb: String,
      key: String,
      targets: Chunk[String],
      workspace: Workspace,
      extra: Chunk[Workspace],
      files: FileRoots,
      home: Path,
  ): UIO[Decision] =
    if !ceiling.allow.contains(permission) then ZIO.succeed(Decision.Deny(missing(policy, permission, name)))
    else each(name, verb, key, targets, workspace.path, rootPaths(workspace, extra), home, files)

  private def each(
      name: String,
      verb: String,
      key: String,
      targets: Chunk[String],
      base: Path,
      roots: Chunk[Path],
      home: Path,
      files: FileRoots,
  ): UIO[Decision] =
    def go(rest: Chunk[String]): UIO[Decision] =
      rest.headOption match
        case None => ZIO.succeed(Decision.Allow)
        case Some(target) =>
          placed(target, base, roots, home, files).flatMap {
            case false => ZIO.succeed(Decision.Deny(outside(name, verb, target, key)))
            case true  => go(rest.drop(1))
          }
    go(targets)

  private def outside(name: String, verb: String, path: String, key: String): String =
    s"$name $verb $path, outside the workspace; grant it in $key"

  /** A path outside every root is refused here and is not resolved. */
  private def placed(
      target: String,
      base: Path,
      roots: Chunk[Path],
      home: Path,
      files: FileRoots,
  ): UIO[Boolean] =
    FileRoots.lexical(target, base, home) match
      case None => ZIO.succeed(false)
      case Some(abs) =>
        val cover = roots.filter(root => abs.startsWith(root))
        if cover.isEmpty then ZIO.succeed(false)
        else
          files.resolve(abs).flatMap {
            case None => ZIO.succeed(false)
            case Some(real) =>
              ZIO.foreach(cover)(files.resolve).map { resolved =>
                resolved.flatten.exists(root => real.startsWith(root))
              }
          }

  private def shellBase(tool: Json.Obj, workspace: Workspace, files: FileRoots, home: Path): UIO[ShellBase] =
    val raw = JsonFields
      .field(tool, "rawInput")
      .flatMap(JsonFields.obj)
      .flatMap(obj => JsonFields.str(obj, "cwd").filter(_.nonEmpty))
    raw match
      case None => ZIO.succeed(ShellBase.Ok(workspace.path))
      case Some(text) =>
        placed(text, workspace.path, Chunk(workspace.path.normalize), home, files).flatMap {
          case false => ZIO.succeed(ShellBase.Off(text))
          case true  =>
            ZIO.succeed(ShellBase.Ok(FileRoots.lexical(text, workspace.path, home).getOrElse(workspace.path)))
        }

  private def grant(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      permission: Permission,
      name: String,
  ): Decision =
    if ceiling.allow.contains(permission) then Decision.Allow
    else Decision.Deny(missing(policy, permission, name))

  private def missing(policy: PermissionPolicy, permission: Permission, name: String): String =
    val base = s"$name needs $permission"
    if clamped(policy, permission) then s"$base, clamped by server policy" else base

  private def clamped(policy: PermissionPolicy, permission: Permission): Boolean =
    policy match
      case PermissionPolicy.Inherit    => false
      case PermissionPolicy.Closed     => permission != Permission.ReadWorkspace
      case PermissionPolicy.Clamp(cap) => !cap.allow.contains(permission)

  private def command(tool: Json.Obj): Option[String] =
    rawCommand(tool)
      .orElse(JsonFields.field(tool, "content").flatMap(JsonFields.text).filter(_.nonEmpty))
      .orElse(JsonFields.str(tool, "title").filter(_.nonEmpty))

  private def rawCommand(tool: Json.Obj): Option[String] =
    JsonFields.field(tool, "rawInput").flatMap(JsonFields.obj).flatMap { raw =>
      JsonFields.field(raw, "command") match
        case Some(Json.Str(text)) if text.nonEmpty => Some(text)
        case Some(Json.Arr(values))                =>
          val parts = values.collect { case Json.Str(text) if text.nonEmpty => text }
          if parts.isEmpty then None else Some(parts.mkString(" "))
        case _ => JsonFields.str(raw, "cmd").filter(_.nonEmpty)
    }

  private def locations(tool: Json.Obj): Chunk[String] =
    listed(tool) ++ rawPaths(tool)

  private def listed(tool: Json.Obj): Chunk[String] =
    JsonFields.field(tool, "locations") match
      case Some(Json.Arr(values)) =>
        values.flatMap { item =>
          JsonFields.obj(item).flatMap(obj => JsonFields.str(obj, "path").filter(_.nonEmpty)) match
            case Some(path) => Chunk(path)
            case None       => Chunk.empty
        }
      case _ => Chunk.empty

  // Path keys only. content, url, and command are not paths.
  private def rawPaths(tool: Json.Obj): Chunk[String] =
    JsonFields.field(tool, "rawInput").fold(Chunk.empty)(pathsIn)

  private def pathsIn(json: Json): Chunk[String] =
    json match
      case obj: Json.Obj =>
        obj.fields.flatMap { (key, value) =>
          if pathKey(key) then pathValues(value) else pathsIn(value)
        }
      case Json.Arr(values) => values.flatMap(pathsIn)
      case _                => Chunk.empty

  private def pathValues(json: Json): Chunk[String] =
    json match
      case Json.Str(text) if text.nonEmpty => Chunk(text)
      case Json.Arr(values)                => values.flatMap(pathValues)
      case obj: Json.Obj                   => pathsIn(obj)
      case _                               => Chunk.empty

  private def pathKey(name: String): Boolean =
    val key = normalize(name).replace("_", "")
    key == "path" || key == "paths" || key.endsWith("path")

  private def rootPaths(workspace: Workspace, extra: Chunk[Workspace]): Chunk[Path] =
    (workspace +: extra).map(_.path.normalize)

  private def readOnly(tool: Json.Obj): Boolean =
    JsonFields.field(tool, "_meta").flatMap(JsonFields.obj).flatMap { meta =>
      JsonFields.field(meta, "x.ai/tool").flatMap(JsonFields.obj).flatMap(found => JsonFields.field(found, "read_only"))
    } match
      case Some(Json.Bool(value)) => value
      case _                      => false

  private def mcpDecision(access: McpAccess, call: McpCall): Decision =
    call match
      case McpCall.Search =>
        if access.policy.grants.isEmpty then Decision.Deny("search_tool needs an MCP grant in terret.mcp")
        else Decision.Allow
      case McpCall.Named(server, tool) =>
        access.policy.grant(server) match
          case None                               => Decision.Deny(s"$server needs an MCP grant in terret.mcp")
          case Some(grant) if !grant.allows(tool) =>
            Decision.Deny(s"$tool needs an MCP grant on $server")
          case Some(_) if !access.defined.contains(server) =>
            Decision.Deny(s"$server is granted but .mcp.json does not define it")
          case Some(_) => Decision.Allow

  private def mcpCall(tool: Json.Obj): Option[McpCall] =
    val own = Chunk(
      JsonFields.str(tool, "name"),
      JsonFields.str(tool, "toolName"),
      metaName(tool),
      JsonFields.str(tool, "title"),
    ).flatten
    if own.exists(name => normalize(name) == "search_tool") then Some(McpCall.Search)
    else
      val candidates = own ++ rawToolName(tool).fold(Chunk.empty)(Chunk(_)) ++ variant(tool).fold(Chunk.empty)(Chunk(_))
      candidates.collectFirst(Function.unlift(splitQualified))

  private def splitQualified(raw: String): Option[McpCall.Named] =
    val stripped = if raw.startsWith("mcp__") then raw.drop("mcp__".length) else raw
    val at       = stripped.indexOf("__")
    if at <= 0 || at + 2 >= stripped.length then None
    else Some(McpCall.Named(stripped.take(at), stripped.drop(at + 2)))

  private def label(tool: Json.Obj, kind: String): String =
    networkLabel(tool)
      .orElse(JsonFields.str(tool, "name").filter(_.nonEmpty))
      .orElse(JsonFields.str(tool, "toolName").filter(_.nonEmpty))
      .orElse(metaName(tool))
      .orElse(variant(tool).map(wireName))
      .getOrElse {
        normalize(kind) match
          case "execute" | "bash" | "terminal" => "run_terminal_command"
          case "fetch" | "web_fetch"           => "web_fetch"
          case "web_search"                    => "web_search"
          case other                           => other
      }

  private def networkLabel(tool: Json.Obj): Option[String] =
    rawToolName(tool).filter(networkName).map(wireName)

  private def rawToolName(tool: Json.Obj): Option[String] =
    JsonFields
      .field(tool, "rawInput")
      .flatMap(JsonFields.obj)
      .flatMap(raw => JsonFields.str(raw, "tool_name").filter(_.nonEmpty))

  private def metaName(tool: Json.Obj): Option[String] =
    JsonFields.field(tool, "_meta").flatMap(JsonFields.obj).flatMap { meta =>
      JsonFields
        .field(meta, "x.ai/tool")
        .flatMap(JsonFields.obj)
        .flatMap(found => JsonFields.str(found, "name").filter(_.nonEmpty))
    }

  private def variant(tool: Json.Obj): Option[String] =
    JsonFields
      .field(tool, "rawInput")
      .flatMap(JsonFields.obj)
      .flatMap(raw => JsonFields.str(raw, "variant").filter(_.nonEmpty))

  private def wireName(raw: String): String =
    normalize(raw).replace("_", "") match
      case "webfetch"         => "web_fetch"
      case "websearch"        => "web_search"
      case "xsearch"          => "x_search"
      case "imageedit"        => "image_edit"
      case "imagegen"         => "image_gen"
      case "imagetovideo"     => "image_to_video"
      case "referencetovideo" => "reference_to_video"
      case _                  => normalize(raw)

  private def webTool(tool: Json.Obj): Boolean =
    val names = Chunk(
      JsonFields.str(tool, "name"),
      JsonFields.str(tool, "toolName"),
      metaName(tool),
      variant(tool),
      rawToolName(tool),
    ).flatten
    names.exists(networkName)

  private def networkName(raw: String): Boolean =
    normalize(raw).replace("_", "") match
      case "webfetch" | "websearch" | "xsearch" | "imageedit" | "imagegen" | "imagetovideo" | "referencetovideo" =>
        true
      case _ => false

  private def networkKind(kind: String): Boolean =
    normalize(kind) match
      case "fetch" | "web_fetch" | "web_search" => true
      case _                                    => false

  private def callId(tool: Json.Obj): String =
    JsonFields.str(tool, "toolCallId").getOrElse("")

  private def pick(options: Chunk[Choice], decision: Decision, callId: String): PermissionAnswer =
    val wanted = decision match
      case Decision.Allow =>
        options
          .find(choice => normalize(choice.kind) == "allow_once")
          .orElse(options.find(choice => isAllow(choice.kind)))
      case Decision.Deny(_) =>
        options
          .find(choice => normalize(choice.kind) == "reject_once")
          .orElse(options.find(choice => isReject(choice.kind)))
    wanted match
      case Some(choice) =>
        decision match
          case Decision.Allow        => PermissionAnswer.Selected(choice.optionId)
          case Decision.Deny(reason) => PermissionAnswer.Refused(choice.optionId, callId, reason)
      case None => PermissionAnswer.Cancelled

  private def options(params: Json): Chunk[Choice] =
    JsonFields.obj(params).flatMap(JsonFields.field(_, "options")) match
      case Some(Json.Arr(values)) =>
        values.flatMap { item =>
          JsonFields.obj(item) match
            case Some(obj) =>
              JsonFields.str(obj, "optionId").filter(_.nonEmpty) match
                case Some(optionId) => Chunk(Choice(optionId, JsonFields.str(obj, "kind").getOrElse("")))
                case None           => Chunk.empty
            case None => Chunk.empty
        }
      case _ => Chunk.empty

  private def isAllow(kind: String): Boolean =
    val norm = normalize(kind)
    norm == "allow_once" || norm == "allow_always"

  private def isReject(kind: String): Boolean =
    val norm = normalize(kind)
    norm == "reject_once" || norm == "reject_always"

  private def normalize(kind: String): String =
    kind.trim.toLowerCase.replace('-', '_')

  private enum Decision:
    case Allow
    case Deny(reason: String)

  private enum ShellBase:
    case Ok(path: Path)
    case Off(raw: String)

  private enum McpCall:
    case Search
    case Named(server: String, tool: String)

  private final case class Choice(optionId: String, kind: String)
end PermissionChoice
