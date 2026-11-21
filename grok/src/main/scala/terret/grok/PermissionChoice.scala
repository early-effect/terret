package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.Chunk

import java.nio.file.{InvalidPathException, Path}

enum PermissionAnswer:
  case Selected(optionId: String)
  case Refused(optionId: String, callId: String, reason: String)
  case Cancelled

object PermissionChoice:
  def decide(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      writeRoots: Chunk[Workspace],
      params: Json,
      access: McpAccess = McpAccess.closed,
      hosts: NetworkPolicy = NetworkPolicy.empty,
  ): PermissionAnswer =
    val tool = JsonFields
      .obj(params)
      .flatMap(JsonFields.field(_, "toolCall"))
      .flatMap(JsonFields.obj)
      .getOrElse(Json.Obj())
    val kind  = JsonFields.str(tool, "kind").getOrElse("")
    val allow = allows(policy, ceiling, workspace, writeRoots, tool, kind, access, hosts)
    pick(options(params), allow, callId(tool))

  def body(answer: PermissionAnswer): Json =
    answer match
      case PermissionAnswer.Selected(optionId)           => selected(optionId)
      case PermissionAnswer.Refused(optionId, _, reason) =>
        Json.Obj(
          "outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str(optionId)),
          "_meta"   -> Json.Obj("followup_message" -> Json.Str(reason)),
        )
      case PermissionAnswer.Cancelled =>
        Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled")))

  private def selected(optionId: String): Json =
    Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str(optionId)))

  private def allows(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      writeRoots: Chunk[Workspace],
      tool: Json.Obj,
      kind: String,
      access: McpAccess,
      hosts: NetworkPolicy,
  ): Decision =
    if networkKind(kind) || webTool(tool) then networkDecision(policy, ceiling, tool, kind, hosts)
    else
      val call = mcpCall(tool)
      policy match
        case PermissionPolicy.Inherit =>
          call match
            case Some(found) => mcpDecision(access, found)
            case None        => Decision.Allow
        case _ =>
          call match
            case Some(found) => mcpDecision(access, found)
            case None        =>
              normalize(kind) match
                case "read" | "search" | "grep" | "glob" =>
                  grant(policy, ceiling, Permission.ReadWorkspace, label(tool, kind))
                case "edit" | "write" | "delete" | "move" | "rename" =>
                  guarded(
                    policy,
                    ceiling,
                    Permission.WriteWorkspace,
                    label(tool, kind),
                    locations(tool),
                    cwd(tool, workspace),
                    roots(workspace, writeRoots),
                  )
                case "execute" | "bash" | "terminal" =>
                  shell(policy, ceiling, tool, workspace, writeRoots)
                case "think" | "thought" => Decision.Allow
                case _                   => Decision.Deny(s"${label(tool, kind)} needs a granted permission")

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
  ): Decision =
    val name = label(tool, "execute")
    if !ceiling.allow.contains(Permission.RunCommands) then Decision.Deny(missing(policy, Permission.RunCommands, name))
    else
      val effect  = command(tool).fold(ShellEffect.Opaque)(ShellWrite.effect)
      val located = locations(tool)
      val writing = effect match
        case ShellEffect.ReadOnly  => located.nonEmpty
        case ShellEffect.Writes(_) => true
        case ShellEffect.Opaque    => true
      if !writing then Decision.Allow
      else if !ceiling.allow.contains(Permission.WriteWorkspace) then
        Decision.Deny(missing(policy, Permission.WriteWorkspace, name))
      else
        val targets = effect match
          case ShellEffect.Writes(paths) => paths ++ located
          case _                         => located
        val base  = cwd(tool, workspace)
        val roots = this.roots(workspace, writeRoots)
        targets.find(path => !inside(path, base, roots)) match
          case Some(path) => Decision.Deny(s"$name writes $path, outside the workspace")
          case None       => Decision.Allow

  private def guarded(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      permission: Permission,
      name: String,
      targets: Chunk[String],
      base: Path,
      roots: Chunk[Path],
  ): Decision =
    if !ceiling.allow.contains(permission) then Decision.Deny(missing(policy, permission, name))
    else
      targets.find(path => !inside(path, base, roots)) match
        case Some(path) => Decision.Deny(s"$name writes $path, outside the workspace")
        case None       => Decision.Allow

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

  private def cwd(tool: Json.Obj, workspace: Workspace): Path =
    val raw = JsonFields
      .field(tool, "rawInput")
      .flatMap(JsonFields.obj)
      .flatMap(obj => JsonFields.str(obj, "cwd").filter(_.nonEmpty))
    raw.flatMap(absolute).getOrElse(workspace.path)

  private def absolute(raw: String): Option[Path] =
    try
      val path = Path.of(raw)
      if path.isAbsolute then Some(path.normalize) else None
    catch case _: InvalidPathException => None

  private def roots(workspace: Workspace, extra: Chunk[Workspace]): Chunk[Path] =
    (workspace +: extra).map(_.path.normalize)

  private def inside(target: String, base: Path, roots: Chunk[Path]): Boolean =
    resolve(target, base) match
      case None       => false
      case Some(path) => roots.exists(root => path.startsWith(root))

  private def resolve(target: String, base: Path): Option[Path] =
    if target.isEmpty then None
    else
      try
        val raw = Path.of(target)
        val abs = if raw.isAbsolute then raw else base.resolve(raw)
        Some(abs.normalize)
      catch case _: InvalidPathException => None

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

  private enum McpCall:
    case Search
    case Named(server: String, tool: String)

  private final case class Choice(optionId: String, kind: String)
end PermissionChoice
