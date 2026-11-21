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
  ): PermissionAnswer =
    val tool = JsonFields
      .obj(params)
      .flatMap(JsonFields.field(_, "toolCall"))
      .flatMap(JsonFields.obj)
      .getOrElse(Json.Obj())
    val kind  = JsonFields.str(tool, "kind").getOrElse("")
    val allow = allows(policy, ceiling, workspace, writeRoots, tool, kind)
    pick(options(params), allow, callId(tool))

  def body(answer: PermissionAnswer): Json =
    answer match
      case PermissionAnswer.Selected(optionId)      => selected(optionId)
      case PermissionAnswer.Refused(optionId, _, _) => selected(optionId)
      case PermissionAnswer.Cancelled               =>
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
  ): Decision =
    policy match
      case PermissionPolicy.Inherit => Decision.Allow
      case _                        =>
        normalize(kind) match
          case "read" | "search" | "grep" | "glob" =>
            grant(ceiling, Permission.ReadWorkspace, label(tool, kind))
          case "edit" | "write" | "delete" | "move" | "rename" =>
            guarded(
              ceiling,
              Permission.WriteWorkspace,
              label(tool, kind),
              locations(tool),
              cwd(tool, workspace),
              roots(workspace, writeRoots),
            )
          case "execute" | "bash" | "terminal" =>
            shell(ceiling, tool, workspace, writeRoots)
          case "think" | "thought" => Decision.Allow
          case _                   => Decision.Deny(s"${label(tool, kind)} needs a granted permission")

  private def shell(
      ceiling: Ceiling,
      tool: Json.Obj,
      workspace: Workspace,
      writeRoots: Chunk[Workspace],
  ): Decision =
    val name = label(tool, "execute")
    if !ceiling.allow.contains(Permission.RunCommands) then Decision.Deny(s"$name needs RunCommands")
    else
      val effect  = command(tool).fold(ShellEffect.Opaque)(ShellWrite.effect)
      val located = locations(tool)
      val writing = effect match
        case ShellEffect.ReadOnly  => located.nonEmpty
        case ShellEffect.Writes(_) => true
        case ShellEffect.Opaque    => true
      if !writing then Decision.Allow
      else if !ceiling.allow.contains(Permission.WriteWorkspace) then Decision.Deny(s"$name needs WriteWorkspace")
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
      ceiling: Ceiling,
      permission: Permission,
      name: String,
      targets: Chunk[String],
      base: Path,
      roots: Chunk[Path],
  ): Decision =
    if !ceiling.allow.contains(permission) then Decision.Deny(s"$name needs $permission")
    else
      targets.find(path => !inside(path, base, roots)) match
        case Some(path) => Decision.Deny(s"$name writes $path, outside the workspace")
        case None       => Decision.Allow

  private def grant(ceiling: Ceiling, permission: Permission, name: String): Decision =
    if ceiling.allow.contains(permission) then Decision.Allow else Decision.Deny(s"$name needs $permission")

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
    JsonFields.field(tool, "locations") match
      case Some(Json.Arr(values)) =>
        values.flatMap { item =>
          JsonFields.obj(item).flatMap(obj => JsonFields.str(obj, "path").filter(_.nonEmpty)) match
            case Some(path) => Chunk(path)
            case None       => Chunk.empty
        }
      case _ => Chunk.empty

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

  private def label(tool: Json.Obj, kind: String): String =
    JsonFields
      .str(tool, "name")
      .filter(_.nonEmpty)
      .orElse(JsonFields.str(tool, "toolName").filter(_.nonEmpty))
      .getOrElse {
        normalize(kind) match
          case "execute" | "bash" | "terminal" => "run_terminal_command"
          case other                           => other
      }

  private def callId(tool: Json.Obj): String =
    JsonFields.str(tool, "toolCallId").getOrElse("")

  private def pick(options: Chunk[Choice], decision: Decision, callId: String): PermissionAnswer =
    val wanted = decision match
      case Decision.Allow   => options.find(choice => isAllow(choice.kind))
      case Decision.Deny(_) => options.find(choice => isReject(choice.kind))
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

  private final case class Choice(optionId: String, kind: String)
end PermissionChoice
