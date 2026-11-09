package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import zio.json.ast.Json
import zio.Chunk

enum PermissionAnswer:
  case Selected(optionId: String)
  case Cancelled
end PermissionAnswer

object PermissionChoice:
  def decide(policy: PermissionPolicy, ceiling: Ceiling, params: Json): PermissionAnswer =
    val kind = JsonFields
      .obj(params)
      .flatMap(JsonFields.field(_, "toolCall"))
      .flatMap(JsonFields.obj)
      .flatMap(JsonFields.str(_, "kind"))
      .getOrElse("")
    pick(options(params), allows(policy, ceiling, kind))

  def body(answer: PermissionAnswer): Json =
    answer match
      case PermissionAnswer.Selected(optionId) =>
        Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str(optionId)))
      case PermissionAnswer.Cancelled =>
        Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled")))

  private def allows(policy: PermissionPolicy, ceiling: Ceiling, kind: String): Boolean =
    policy match
      case PermissionPolicy.Inherit => true
      case _                         =>
        normalize(kind) match
          case "read" | "search" | "grep" | "glob"         => ceiling.allow.contains(Permission.ReadWorkspace)
          case "edit" | "write" | "delete" | "move" | "rename" =>
            ceiling.allow.contains(Permission.WriteWorkspace)
          case "execute" | "bash" | "terminal" => ceiling.allow.contains(Permission.RunCommands)
          case "think" | "thought"             => true
          case _                               => false

  private def pick(options: Chunk[Choice], allow: Boolean): PermissionAnswer =
    val wanted =
      if allow then options.find(choice => isAllow(choice.kind))
      else options.find(choice => isReject(choice.kind))
    wanted match
      case Some(choice) => PermissionAnswer.Selected(choice.optionId)
      case None         => PermissionAnswer.Cancelled

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

  private final case class Choice(optionId: String, kind: String)
end PermissionChoice
