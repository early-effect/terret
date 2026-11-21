package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, ZIO}

object PermissionChoiceSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  private val commands = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
  private val both     = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))

  def spec = suite("PermissionChoice")(
    test("inherit allows and a closed ceiling rejects an edit") {
      checked(PermissionPolicy.Inherit, Ceiling.all, params("edit")).flatMap { allowed =>
        checked(PermissionPolicy.Closed, Ceiling.closed, params("edit")).map { denied =>
          assertTrue(
            allowed == PermissionAnswer.Selected("allow-once"),
            denied == PermissionAnswer.Refused("reject-once", "", "edit needs WriteWorkspace"),
          )
        }
      }
    },
    test("an unknown kind under a partial ceiling is denied") {
      val clamped = PermissionPolicy.Clamp(Ceiling.all)
      checked(clamped, Ceiling.all, params("other")).flatMap { unknown =>
        checked(PermissionPolicy.Closed, Ceiling.closed, params("think")).map { thought =>
          assertTrue(
            unknown == PermissionAnswer.Refused("reject-once", "", "other needs a granted permission"),
            thought == PermissionAnswer.Selected("allow-once"),
          )
        }
      }
    },
    test("a shell write needs both grants and a path inside an allowed root") {
      val inside  = shell("echo hi > out.txt")
      val devNull = shell("echo hi > /dev/null")
      val outside = shell("echo hi > /tmp/terret-probe.log")
      val version = shell("./histos --version")
      for
        granted  <- checked(PermissionPolicy.Clamp(both), both, inside)
        readOnly <- checked(PermissionPolicy.Clamp(commands), commands, devNull)
        missing  <- checked(PermissionPolicy.Clamp(commands), commands, inside)
        escaped  <- checked(PermissionPolicy.Clamp(both), both, outside)
        scratch  <- ZIO.fromEither(Workspace.from("/tmp")).orDieWith(_ => new IllegalArgumentException("workspace"))
        rooted   <- checked(PermissionPolicy.Clamp(both), both, outside, Chunk(scratch))
        command  <- checked(PermissionPolicy.Clamp(commands), commands, version)
      yield assertTrue(
        granted == PermissionAnswer.Selected("allow-once"),
        readOnly == PermissionAnswer.Selected("allow-once"),
        missing == PermissionAnswer.Refused("reject-once", "call_1", "run_terminal_command needs WriteWorkspace"),
        escaped == PermissionAnswer
          .Refused("reject-once", "call_1", "run_terminal_command writes /tmp/terret-probe.log, outside the workspace"),
        rooted == PermissionAnswer.Selected("allow-once"),
        command == PermissionAnswer.Selected("allow-once"),
      )
    },
    test("tee and cp are writes, and an edit outside the workspace is refused") {
      val tee    = shell("echo hi | tee out.txt")
      val copied = shell("cp README.md target/x.md")
      for
        allowed <- checked(PermissionPolicy.Clamp(both), both, tee)
        denied  <- checked(PermissionPolicy.Clamp(commands), commands, copied)
        edit    <- checked(PermissionPolicy.Clamp(both), both, located("edit", "/etc/passwd"))
      yield assertTrue(
        allowed == PermissionAnswer.Selected("allow-once"),
        denied == PermissionAnswer.Refused("reject-once", "call_1", "run_terminal_command needs WriteWorkspace"),
        edit == PermissionAnswer.Refused("reject-once", "call_1", "edit writes /etc/passwd, outside the workspace"),
      )
    },
    test("a missing matching option cancels") {
      val allowOnly = Json.Obj(
        "toolCall" -> Json.Obj("kind" -> Json.Str("edit")),
        "options"  -> Json.Arr(Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once"))),
      )
      checked(PermissionPolicy.Closed, Ceiling.closed, allowOnly).map { answer =>
        assertTrue(
          answer == PermissionAnswer.Cancelled,
          PermissionChoice.body(answer) == Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled"))),
        )
      }
    },
  )

  private def params(kind: String): Json =
    Json.Obj("toolCall" -> Json.Obj("kind" -> Json.Str(kind)), "options" -> options)

  private def shell(command: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("execute"),
        "rawInput"   -> Json.Obj("command" -> Json.Str(command)),
      ),
      "options" -> options,
    )

  private def located(kind: String, path: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str(kind),
        "locations"  -> Json.Arr(Json.Obj("path" -> Json.Str(path))),
      ),
      "options" -> options,
    )

  private def checked(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      params: Json,
      roots: Chunk[Workspace] = Chunk.empty,
  ) =
    ZIO
      .fromEither(Workspace.from("/work"))
      .map { root =>
        PermissionChoice.decide(policy, ceiling, root, roots, params)
      }
      .orDieWith(_ => new IllegalArgumentException("workspace"))
end PermissionChoiceSpec
