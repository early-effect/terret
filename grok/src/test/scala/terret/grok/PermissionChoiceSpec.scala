package terret.grok

import terret.permission.{Ceiling, PermissionPolicy}
import zio.json.ast.Json
import zio.test.*

object PermissionChoiceSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  private def params(kind: String): Json =
    Json.Obj("toolCall" -> Json.Obj("kind" -> Json.Str(kind)), "options" -> options)

  def spec = suite("PermissionChoice")(
    test("inherit allows and a closed ceiling rejects an edit") {
      assertTrue(
        PermissionChoice.decide(PermissionPolicy.Inherit, Ceiling.all, params("edit")) == PermissionAnswer
          .Selected("allow-once"),
        PermissionChoice.decide(PermissionPolicy.Closed, Ceiling.closed, params("edit")) == PermissionAnswer
          .Selected("reject-once"),
      )
    },
    test("an unknown kind under a partial ceiling is denied") {
      val clamped = PermissionPolicy.Clamp(Ceiling.all)
      assertTrue(
        PermissionChoice.decide(clamped, Ceiling.all, params("other")) == PermissionAnswer.Selected("reject-once"),
        PermissionChoice.decide(PermissionPolicy.Closed, Ceiling.closed, params("think")) == PermissionAnswer
          .Selected("allow-once"),
      )
    },
    test("a missing matching option cancels") {
      val allowOnly = Json.Obj(
        "toolCall" -> Json.Obj("kind" -> Json.Str("edit")),
        "options"  -> Json.Arr(Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once"))),
      )
      val answer = PermissionChoice.decide(PermissionPolicy.Closed, Ceiling.closed, allowOnly)
      assertTrue(
        answer == PermissionAnswer.Cancelled,
        PermissionChoice.body(answer) == Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled"))),
      )
    },
  )
end PermissionChoiceSpec
