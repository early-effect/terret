package terret.model

import zio.Chunk
import zio.test.*

object ModelKeySpec extends ZIOSpecDefault:
  private val known = Chunk(ModelKey("grok-4.7"), ModelKey("grok-4.5"), ModelKey("grok-4.7"))

  def spec = suite("Model key")(
    test("an omitted model stays the harness default") {
      assertTrue(ModelKey.choose(None, known) == Right(None))
    },
    test("a known name is chosen and an unknown name lists the keys") {
      assertTrue(
        ModelKey.choose(Some("grok-4.7"), known) == Right(Some(ModelKey("grok-4.7"))),
        ModelKey.choose(Some("missing"), known) == Left(UnknownModel("missing", Chunk("grok-4.5", "grok-4.7"))),
        ModelKey.choose(Some(""), known) == Left(UnknownModel("", Chunk("grok-4.5", "grok-4.7"))),
      )
    },
  )
end ModelKeySpec
