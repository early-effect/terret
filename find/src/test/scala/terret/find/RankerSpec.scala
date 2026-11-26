package terret.find

import zio.Chunk
import zio.test.*

object RankerSpec extends ZIOSpecDefault:
  private def close(actual: Double, expected: Double): Boolean =
    math.abs(actual - expected) < 1e-9

  def spec = suite("reciprocal rank fusion")(
    test("a name in both lists outranks a name in one, and a tie breaks by name") {
      val fused = Ranker.fuse(Chunk(Chunk("build", "test"), Chunk("notes", "build")))
      val tied  = Ranker.fuse(Chunk(Chunk("b"), Chunk("a")))
      assertTrue(
        fused.map(_.map(_.name)) == Right(Chunk("build", "notes", "test")),
        fused.map(_.map(hit => close(hit.score, expected(hit.name)))) == Right(Chunk(true, true, true)),
        tied.map(_.map(_.name)) == Right(Chunk("a", "b")),
      )
    },
    test("a repeated name keeps its first rank and does not pull the next name forward") {
      val fused = Ranker.fuse(Chunk(Chunk("a", "a", "b")))
      assertTrue(
        fused.map(_.map(_.name)) == Right(Chunk("a", "b")),
        fused.map(_.lift(1).exists(hit => close(hit.score, 1.0 / 63))) == Right(true),
      )
    },
    test("a non-positive constant is refused and empty lists fuse to nothing") {
      assertTrue(
        Ranker.fuse(Chunk(Chunk("a")), 0) == Left(FindError.NonPositive(0)),
        Ranker.fuse(Chunk.empty) == Right(Chunk.empty),
        Ranker.fuse(Chunk(Chunk.empty, Chunk.empty)) == Right(Chunk.empty),
      )
    },
  )

  private def expected(name: String): Double =
    name match
      case "build" => 1.0 / 61 + 1.0 / 62
      case "notes" => 1.0 / 61
      case "test"  => 1.0 / 62
      case _       => -1
end RankerSpec
