package terret.find

import zio.Chunk
import zio.test.*

object VectorMathSpec extends ZIOSpecDefault:
  private val unit = Embedding(ModelId.bgeSmall, Chunk(1f, 0f))

  private def close(actual: Double, expected: Double): Boolean =
    math.abs(actual - expected) < 1e-5

  def spec = suite("vector math")(
    test("a 3-4-5 vector normalizes to unit length") {
      val normalized = VectorMath.normalize(Chunk(3f, 4f)) match
        case Right(values) =>
          values.toList match
            case x :: y :: Nil =>
              close(x.toDouble, 0.6) && close(y.toDouble, 0.8) &&
              close(math.sqrt(x.toDouble * x.toDouble + y.toDouble * y.toDouble), 1.0)
            case _ => false
        case Left(_) => false
      assertTrue(normalized)
    },
    test("cosine is 1, 0, and -1 for the axis cases, and it is symmetric") {
      val east  = Chunk(1f, 0f)
      val north = Chunk(0f, 1f)
      val west  = Chunk(-1f, 0f)
      assertTrue(
        VectorMath.cosine(east, east).exists(close(_, 1)),
        VectorMath.cosine(east, north).exists(close(_, 0)),
        VectorMath.cosine(east, west).exists(close(_, -1)),
        VectorMath.cosine(east, north) == VectorMath.cosine(north, east),
      )
    },
    test("a zero vector, an empty vector, and a width mismatch are refused") {
      assertTrue(
        VectorMath.normalize(Chunk.empty) == Left(FindError.Degenerate),
        VectorMath.normalize(Chunk(0f, 0f)) == Left(FindError.Degenerate),
        VectorMath.cosine(Chunk.empty, Chunk.empty) == Left(FindError.Degenerate),
        VectorMath.cosine(Chunk(1f), Chunk(1f, 0f)) == Left(FindError.DimMismatch(1, 2)),
      )
    },
    test("similarity refuses a different model before it looks at the floats") {
      val other = Embedding(ModelId("other"), Chunk(1f, 0f))
      val wider = Embedding(ModelId.bgeSmall, Chunk(1f, 0f, 0f))
      assertTrue(
        VectorMath.similarity(unit, other) == Left(FindError.ModelMismatch("bge-small-en-v1.5", "other")),
        VectorMath.similarity(unit, wider) == Left(FindError.DimMismatch(2, 3)),
        VectorMath.similarity(unit, unit).exists(close(_, 1)),
      )
    },
  )
end VectorMathSpec
