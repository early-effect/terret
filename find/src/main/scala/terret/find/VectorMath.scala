package terret.find

import zio.Chunk

/** Cosine on stored vectors. Normalization happens once, at embed time. */
object VectorMath:
  /** Scale `values` to unit length. An empty or zero vector has no direction. */
  def normalize(values: Chunk[Float]): Either[FindError, Chunk[Float]] =
    val squared = sumSquares(values)
    if values.isEmpty || squared == 0.0 then Left(FindError.Degenerate)
    else
      val norm = math.sqrt(squared)
      Right(values.map(value => (value.toDouble / norm).toFloat))

  /** Cosine of two vectors that already share a width. Identical unit vectors score 1. */
  def cosine(left: Chunk[Float], right: Chunk[Float]): Either[FindError, Double] =
    if left.length != right.length then Left(FindError.DimMismatch(left.length, right.length))
    else
      val leftNorm  = math.sqrt(sumSquares(left))
      val rightNorm = math.sqrt(sumSquares(right))
      if left.isEmpty || leftNorm == 0.0 || rightNorm == 0.0 then Left(FindError.Degenerate)
      else Right(dot(left, right) / (leftNorm * rightNorm))

  /** Cosine only when both vectors name the same model and the same width. */
  def similarity(left: Embedding, right: Embedding): Either[FindError, Double] =
    if left.model != right.model then Left(FindError.ModelMismatch(left.model.value, right.model.value))
    else cosine(left.values, right.values)

  private def sumSquares(values: Chunk[Float]): Double =
    values.foldLeft(0.0)((acc, value) => acc + value.toDouble * value.toDouble)

  private def dot(left: Chunk[Float], right: Chunk[Float]): Double =
    left.zip(right).foldLeft(0.0) { case (acc, (lhs, rhs)) => acc + lhs.toDouble * rhs.toDouble }
end VectorMath
