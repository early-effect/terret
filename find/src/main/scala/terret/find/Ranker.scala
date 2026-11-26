package terret.find

import zio.Chunk

/** Reciprocal rank fusion. Rank is 1-based. A name's score is the sum of `1 / (k + rank)` across lists.
  *
  * A repeated name in one list keeps its first rank. Ties break by name, so the order does not depend on map iteration.
  */
object Ranker:
  val DefaultK: Int = 60

  def fuse(lists: Chunk[Chunk[String]], k: Int = DefaultK): Either[FindError, Chunk[Hit]] =
    if k <= 0 then Left(FindError.NonPositive(k))
    else
      val totals = lists.foldLeft(Map.empty[String, Double]): (acc, list) =>
        ranks(list).foldLeft(acc): (scores, placed) =>
          val (name, rank) = placed
          scores.updated(name, scores.getOrElse(name, 0.0) + weight(k, rank))
      Right(
        Chunk
          .fromIterable(totals)
          .sortBy((name, score) => (-score, name))
          .map((name, score) => Hit(name, score))
      )

  /** First occurrence wins. The rank is that occurrence's 1-based place in the original list. */
  private def ranks(list: Chunk[String]): Chunk[(String, Int)] =
    list.zipWithIndex
      .foldLeft((Chunk.empty[(String, Int)], Set.empty[String])):
        case ((acc, seen), (name, index)) =>
          if seen.contains(name) then (acc, seen)
          else (acc :+ ((name, index + 1)), seen + name)
      ._1

  private def weight(k: Int, rank: Int): Double =
    1.0 / (k + rank)
end Ranker
