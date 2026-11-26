package terret.find

import slayer.*
import zio.{Chunk, Scope, ZIO}
import zio.test.*

object FindSpec extends ZIOSpecDefault:
  private val unmatched = "qqqq wwww"

  private val axes: Map[String, Int] =
    Seeds.all.zipWithIndex.foldLeft(Map.empty[String, Int]): (acc, placed) =>
      val (ref, index) = placed
      acc.updated(ref.description, index)

  private val refused = (stubbed[Embedder] ++ stubbed[Index]) >+> Find.layer

  private val indexed = (stubbed[Embedder] ++ SqliteIndex.temporary) >+> Find.layer

  private val open = Access(Chunk(Grant(ServerName("histos"), None)), Set(ServerName("histos")))

  def spec = suite("find")(
    test("a blank query and a non-positive limit never embed") {
      for
        blank <- Find.find("   ", 3, open).either
        limit <- Find.find("compile the project", 0, open).either
      yield assertTrue(
        blank == Left(FindError.EmptyQuery),
        limit == Left(FindError.NonPositive(0)),
      )
    }.provide(refused),
    test("a seed is stored and find omits it because it has no server") {
      for
        _      <- stub[Embedder](_.embedQuery(any[String]))((text: String) => embed(text))
        _      <- stub[Embedder](_.embedPassage(any[String]))((text: String) => embed(text))
        _      <- Seeds.index
        loaded <- ZIO.foreach(Seeds.all): ref =>
          ZIO.serviceWithZIO[Index](_.lookup(ref.tool.value))
        found <- Find.find("compile the project", 1, open)
        none  <- Find.find(unmatched, 1, open)
      yield assertTrue(
        Seeds.all.forall(_.server.isEmpty),
        loaded.map(_.map(_.ref)) == Seeds.all.map(Some(_)),
        found == FindResult.empty,
        none == FindResult.empty,
      )
    }.provideSome[Scope](indexed),
  )

  private def embed(text: String): ZIO[Any, FindError, Embedding] =
    if text == unmatched then ZIO.succeed(Embedding(ModelId("other"), Chunk.fill(Seeds.all.length)(0f)))
    else
      axes.get(text) match
        case Some(index) =>
          ZIO.succeed(
            Embedding(
              ModelId.bgeSmall,
              Chunk.fromArray(Array.tabulate(Seeds.all.length)(i => if i == index then 1f else 0f)),
            )
          )
        case None => ZIO.fail(FindError.Embedding(EmbeddingProblem.Shape))
end FindSpec
