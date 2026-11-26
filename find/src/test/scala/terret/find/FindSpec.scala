package terret.find

import slayer.*
import zio.{Chunk, Scope, ZIO}
import zio.test.*

object FindSpec extends ZIOSpecDefault:
  private val unmatched = "qqqq wwww"

  private val axes: Map[String, Int] =
    Seeds.all.zipWithIndex.foldLeft(Map.empty[String, Int]): (acc, placed) =>
      val (ref, index) = placed
      acc.updated(ref.doc, index)

  private val refused = (stubbed[Embedder] ++ stubbed[Index]) >+> Find.layer

  private val indexed = (stubbed[Embedder] ++ SqliteIndex.temporary) >+> Find.layer

  def spec = suite("find")(
    test("every capability survives its wire form and an unknown one is refused") {
      val sample = Entry("n", "s", "d", Chunk("nope"), Embedding(ModelId.bgeSmall, Chunk(1f)))
      assertTrue(
        Capability.values.forall(cap => Capability.from(cap.wire) == Right(cap)),
        Capability.from("nope") == Left(FindError.UnknownCapability("nope")),
        ToolRef.from(sample) == Left(FindError.UnknownCapability("nope")),
      )
    },
    test("a blank query and a non-positive limit never embed") {
      for
        blank <- Find.find("   ", 3).either
        limit <- Find.find("compile the project", 0).either
      yield assertTrue(
        blank == Left(FindError.EmptyQuery),
        limit == Left(FindError.NonPositive(0)),
      )
    }.provide(refused),
    test("each seed query ranks its tool first") {
      for
        _      <- stub[Embedder](_.embedQuery(any[String]))((text: String) => embed(text))
        _      <- stub[Embedder](_.embedPassage(any[String]))((text: String) => embed(text))
        _      <- Seeds.index
        loaded <- ZIO.foreach(Seeds.all): ref =>
          Find.find(ref.doc, 1).map(_.headOption)
        none <- Find.find(unmatched, 1)
      yield assertTrue(
        loaded == Seeds.all.map(Some(_)),
        loaded.exists(_.exists(ref => ref.name == "histos-build" && ref.capabilities.contains(Capability.RunCommands))),
        none.isEmpty,
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
