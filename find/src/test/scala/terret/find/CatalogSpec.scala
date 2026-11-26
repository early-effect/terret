package terret.find

import slayer.*
import zio.{Chunk, Scope, ZIO}
import zio.test.*

object CatalogSpec extends ZIOSpecDefault:
  private val listed = ToolRef(Some(ServerName("histos")), ToolId("test"), "run the tests")

  def spec = suite("catalog")(
    test("a listed tool is embedded and stored under server/tool") {
      for
        _ <- stub[Catalog](_.list)(ZIO.succeed(Chunk(listed)))
        _ <- stub[Embedder](_.embedPassage(any[String]))((_: String) =>
          ZIO.succeed(Embedding(ModelId.bgeSmall, Chunk(1f, 0f)))
        )
        _     <- Indexer.load
        found <- ZIO.serviceWithZIO[Index](_.lookup("histos/test"))
      yield assertTrue(
        found.map(_.ref) == Some(listed),
        found.map(_.description) == Some("run the tests"),
      )
    }.provideSome[Scope](stubbed[Catalog] ++ stubbed[Embedder] ++ SqliteIndex.temporary)
  )
end CatalogSpec
