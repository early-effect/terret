package terret.find

import slayer.*
import zio.{Chunk, Ref, ZIO}
import zio.test.*

object EmbedderSpec extends ZIOSpecDefault:
  private val sample = Embedding(ModelId.bgeSmall, Chunk(1f, 0f))

  def spec = suite("embedder")(
    test("queries carry the retrieval prefix and passages do not") {
      val text = "build the project"
      assertTrue(
        BgeSmall.queryText(text) == "Represent this sentence for searching relevant passages: build the project",
        BgeSmall.passageText(text) == text,
        BgeSmall.Dims == 384,
        ModelId.bgeSmall.value == "bge-small-en-v1.5",
      )
    },
    test("a blank never reaches the service, and a real string is passed through unprefixed") {
      for
        seen <- Ref.make(Chunk.empty[String])
        _    <- stub[Embedder](_.embedQuery(any[String]))((text: String) =>
          seen.update(_ :+ s"q:$text") *> ZIO.succeed(sample)
        )
        _ <- stub[Embedder](_.embedPassage(any[String]))((text: String) =>
          seen.update(_ :+ s"p:$text") *> ZIO.succeed(sample)
        )
        blankQuery   <- Embedder.embedQuery("  ").either
        blankPassage <- Embedder.embedPassage("").either
        query        <- Embedder.embedQuery("build")
        passage      <- Embedder.embedPassage("  compile  ")
        texts        <- seen.get
      yield assertTrue(
        blankQuery == Left(FindError.EmptyQuery),
        blankPassage == Left(FindError.EmptyQuery),
        query == sample,
        passage == sample,
        texts == Chunk("q:build", "p:compile"),
      )
    }.provide(stubbed[Embedder]),
  )
end EmbedderSpec
