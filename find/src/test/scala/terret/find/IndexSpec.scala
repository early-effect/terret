package terret.find

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object IndexSpec extends ZIOSpecDefault:
  private val compile = Entry(
    Some(ServerName("histos")),
    ToolId("build"),
    "compile the scala project and run scala tests",
    Embedding(ModelId.bgeSmall, Chunk(1f, 0f, 0f)),
  )
  private val bread = Entry(
    Some(ServerName("kitchen")),
    ToolId("bake"),
    "recipe for sourdough bread",
    Embedding(ModelId.bgeSmall, Chunk(0f, 1f, 0f)),
  )
  private val other = Entry(
    None,
    ToolId("other-space"),
    "scala in another model",
    Embedding(ModelId("other"), Chunk(1f, 0f, 0f)),
  )

  def spec = suite("sqlite index")(
    test("fts terms are quoted so they cannot become operators") {
      assertTrue(
        FtsQuery.matchOf("Scala Project") == Some("\"project\" OR \"scala\""),
        FtsQuery.matchOf("or") == Some("\"or\""),
        FtsQuery.matchOf("...") == None,
        FtsQuery.quote("a\"b") == "\"a\"\"b\"",
        SqliteIndex.engine == LexicalEngine.Fts5,
      )
    },
    test("lexical rank, nearest rank, lookup, and replace") {
      for
        index   <- ZIO.service[Index]
        _       <- index.put(compile)
        _       <- index.put(bread)
        _       <- index.put(other)
        scala   <- index.lexical("scala tests", 5)
        loaf    <- index.lexical("sourdough", 5)
        blank   <- index.lexical("  ", 5)
        top     <- index.nearest(compile.embedding, 1)
        near    <- index.nearest(compile.embedding, 5)
        found   <- index.lookup(compile.key)
        missing <- index.lookup("absent")
        _       <- index.put(compile.copy(description = "release notes only"))
        renamed <- index.lexical("release notes", 5)
        stale   <- index.lexical("sourdough", 5)
      yield assertTrue(
        scala.map(_.name) == Chunk(compile.key, other.key),
        loaf.map(_.name) == Chunk(bread.key),
        blank.isEmpty,
        top == Chunk(Hit(compile.key, 1.0)),
        near == Chunk(Hit(compile.key, 1.0), Hit(bread.key, 0.0)),
        found == Some(compile),
        missing == None,
        renamed.map(_.name) == Chunk(compile.key),
        stale.map(_.name) == Chunk(bread.key),
      )
    }.provideSome[Scope](SqliteIndex.temporary),
    test("a non-positive limit is refused") {
      for
        index <- ZIO.service[Index]
        zero  <- index.lexical("scala", 0).either
        below <- index.nearest(compile.embedding, -1).either
      yield assertTrue(
        zero == Left(FindError.NonPositive(0)),
        below == Left(FindError.NonPositive(-1)),
      )
    }.provideSome[Scope](SqliteIndex.temporary),
  )
end IndexSpec
