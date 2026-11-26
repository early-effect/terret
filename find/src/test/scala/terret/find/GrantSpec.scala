package terret.find

import slayer.*
import zio.{Chunk, Scope, ZIO}
import zio.test.*

object GrantSpec extends ZIOSpecDefault:
  private val histos = ServerName("histos")
  private val github = ServerName("github")
  private val linear = ServerName("linear")

  /** `histos`, `github/search_code`, and `linear`. `.mcp.json` defines histos and github. */
  private val access = Access(
    Chunk(
      Grant(histos, None),
      Grant(github, Some(Set(ToolId("search_code")))),
      Grant(linear, None),
    ),
    Set(histos, github),
  )

  private val createPr   = tool(github, ToolId("create_pr"), "open a pull request", Chunk(1f, 0f, 0f, 0f))
  private val searchCode = tool(github, ToolId("search_code"), "search code in the repository", Chunk(0f, 1f, 0f, 0f))
  private val build      = tool(histos, ToolId("build"), "compile the scala project", Chunk(0f, 0f, 1f, 0f))
  private val testTool   = tool(histos, ToolId("test"), "run the test suite", Chunk(0f, 0f, 0f, 1f))

  private val indexed = (stubbed[Embedder] ++ SqliteIndex.temporary) >+> Find.layer

  def spec = suite("grants")(
    test("an ungranted server is NotGranted before the missing file is considered") {
      val ref = ToolRef(Some(ServerName("other")), ToolId("x"), "")
      assertTrue(Guard(ref, access) == Left(CallError.NotGranted(ServerName("other"), ToolId("x"))))
    },
    test("a seed, an unknown server, and an ungranted tool are refused before the runner") {
      for
        seed    <- Calls.invoke(None, "triage-issue", "{}", access).either
        missing <- Calls.invoke(Some("linear"), "list_issues", "{}", access).either
        denied  <- Calls.invoke(Some("github"), "create_pr", "{}", access).either
        blank   <- Calls.invoke(Some("  "), "create_pr", "{}", access).either
      yield assertTrue(
        seed == Left(CallError.NotCallable),
        blank == Left(CallError.NotCallable),
        missing == Left(CallError.UnknownServer(linear)),
        denied == Left(CallError.NotGranted(github, ToolId("create_pr"))),
      )
    }.provide(stubbed[Runner]),
    test("find open a pull request withholds create_pr and returns the next granted tool") {
      for
        _ <- stub[Embedder](_.embedQuery(any[String]))((text: String) => query(text))
        _ <- ZIO.foreachDiscard(Chunk(createPr, searchCode, build, testTool))(entry =>
          ZIO.serviceWithZIO[Index](_.put(entry))
        )
        found <- Find.find("open a pull request", 1, access)
      yield assertTrue(
        found.hint.isEmpty,
        found.hits == Chunk(searchCode.ref),
        found.withheld == Chunk(
          Withheld(github, ToolId("create_pr"), Enable.AddGrant("github/create_pr"))
        ),
      )
    }.provideSome[Scope](indexed),
    test("a granted server missing from the file is withheld with DefineServer") {
      val issue  = tool(linear, ToolId("list_issues"), "file a linear issue", Chunk(1f, 0f))
      val tests  = tool(histos, ToolId("test"), "run the tests", Chunk(0f, 1f))
      val narrow = (stubbed[Embedder] ++ SqliteIndex.temporary) >+> Find.layer
      (for
        _ <- stub[Embedder](_.embedQuery(any[String]))((text: String) =>
          if text == "file a linear issue" then ZIO.succeed(issue.embedding)
          else ZIO.fail(FindError.Embedding(EmbeddingProblem.Shape))
        )
        _     <- ZIO.serviceWithZIO[Index](_.put(issue))
        _     <- ZIO.serviceWithZIO[Index](_.put(tests))
        found <- Find.find("file a linear issue", 1, access)
      yield assertTrue(
        found.hits == Chunk(tests.ref),
        found.withheld == Chunk(Withheld(linear, ToolId("list_issues"), Enable.DefineServer(linear))),
      )).provideSome[Scope](narrow)
    },
    test("an empty grant returns no hits and NoGrants, and does not embed") {
      for found <- Find.find("open a pull request", 1, Access.empty)
      yield assertTrue(found == FindResult.noGrants)
    }.provide(stubbed[Embedder] ++ stubbed[Index] >+> Find.layer),
    test("withheld keeps the three strongest refusals") {
      val server = ServerName("histos")
      val names  = Chunk(ToolId("a"), ToolId("b"), ToolId("c"), ToolId("d"), ToolId("keep"))
      val ranked = names.map(id => ToolRef(Some(server), id, id.value))
      val kept   = Access(Chunk(Grant(server, Some(Set(ToolId("keep"))))), Set(server))
      val found  = Selection(ranked, 1, kept)
      assertTrue(
        found.hits.map(_.tool.value) == Chunk("keep"),
        found.withheld.map(_.tool.value) == Chunk("a", "b", "c"),
        found.withheld.map(_.enable) == Chunk(
          Enable.AddGrant("histos/a"),
          Enable.AddGrant("histos/b"),
          Enable.AddGrant("histos/c"),
        ),
      )
    },
  )

  private def tool(server: ServerName, id: ToolId, description: String, values: Chunk[Float]): Entry =
    Entry(Some(server), id, description, Embedding(ModelId.bgeSmall, values))

  private def query(text: String): ZIO[Any, FindError, Embedding] =
    if text == "open a pull request" then ZIO.succeed(createPr.embedding)
    else ZIO.fail(FindError.Embedding(EmbeddingProblem.Shape))
end GrantSpec
