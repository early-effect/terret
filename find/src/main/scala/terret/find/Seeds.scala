package terret.find

import zio.{Chunk, ZIO}

/** Search fixtures. Each one has no server, so `call` refuses it with `NotCallable`. */
object Seeds:
  val all: Chunk[ToolRef] = Chunk(
    ToolRef(None, ToolId("triage-issue"), "label this github issue"),
    ToolRef(None, ToolId("triage-pr"), "triage this pull request"),
    ToolRef(None, ToolId("histos-build"), "compile the project"),
    ToolRef(None, ToolId("histos-test"), "run the tests"),
    ToolRef(None, ToolId("release-notes"), "draft the release notes"),
    ToolRef(None, ToolId("dependency-bump"), "which dependencies are behind"),
    ToolRef(None, ToolId("docs-pass"), "update the readme"),
    ToolRef(None, ToolId("permission-review"), "what permissions does this tool have"),
  )

  def index: ZIO[Embedder & Index, FindError, Unit] =
    ZIO.foreachDiscard(all): ref =>
      for
        embedding <- Embedder.embedPassage(ref.description)
        _         <- ZIO.serviceWithZIO[Index](_.put(Entry(ref.server, ref.tool, ref.description, embedding)))
      yield ()
end Seeds
