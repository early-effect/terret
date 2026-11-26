package terret.find

import zio.{Chunk, ZIO}

/** The tools this spike indexes. Each description is the phrase a search can match. */
object Seeds:
  val all: Chunk[ToolRef] = Chunk(
    tool("triage-issue", "IssueKey => UIO[Labels]", "label this github issue", Chunk(Capability.Network)),
    tool(
      "triage-pr",
      "PullRequest => UIO[Verdict]",
      "triage this pull request",
      Chunk(Capability.ReadWorkspace, Capability.Network),
    ),
    tool(
      "histos-build",
      "Workspace => IO[BuildFailure, Unit]",
      "compile the project",
      Chunk(Capability.RunCommands),
    ),
    tool(
      "histos-test",
      "Selector => IO[TestFailure, Report]",
      "run the tests",
      Chunk(Capability.RunCommands),
    ),
    tool(
      "release-notes",
      "Range => UIO[Changelog]",
      "draft the release notes",
      Chunk(Capability.ReadWorkspace, Capability.WriteWorkspace),
    ),
    tool(
      "dependency-bump",
      "Catalog => UIO[Listings]",
      "which dependencies are behind",
      Chunk(Capability.ReadWorkspace, Capability.Network),
    ),
    tool(
      "docs-pass",
      "Tree => UIO[Unit]",
      "update the readme",
      Chunk(Capability.ReadWorkspace, Capability.WriteWorkspace),
    ),
    tool(
      "permission-review",
      "Declared => UIO[Grants]",
      "what permissions does this tool have",
      Chunk(Capability.ReadWorkspace),
    ),
  )

  def index: ZIO[Embedder & Index, FindError, Unit] =
    ZIO.foreachDiscard(all): ref =>
      for
        embedding <- Embedder.embedPassage(ref.doc)
        _         <- ZIO.serviceWithZIO[Index](_.put(entry(ref, embedding)))
      yield ()

  private def tool(name: String, signature: String, doc: String, capabilities: Chunk[Capability]): ToolRef =
    ToolRef(name, signature, doc, capabilities)

  private def entry(ref: ToolRef, embedding: Embedding): Entry =
    Entry(ref.name, ref.signature, ref.doc, ref.capabilities.map(_.wire), embedding)
end Seeds
