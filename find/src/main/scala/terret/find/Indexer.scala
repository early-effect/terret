package terret.find

import zio.ZIO

/** Writes the catalog into the index. A blank description is stored as written and embedded as the tool name. */
object Indexer:
  def load: ZIO[Catalog & Embedder & Index, FindError, Unit] =
    for
      tools <- Catalog.list
      _     <- ZIO.foreachDiscard(tools)(store)
    yield ()

  private def store(ref: ToolRef): ZIO[Embedder & Index, FindError, Unit] =
    val passage = if ref.description.trim.isEmpty then ref.tool.value else ref.description
    for
      embedding <- Embedder.embedPassage(passage)
      _         <- ZIO.serviceWithZIO[Index](_.put(Entry(ref.server, ref.tool, ref.description, embedding)))
    yield ()
end Indexer
