package terret.find

import zio.{Chunk, IO, ZIO, ZLayer}

/** Finds tools whose stored description matches a request. */
trait Find:
  def find(query: String, k: Int): IO[FindError, Chunk[ToolRef]]

object Find:
  /** Fusion reads at least this many hits from each ranker, even when `k` is smaller. */
  val Pool: Int = 32

  def find(query: String, k: Int): ZIO[Find, FindError, Chunk[ToolRef]] =
    ZIO.serviceWithZIO[Find](_.find(query, k))

  def layer: ZLayer[Embedder & Index, Nothing, Find] =
    ZLayer.fromFunction(Live.apply)

  private final case class Live(embedder: Embedder, index: Index) extends Find:
    def find(query: String, k: Int): IO[FindError, Chunk[ToolRef]] =
      ZIO
        .fromEither(Embedder.nonBlank(query))
        .flatMap: text =>
          if k <= 0 then ZIO.fail(FindError.NonPositive(k))
          else search(text, k)

    private def search(text: String, k: Int): IO[FindError, Chunk[ToolRef]] =
      val pool = math.max(k, Pool)
      for
        vector  <- embedder.embedQuery(text)
        lexical <- index.lexical(text, pool)
        nearest <- index.nearest(vector, pool)
        fused   <- ZIO.fromEither(Ranker.fuse(Chunk(lexical.map(_.name), nearest.map(_.name))))
        refs    <- ZIO.foreach(fused.take(k))(hit => load(hit.name))
      yield refs

    private def load(name: String): IO[FindError, ToolRef] =
      index
        .lookup(name)
        .flatMap:
          case Some(entry) => ZIO.fromEither(ToolRef.from(entry))
          case None        => ZIO.fail(FindError.Storage(s"indexed name $name is not stored"))
  end Live
end Find
