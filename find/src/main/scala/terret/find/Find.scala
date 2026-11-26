package terret.find

import zio.{Chunk, IO, ZIO, ZLayer}

/** Finds tools whose stored description matches a request, then keeps the ones the guard allows. */
trait Find:
  def find(query: String, k: Int, access: Access): IO[FindError, FindResult]

object Find:
  /** How a query is classified before any embedder or index runs. */
  enum Screened:
    case Done(result: FindResult)
    case Query(text: String)

  /** Blank and non-positive fail. An empty grant is `NoGrants` and does not search. */
  def screen(query: String, k: Int, access: Access): IO[FindError, Screened] =
    ZIO.fromEither(Embedder.nonBlank(query)).flatMap { text =>
      if k <= 0 then ZIO.fail(FindError.NonPositive(k))
      else if access.empty then ZIO.succeed(Screened.Done(FindResult.noGrants))
      else ZIO.succeed(Screened.Query(text))
    }

  def find(query: String, k: Int, access: Access): ZIO[Find, FindError, FindResult] =
    ZIO.serviceWithZIO[Find](_.find(query, k, access))

  def layer: ZLayer[Embedder & Index, Nothing, Find] =
    ZLayer.fromFunction(Live.apply)

  private final case class Live(embedder: Embedder, index: Index) extends Find:
    def find(query: String, k: Int, access: Access): IO[FindError, FindResult] =
      Find.screen(query, k, access).flatMap {
        case Screened.Done(result) => ZIO.succeed(result)
        case Screened.Query(text)  => search(text, k, access)
      }

    private def search(text: String, k: Int, access: Access): IO[FindError, FindResult] =
      for
        vector  <- embedder.embedQuery(text)
        lexical <- index.lexical(text, Whole)
        nearest <- index.nearest(vector, Whole)
        fused   <- ZIO.fromEither(Ranker.fuse(Chunk(lexical.map(_.name), nearest.map(_.name))))
        refs    <- ZIO.foreach(fused)(hit => load(hit.name))
      yield Selection(refs, k, access)

    private def load(key: String): IO[FindError, ToolRef] =
      index.lookup(key).flatMap {
        case Some(entry) => ZIO.succeed(entry.ref)
        case None        => ZIO.fail(FindError.Storage(s"indexed name $key is not stored"))
      }
  end Live

  /** Ask the index for every row. The guard runs before `k` truncates the hits. */
  private val Whole: Int = Int.MaxValue
end Find
