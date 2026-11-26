package terret.model

import zio.{Chunk, IO, ZIO, ZLayer}

/** Model names a host may pass to `open`. */
trait ModelCatalog:
  def known: IO[CatalogError, Chunk[ModelKey]]

object ModelCatalog:
  /** No keys. A requested model is unknown. An omitted model does not call `known`. */
  val empty: ModelCatalog = new:
    def known: IO[CatalogError, Chunk[ModelKey]] = ZIO.succeed(Chunk.empty)

  val emptyLayer: ZLayer[Any, Nothing, ModelCatalog] = ZLayer.succeed(empty)
end ModelCatalog

enum CatalogError:
  case Unreadable(path: String)
  case Malformed(path: String, line: Int)
