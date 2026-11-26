package terret.find

import zio.{Chunk, IO, ZIO, ZLayer}

/** The tools `tools/list` returned for the servers in `.mcp.json`. */
trait Catalog:
  def list: IO[FindError, Chunk[ToolRef]]

object Catalog:
  def list: ZIO[Catalog, FindError, Chunk[ToolRef]] =
    ZIO.serviceWithZIO(_.list)

  def live(load: IO[FindError, Chunk[ToolRef]]): ZLayer[Any, Nothing, Catalog] =
    ZLayer.succeed(new Catalog:
      def list: IO[FindError, Chunk[ToolRef]] = load)
end Catalog
