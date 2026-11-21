package terret.grok

import terret.workspace.Workspace
import zio.{Chunk, IO, ZIO}

import java.nio.file.Files

trait McpCatalog:
  def load(workspace: Workspace): IO[McpDocumentError, Chunk[McpServer]]

object McpCatalog:
  val empty: McpCatalog = new:
    def load(workspace: Workspace): IO[McpDocumentError, Chunk[McpServer]] =
      ZIO.succeed(Chunk.empty)

  val live: McpCatalog = new:
    def load(workspace: Workspace): IO[McpDocumentError, Chunk[McpServer]] =
      val path = workspace.path.resolve(".mcp.json")
      ZIO
        .attemptBlocking(if Files.isRegularFile(path) then Some(Files.readString(path)) else None)
        .mapError(_ => McpDocumentError.Unreadable)
        .flatMap:
          case None       => ZIO.succeed(Chunk.empty)
          case Some(text) => ZIO.fromEither(McpServer.parse(text))
end McpCatalog
