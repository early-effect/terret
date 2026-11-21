package terret.grok

import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, ZIO}

import java.nio.file.{Files, Path}
import java.util.{Comparator, UUID}

object McpDocumentSpec extends ZIOSpecDefault:
  private val histos = McpServer.Stdio(
    "histos",
    "histos",
    Chunk("mcp"),
    Chunk("HOME" -> "/work", "PATH" -> "/usr/bin"),
  )

  def spec = suite("MCP document")(
    test("stdio, http, and sse encode the ACP fields, and env pairs are sorted") {
      val http = McpServer.Http("metals", "http://127.0.0.1:1/mcp", Chunk.empty)
      val sse  = McpServer.Sse("legacy", "http://127.0.0.1:2/sse", Chunk("Authorization" -> "secret"))
      assertTrue(
        McpServer.encode(histos) == Json.Obj(
          "name"    -> Json.Str("histos"),
          "command" -> Json.Str("histos"),
          "args"    -> Json.Arr(Json.Str("mcp")),
          "env"     -> Json.Arr(
            Json.Obj("name" -> Json.Str("HOME"), "value" -> Json.Str("/work")),
            Json.Obj("name" -> Json.Str("PATH"), "value" -> Json.Str("/usr/bin")),
          ),
        ),
        McpServer.encode(http) == Json.Obj(
          "type"    -> Json.Str("http"),
          "name"    -> Json.Str("metals"),
          "url"     -> Json.Str("http://127.0.0.1:1/mcp"),
          "headers" -> Json.Arr(),
        ),
        McpServer.encode(sse) == Json.Obj(
          "type"    -> Json.Str("sse"),
          "name"    -> Json.Str("legacy"),
          "url"     -> Json.Str("http://127.0.0.1:2/sse"),
          "headers" -> Json.Arr(
            Json.Obj("name" -> Json.Str("Authorization"), "value" -> Json.Str("secret"))
          ),
        ),
      )
    },
    test("the file accepts an object or a name/value array, and rejects a mixed transport") {
      val stdio = McpServer.parse(
        """{"mcpServers":{"histos":{"command":"histos","args":["mcp"],"env":{"HOME":"/work","PATH":"/usr/bin"}}}}"""
      )
      val http = McpServer.parse(
        """{"mcpServers":{"metals":{"type":"http","url":"http://127.0.0.1:1/mcp","headers":[{"name":"Authorization","value":"secret"}]}}}"""
      )
      val both = McpServer.parse(
        """{"mcpServers":{"histos":{"command":"histos","url":"http://127.0.0.1:1/mcp"}}}"""
      )
      assertTrue(
        stdio == Right(Chunk(histos)),
        http == Right(
          Chunk(McpServer.Http("metals", "http://127.0.0.1:1/mcp", Chunk("Authorization" -> "secret")))
        ),
        both == Left(McpDocumentError.Transport("histos", "both")),
        McpServer.parse("{") == Left(McpDocumentError.Malformed),
        McpServer.parse("{}") == Left(McpDocumentError.ServersMissing),
        McpServer.parse("""{"mcpServers":{"histos":{"type":"http"}}}""") == Left(McpDocumentError.UrlMissing("histos")),
        McpServer.parse("""{"mcpServers":{"histos":{}}}""") == Left(McpDocumentError.CommandMissing("histos")),
        McpServer.parse("""{"mcpServers":{"histos":{"command":"histos","args":[1]}}}""") == Left(
          McpDocumentError.Args("histos")
        ),
      )
    },
    test("a missing file is an empty catalog and a broken file is malformed") {
      ZIO.scoped {
        for
          root    <- scratch
          absent  <- load(root.resolve("missing"))
          broken  <- write(root.resolve("broken"), "{").flatMap(load).either
          defined <- write(root.resolve("good"), good).flatMap(load)
        yield assertTrue(
          absent == Chunk.empty,
          broken == Left(McpDocumentError.Malformed),
          defined == Chunk(McpServer.Stdio("histos", "histos", Chunk("mcp"), Chunk.empty)),
        )
      }
    },
  )

  private val good =
    """{"mcpServers":{"histos":{"command":"histos","args":["mcp"]}}}"""

  private def scratch =
    val root = Path.of("target", "mcp-catalog").toAbsolutePath
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        val dir = root.resolve(UUID.randomUUID.toString)
        Files.createDirectories(dir)
        dir
      }.orDie
    )(delete)

  private def write(dir: Path, text: String) =
    ZIO.attemptBlocking {
      Files.createDirectories(dir)
      Files.writeString(dir.resolve(".mcp.json"), text)
      dir
    }.orDie

  private def load(dir: Path) =
    ZIO
      .fromEither(Workspace.from(dir.toString))
      .orDieWith(_ => new IllegalArgumentException("workspace"))
      .flatMap(McpCatalog.live.load)

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk = Files.walk(root)
        try
          val paths = walk.sorted(Comparator.reverseOrder).iterator()
          while paths.hasNext do Files.deleteIfExists(paths.next())
        finally walk.close()
    }.ignore
end McpDocumentSpec
