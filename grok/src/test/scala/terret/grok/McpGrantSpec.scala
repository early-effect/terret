package terret.grok

import zio.Chunk
import zio.test.*

object McpGrantSpec extends ZIOSpecDefault:
  def spec = suite("MCP grants")(
    test("blank grants nothing, a server grants every tool, and a colon grants only the named tools") {
      val blank   = McpPolicy.parse("  ")
      val server  = McpPolicy.parse("histos")
      val tools   = McpPolicy.parse("histos:version+compile")
      val several = McpPolicy.parse("histos, metals:graph")
      assertTrue(
        blank == Right(McpPolicy.empty),
        server == Right(McpPolicy(Chunk(McpGrant("histos", None)))),
        server.exists(_.grant("histos").exists(_.allows("cache_clean"))),
        tools == Right(McpPolicy(Chunk(McpGrant("histos", Some(Set("version", "compile")))))),
        tools.exists(_.grant("histos").exists(grant => grant.allows("version") && !grant.allows("cache_clean"))),
        several == Right(
          McpPolicy(Chunk(McpGrant("histos", None), McpGrant("metals", Some(Set("graph")))))
        ),
      )
    },
    test("a colon with no tools, a bad token, and a repeated server are errors") {
      assertTrue(
        McpPolicy.parse("histos:") == Left(McpGrantError.EmptyTool("histos")),
        McpPolicy.parse(":version") == Left(McpGrantError.EmptyName),
        McpPolicy.parse("histos,") == Left(McpGrantError.BadToken("")),
        McpPolicy.parse("nope!") == Left(McpGrantError.BadToken("nope!")),
        McpPolicy.parse("histos,histos") == Left(McpGrantError.Duplicate("histos")),
        McpGrantError.EmptyTool("histos").render == "histos grants no tool. Omit the colon to grant the server",
        McpGrantError.EmptyName.render == "an MCP grant needs a server name",
        McpGrantError.BadToken("nope!").render == "'nope!' is not an MCP grant",
        McpGrantError.Duplicate("histos").render == "histos is granted twice",
      )
    },
    test("a grant keeps servers the file defines and drops the rest") {
      val policy  = McpPolicy(Chunk(McpGrant("histos", Some(Set("version"))), McpGrant("missing", None)))
      val defined = Chunk(
        McpServer.Stdio("histos", "histos", Chunk("mcp"), Chunk.empty),
        McpServer.Http("metals", "http://127.0.0.1:1/mcp", Chunk.empty),
      )
      assertTrue(policy.granted(defined).map(_.server) == Chunk("histos"))
    },
  )
