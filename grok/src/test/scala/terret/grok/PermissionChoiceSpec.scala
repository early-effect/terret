package terret.grok

import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, ZIO}

object PermissionChoiceSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  private val commands = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
  private val both     = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))

  def spec = suite("PermissionChoice")(
    test("inherit allows and a closed ceiling rejects an edit") {
      checked(PermissionPolicy.Inherit, Ceiling.all, params("edit")).flatMap { allowed =>
        checked(PermissionPolicy.Closed, Ceiling.closed, params("edit")).map { denied =>
          assertTrue(
            allowed == PermissionAnswer.Selected("allow-once"),
            denied == PermissionAnswer.Refused(
              "reject-once",
              "",
              "edit needs WriteWorkspace, clamped by server policy",
            ),
          )
        }
      }
    },
    test("an unknown kind under a partial ceiling is denied") {
      val clamped = PermissionPolicy.Clamp(Ceiling.all)
      checked(clamped, Ceiling.all, params("other")).flatMap { unknown =>
        checked(PermissionPolicy.Closed, Ceiling.closed, params("think")).map { thought =>
          assertTrue(
            unknown == PermissionAnswer.Refused("reject-once", "", "other needs a granted permission"),
            thought == PermissionAnswer.Selected("allow-once"),
          )
        }
      }
    },
    test("a shell write needs both grants and a path inside an allowed root") {
      val inside  = shell("echo hi > out.txt")
      val devNull = shell("echo hi > /dev/null")
      val outside = shell("echo hi > /tmp/terret-probe.log")
      val version = shell("./histos --version")
      for
        granted  <- checked(PermissionPolicy.Clamp(both), both, inside)
        readOnly <- checked(PermissionPolicy.Clamp(commands), commands, devNull)
        missing  <- checked(PermissionPolicy.Clamp(commands), commands, inside)
        escaped  <- checked(PermissionPolicy.Clamp(both), both, outside)
        scratch  <- ZIO.fromEither(Workspace.from("/tmp")).orDieWith(_ => new IllegalArgumentException("workspace"))
        rooted   <- checked(PermissionPolicy.Clamp(both), both, outside, Chunk(scratch))
        command  <- checked(PermissionPolicy.Clamp(commands), commands, version)
      yield assertTrue(
        granted == PermissionAnswer.Selected("allow-once"),
        readOnly == PermissionAnswer.Selected("allow-once"),
        missing == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "run_terminal_command needs WriteWorkspace, clamped by server policy",
        ),
        escaped == PermissionAnswer
          .Refused("reject-once", "call_1", "run_terminal_command writes /tmp/terret-probe.log, outside the workspace"),
        rooted == PermissionAnswer.Selected("allow-once"),
        command == PermissionAnswer.Selected("allow-once"),
      )
    },
    test("tee and cp are writes, and an edit outside the workspace is refused") {
      val tee    = shell("echo hi | tee out.txt")
      val copied = shell("cp README.md target/x.md")
      for
        allowed <- checked(PermissionPolicy.Clamp(both), both, tee)
        denied  <- checked(PermissionPolicy.Clamp(commands), commands, copied)
        edit    <- checked(PermissionPolicy.Clamp(both), both, located("edit", "/etc/passwd"))
      yield assertTrue(
        allowed == PermissionAnswer.Selected("allow-once"),
        denied == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "run_terminal_command needs WriteWorkspace, clamped by server policy",
        ),
        edit == PermissionAnswer.Refused("reject-once", "call_1", "edit writes /etc/passwd, outside the workspace"),
      )
    },
    test("a recorded shell permission uses the live option ids") {
      for
        params  <- LiveBodies.json("request-permission.json")
        allowed <- checked(PermissionPolicy.Clamp(both), both, params)
        denied  <- checked(PermissionPolicy.Clamp(commands), commands, params)
      yield assertTrue(
        allowed == PermissionAnswer.Selected("allow-once"),
        denied == PermissionAnswer.Refused(
          "reject-once",
          "call-57c13523-c289-4650-b466-44fc96e26cf1-0",
          "bash needs WriteWorkspace, clamped by server policy",
        ),
      )
    },
    test("an edit checks rawInput paths and ignores file content") {
      val outside = writeFile("/etc/passwd", "class Probe {}")
      val inside  = writeFile("/work/Probe.java", "/etc/passwd")
      val listed  = writePaths("/etc/passwd")
      val nested  = writeNested("/etc/passwd")
      for
        escaped <- checked(PermissionPolicy.Clamp(both), both, outside)
        kept    <- checked(PermissionPolicy.Clamp(both), both, inside)
        paths   <- checked(PermissionPolicy.Clamp(both), both, listed)
        wrapped <- checked(PermissionPolicy.Clamp(both), both, nested)
      yield assertTrue(
        escaped == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "write writes /etc/passwd, outside the workspace",
        ),
        kept == PermissionAnswer.Selected("allow-once"),
        paths == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "edit writes /etc/passwd, outside the workspace",
        ),
        wrapped == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "edit writes /etc/passwd, outside the workspace",
        ),
      )
    },
    test("use_tool names a network tool as Network") {
      val open = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      for
        fetch   <- checked(PermissionPolicy.Clamp(open), open, hosted("web_fetch"))
        search  <- checked(PermissionPolicy.Clamp(open), open, hosted("web_search"))
        image   <- checked(PermissionPolicy.Clamp(open), open, hosted("image_gen"))
        plain   <- checked(PermissionPolicy.Clamp(open), open, hosted("list_dir"))
        granted <- checked(PermissionPolicy.Clamp(Ceiling.all), Ceiling.all, hosted("web_fetch"))
      yield assertTrue(
        fetch == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "web_fetch needs Network, clamped by server policy",
        ),
        search == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "web_search needs Network, clamped by server policy",
        ),
        image == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "image_gen needs Network, clamped by server policy",
        ),
        plain == PermissionAnswer.Refused("reject-once", "call_1", "use_tool needs a granted permission"),
        granted == PermissionAnswer.Selected("allow-once"),
      )
    },
    test("a refusal selects reject and carries the reason back") {
      val reason = "web_fetch needs Network, clamped by server policy"
      val answer = PermissionAnswer.Refused("reject-once", "call_1", reason)
      assertTrue(
        PermissionChoice.body(answer) == Json.Obj(
          "outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str("reject-once")),
          "_meta"   -> Json.Obj("followup_message" -> Json.Str(reason)),
        )
      )
    },
    test("a fetch without Network names the grant, and a server clamp says so") {
      val open   = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      val domain = Json.Arr(
        Json.Obj("optionId" -> Json.Str("allow-always-domain"), "kind"  -> Json.Str("allow_always_domain")),
        Json.Obj("optionId" -> Json.Str("allow-once"), "kind"           -> Json.Str("allow_once")),
        Json.Obj("optionId" -> Json.Str("reject-once"), "kind"          -> Json.Str("reject_once")),
        Json.Obj("optionId" -> Json.Str("reject-always-domain"), "kind" -> Json.Str("reject_always_domain")),
      )
      for
        granted <- checked(PermissionPolicy.Clamp(Ceiling.all), Ceiling.all, fetch(domain))
        clamped <- checked(PermissionPolicy.Clamp(open), open, fetch(domain))
        session <- checked(PermissionPolicy.Clamp(Ceiling.all), open, fetch(domain))
        bare    <- checked(PermissionPolicy.Clamp(open), open, params("fetch"))
      yield assertTrue(
        granted == PermissionAnswer.Selected("allow-once"),
        clamped == PermissionAnswer.Refused(
          "reject-once",
          "",
          "web_fetch needs Network, clamped by server policy",
        ),
        session == PermissionAnswer.Refused("reject-once", "", "web_fetch needs Network"),
        bare == PermissionAnswer.Refused(
          "reject-once",
          "",
          "web_fetch needs Network, clamped by server policy",
        ),
      )
    },
    test("kind search stays a workspace read, and a web search needs Network") {
      val open    = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      val reading = Ceiling(Set(Permission.ReadWorkspace))
      for
        local <- checked(PermissionPolicy.Clamp(reading), reading, params("search"))
        named <- checked(PermissionPolicy.Clamp(open), open, searchTool)
        read  <- checked(
          PermissionPolicy.Clamp(Ceiling(Set(Permission.RunCommands))),
          Ceiling(Set.empty[Permission]),
          params("search"),
        )
      yield assertTrue(
        local == PermissionAnswer.Selected("allow-once"),
        named == PermissionAnswer.Refused(
          "reject-once",
          "",
          "web_search needs Network, clamped by server policy",
        ),
        read == PermissionAnswer.Refused(
          "reject-once",
          "",
          "search needs ReadWorkspace, clamped by server policy",
        ),
      )
    },
    test("an MCP call needs a grant, and a network use_tool stays Network") {
      val version = McpAccess(McpPolicy(Chunk(McpGrant("histos", Some(Set("version"))))), Set("histos"))
      val every   = McpAccess(McpPolicy(Chunk(McpGrant("histos", None))), Set("histos"))
      val absent  = McpAccess(McpPolicy(Chunk(McpGrant("histos", None))), Set.empty)
      for
        search   <- checked(PermissionPolicy.Inherit, Ceiling.all, mcpSearch)
        allowed  <- checked(PermissionPolicy.Closed, Ceiling.closed, hosted("histos__version"), access = version)
        tool     <- checked(PermissionPolicy.Closed, Ceiling.closed, hosted("histos__compile"), access = version)
        server   <- checked(PermissionPolicy.Clamp(Ceiling.all), Ceiling.all, hosted("metals__graph"), access = version)
        prefixed <- checked(PermissionPolicy.Inherit, Ceiling.all, hosted("mcp__histos__cache_clean"), access = every)
        missing  <- checked(PermissionPolicy.Closed, Ceiling.closed, hosted("histos__version"), access = absent)
        unqualified <- checked(PermissionPolicy.Clamp(Ceiling.all), Ceiling.all, hosted("list_dir"), access = every)
        network     <- checked(PermissionPolicy.Closed, Ceiling.closed, hosted("web_fetch"), access = every)
      yield assertTrue(
        search == PermissionAnswer.Refused("reject-once", "call_1", "search_tool needs an MCP grant in terret.mcp"),
        allowed == PermissionAnswer.Selected("allow-once"),
        tool == PermissionAnswer.Refused("reject-once", "call_1", "compile needs an MCP grant on histos"),
        server == PermissionAnswer.Refused("reject-once", "call_1", "metals needs an MCP grant in terret.mcp"),
        prefixed == PermissionAnswer.Selected("allow-once"),
        missing == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "histos is granted but .mcp.json does not define it",
        ),
        unqualified == PermissionAnswer.Refused("reject-once", "call_1", "use_tool needs a granted permission"),
        network == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "web_fetch needs Network, clamped by server policy",
        ),
      )
    },
    test("a missing matching option cancels") {
      val allowOnly = Json.Obj(
        "toolCall" -> Json.Obj("kind" -> Json.Str("edit")),
        "options"  -> Json.Arr(Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once"))),
      )
      checked(PermissionPolicy.Closed, Ceiling.closed, allowOnly).map { answer =>
        assertTrue(
          answer == PermissionAnswer.Cancelled,
          PermissionChoice.body(answer) == Json.Obj("outcome" -> Json.Obj("outcome" -> Json.Str("cancelled"))),
        )
      }
    },
  )

  private def params(kind: String): Json =
    Json.Obj("toolCall" -> Json.Obj("kind" -> Json.Str(kind)), "options" -> options)

  private def fetch(choices: Json.Arr): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "kind"     -> Json.Str("fetch"),
        "rawInput" -> Json.Obj("variant" -> Json.Str("WebFetch"), "url" -> Json.Str("https://example.com")),
        "_meta"    -> Json.Obj("x.ai/tool" -> Json.Obj("name" -> Json.Str("web_fetch"))),
      ),
      "options" -> choices,
    )

  private val searchTool: Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "kind"     -> Json.Str("search"),
        "rawInput" -> Json.Obj("variant" -> Json.Str("WebSearch")),
      ),
      "options" -> options,
    )

  private def shell(command: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("execute"),
        "rawInput"   -> Json.Obj("command" -> Json.Str(command)),
      ),
      "options" -> options,
    )

  private def writeFile(path: String, content: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("edit"),
        "rawInput"   -> Json.Obj(
          "variant"   -> Json.Str("Write"),
          "file_path" -> Json.Str(path),
          "content"   -> Json.Str(content),
        ),
      ),
      "options" -> options,
    )

  private def writePaths(path: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("edit"),
        "rawInput"   -> Json.Obj("paths" -> Json.Arr(Json.Str(path))),
      ),
      "options" -> options,
    )

  private def writeNested(path: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("edit"),
        "rawInput"   -> Json.Obj("wrapper" -> Json.Obj("path" -> Json.Str(path))),
      ),
      "options" -> options,
    )

  private def hosted(tool: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("other"),
        "title"      -> Json.Str(tool),
        "rawInput"   -> Json.Obj(
          "variant"    -> Json.Str("UseTool"),
          "tool_name"  -> Json.Str(tool),
          "tool_input" -> Json.Obj(
            "url"   -> Json.Str("https://example.com"),
            "query" -> Json.Str("/etc/passwd"),
          ),
        ),
        "_meta" -> Json.Obj("x.ai/tool" -> Json.Obj("name" -> Json.Str("use_tool"))),
      ),
      "options" -> options,
    )

  private def located(kind: String, path: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str(kind),
        "locations"  -> Json.Arr(Json.Obj("path" -> Json.Str(path))),
      ),
      "options" -> options,
    )

  private val mcpSearch: Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("other"),
        "title"      -> Json.Str("search_tool"),
        "_meta"      -> Json.Obj("x.ai/tool" -> Json.Obj("name" -> Json.Str("search_tool"))),
      ),
      "options" -> options,
    )

  private def checked(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      params: Json,
      roots: Chunk[Workspace] = Chunk.empty,
      access: McpAccess = McpAccess.closed,
  ) =
    ZIO
      .fromEither(Workspace.from("/work"))
      .map { root =>
        PermissionChoice.decide(policy, ceiling, root, roots, params, access)
      }
      .orDieWith(_ => new IllegalArgumentException("workspace"))
end PermissionChoiceSpec
