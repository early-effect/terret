package terret.app

import heddle.mcp.protocol.{ClientRequest, Message, Methods, ProtocolVersion, RequestId}
import heddle.mcp.transport.Http
import terret.memory.{InMemory, Script}
import terret.model.ModelCatalog
import terret.permission.PermissionPolicy
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.json.*
import zio.test.*
import zio.durationInt
import zio.{ConfigProvider, Scope, ZIO, ZLayer}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

object ProcessSpec extends ZIOSpecDefault:
  def spec = suite("process")(
    test("an empty config is the local Warp default") {
      ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map.empty))
        .map { config =>
          assertTrue(
            config == ProcessConfig.local,
            config.listen match
              case http: Listen.Http =>
                WarpLocal.url(http.host, http.port) == "http://127.0.0.1:8080/mcp"
              case Listen.Stdio => false,
          )
        }
    },
    test("stdio, a clamp, and a socket bind decode") {
      val parsed = ProcessConfig.load(
        ProcessConfig.Raw(
          listen = "stdio",
          host = WarpLocal.Host,
          port = WarpLocal.Port,
          policy = "Clamp",
          allow = "ReadWorkspace, Network",
          root = Some("/work"),
          version = "dev",
          harness = "grok",
          command = Some("grok"),
          profile = None,
          transport = Some("AcpWebSocket"),
          approval = Some("Always"),
          bindHost = Some("127.0.0.1"),
          bindPort = Some(6767),
          bindSecret = Some("secret"),
        )
      )
      assertTrue(
        parsed.map(_.listen) == Right(Listen.Stdio),
        parsed.map(_.policy) == Right(
          PermissionPolicy.Clamp(
            terret.permission.Ceiling(
              Set(terret.permission.Permission.ReadWorkspace, terret.permission.Permission.Network)
            )
          )
        ),
        parsed.map(_.root) == Workspace.from("/work").map(Some(_)),
        parsed.map(_.harness) == terret.grok.AcpBind
          .make("127.0.0.1", 6767, "secret")
          .map { bind =>
            ProcessHarness.Grok(
              terret.grok.GrokConfig.socket(bind).withApproval(terret.grok.GrokApproval.Always)
            )
          }
          .left
          .map(ProcessError.Grok(_)),
      )
    },
    test("dsh is the other harness, and the other harness's keys are refused") {
      val selected = ProcessConfig.load(
        defaults.copy(harness = "dsh", command = None, profile = Some("sdk"), transport = None, approval = None)
      )
      val blank = ProcessConfig.load(
        defaults.copy(harness = "dsh", command = None, profile = Some(""), transport = None, approval = None)
      )
      val profile = ProcessConfig.load(defaults.copy(profile = Some("headless")))
      val keys    = ProcessConfig.load(defaults.copy(harness = "dsh"))
      val unknown = ProcessConfig.load(defaults.copy(harness = "codex"))
      val made    = terret.dsh.DshConfig.make("dsh", "sdk").map(ProcessHarness.Dsh(_)).left.map(ProcessError.Dsh(_))
      assertTrue(
        selected.map(_.harness) == made,
        blank == Left(ProcessError.Dsh(terret.dsh.DshConfigError.EmptyProfile)),
        profile == Left(ProcessError.ProfileUnexpected),
        keys == Left(ProcessError.GrokKeys),
        unknown == Left(ProcessError.Harness("codex")),
      )
    },
    test("ask under a ceiling uses stdio, and an explicit cli ask is refused") {
      val omitted   = ProcessConfig.load(defaults.copy(transport = None))
      val explicit  = ProcessConfig.load(defaults)
      val inherited = ProcessConfig.load(defaults.copy(policy = "Inherit", transport = None))
      val roots     = ProcessConfig.load(defaults.copy(transport = None, writeRoots = Some("work")))
      val scratch   = ProcessConfig.load(defaults.copy(transport = None, writeRoots = Some("/tmp")))
      assertTrue(
        omitted.map(_.harness) == Right(ProcessHarness.Grok(terret.grok.GrokConfig.stdio)),
        explicit == Left(ProcessError.CliAsk),
        inherited.map(_.harness) == Right(ProcessHarness.Grok(terret.grok.GrokConfig.cli)),
        roots == Left(ProcessError.Workspace(terret.workspace.WorkspaceError.Relative("work"))),
        scratch.map(_.writeRoots) == terret.workspace.Workspace
          .from("/tmp")
          .map(root => zio.Chunk(root))
          .left
          .map(ProcessError.Workspace(_)),
      )
    },
    test("an empty version and a relative root are refused") {
      val empty    = ProcessConfig.load(defaults.copy(version = ""))
      val relative = ProcessConfig.load(defaults.copy(root = Some("work")))
      assertTrue(
        empty == Left(ProcessError.Version),
        relative == Left(ProcessError.Workspace(terret.workspace.WorkspaceError.Relative("work"))),
      )
    },
    test("terret.grok.config selects the model file") {
      val stdio      = defaults.copy(transport = None)
      val selected   = ProcessConfig.load(stdio.copy(grokConfig = Some("/var/terret/grok.toml")))
      val empty      = ProcessConfig.load(stdio.copy(grokConfig = Some("")))
      val overridden = ZIO
        .config(ProcessConfig.raw)
        .mapError(ProcessError.Config(_))
        .flatMap(raw => ZIO.fromEither(ProcessConfig.load(raw)))
        .withConfigProvider(ConfigProvider.fromMap(Map("terret.grok.config" -> "/var/terret/grok.toml")))
      for chosen <- overridden
      yield assertTrue(
        selected.map(_.grokConfig) == Right(java.nio.file.Path.of("/var/terret/grok.toml")),
        empty == Left(ProcessError.GrokConfigPath),
        chosen.grokConfig == java.nio.file.Path.of("/var/terret/grok.toml"),
      )
    },
    test("the Warp file names the local HTTP endpoint") {
      val doc = WarpLocal.http(WarpLocal.url("0.0.0.0", 8080))
      assertTrue(
        WarpLocal.url("0.0.0.0", 8080) == "http://127.0.0.1:8080/mcp",
        WarpLocal.url("::1", 9) == "http://[::1]:9/mcp",
        endpoint(doc) == Some(Json.Str("http://127.0.0.1:8080/mcp")),
      )
    },
    test("HTTP serves the jobs on the bound port") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          config = ProcessConfig.local.copy(listen = Listen.Http("127.0.0.1", 0))
          server <- Process.bind(config, memory).provideSome[Scope](ZLayer.succeed(ModelCatalog.empty))
          port   <- server.port
          status <- listed(port)
        yield assertTrue(status == Right(Set("open", "run", "status", "resume", "export", "cancel")))
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private def endpoint(doc: Json): Option[Json] =
    doc match
      case root: Json.Obj =>
        root.get("mcpServers") match
          case Some(servers: Json.Obj) =>
            servers.get("terret") match
              case Some(entry: Json.Obj) => entry.get("url")
              case _                     => None
          case _ => None
      case _ => None

  private val defaults: ProcessConfig.Raw = ProcessConfig.Raw(
    listen = "http",
    host = WarpLocal.Host,
    port = WarpLocal.Port,
    policy = "Closed",
    allow = "",
    root = None,
    version = "dev",
    harness = "grok",
    command = Some("grok"),
    profile = None,
    transport = Some("Cli"),
    approval = Some("Ask"),
    bindHost = None,
    bindPort = None,
    bindSecret = None,
  )

  private def listed(port: Int): ZIO[Any, Throwable, Either[String, Set[String]]] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attempt(HttpClient.newHttpClient()))(client => ZIO.attempt(client.close()).ignore)
        .flatMap: client =>
          ZIO.attemptBlocking:
            val tool    = Message.stateless(RequestId.Num(1), ClientRequest.ListTools(None))
            val request = HttpRequest
              .newBuilder(URI.create(s"http://127.0.0.1:$port/mcp"))
              .header("Content-Type", "application/json")
              .header("Accept", "application/json")
              .header(Http.ProtocolHeader, ProtocolVersion.Current.value)
              .header(Http.MethodHeader, Methods.ListTools)
              .POST(HttpRequest.BodyPublishers.ofString(tool.json.toJson))
              .build()
            val response = client.send(request, HttpResponse.BodyHandlers.ofString())
            val body     = Option(response.body()).getOrElse("")
            if response.statusCode() != 200 then Left(body)
            else names(body)

  private def names(body: String): Either[String, Set[String]] =
    for
      json    <- body.fromJson[Json]
      message <- Message.decode(json).left.map(_.json.toJson)
      result  <- message match
        case Message.Result(_, result) => Right(result)
        case other                     => Left(other.json.toJson)
      tools <- result.get("tools") match
        case Some(Json.Arr(values)) => Right(values)
        case _                      => Left("missing tools")
    yield tools.flatMap {
      case obj: Json.Obj =>
        obj.get("name") match
          case Some(Json.Str(name)) => Set(name)
          case _                    => Set.empty
      case _ => Set.empty
    }.toSet
end ProcessSpec
