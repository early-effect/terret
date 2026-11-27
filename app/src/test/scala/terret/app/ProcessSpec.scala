package terret.app

import heddle.mcp.protocol.{ClientRequest, Message, Methods, ProtocolVersion, RequestId}
import heddle.mcp.transport.Http
import terret.mcp.{OpenJob, Opened, RunJob, SessionJob, StatusView, WireState}
import terret.memory.{InMemory, Script}
import terret.model.ModelCatalog
import terret.permission.{Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.json.*
import zio.test.*
import zio.durationInt
import zio.{Chunk, ConfigProvider, Scope, ZIO, ZLayer}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

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
        ProcessConfig.load(defaults.copy(transport = None, readRoots = Some("work"))) ==
          Left(ProcessError.Workspace(terret.workspace.WorkspaceError.Relative("work"))),
        ProcessConfig.load(defaults.copy(transport = None, readRoots = Some("/tmp"))).map(_.readRoots) ==
          terret.workspace.Workspace.from("/tmp").map(root => zio.Chunk(root)).left.map(ProcessError.Workspace(_)),
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
    test("journal limit must be positive, and access is both fields or neither") {
      val open = ProcessConfig.load(defaults.copy(transport = None))
      val zero = ProcessConfig.load(defaults.copy(transport = None, journalLimit = 0))
      val half = ProcessConfig.load(
        defaults.copy(transport = None, accessTeam = Some("example.cloudflareaccess.com"))
      )
      val bad = ProcessConfig.load(
        defaults.copy(
          transport = None,
          accessTeam = Some("https://example.cloudflareaccess.com"),
          accessAudience = Some("aud"),
        )
      )
      val both = ProcessConfig.load(
        defaults.copy(
          transport = None,
          journalLimit = 128,
          accessTeam = Some("example.cloudflareaccess.com"),
          accessAudience = Some("aud"),
        )
      )
      val spaced = ProcessConfig.load(
        defaults.copy(
          transport = None,
          accessTeam = Some("example.cloudflareaccess.com"),
          accessAudience = Some("two words"),
        )
      )
      assertTrue(
        open.map(_.journal.limit) == Right(4096),
        open.map(_.access) == Right(None),
        zero == Left(ProcessError.Journal(terret.transcript.JournalBoundError.NonPositive(0))),
        half == Left(ProcessError.Access(AccessConfigError.Incomplete)),
        bad == Left(ProcessError.Access(AccessConfigError.Team)),
        spaced == Left(ProcessError.Access(AccessConfigError.Audience)),
        both.map(_.journal.limit) == Right(128),
        both.map(_.access.map(_.team.value)) == Right(Some("example.cloudflareaccess.com")),
        both.map(_.access.map(_.audience.value)) == Right(Some("aud")),
        both.map(_.access.map(_.port)) == Right(Some(AccessConfig.TunnelPort)),
        ProcessConfig
          .load(
            defaults.copy(
              transport = None,
              accessTeam = Some("example.cloudflareaccess.com"),
              accessAudience = Some("aud"),
              accessPort = Some(8080),
            )
          )
          .left
          .exists(_.render.contains("8080")),
        !ProcessError.Journal(terret.transcript.JournalBoundError.NonPositive(-3)).render.contains("secret"),
      )
    },
    test("subscriptions sit beside the pid file") {
      assertTrue(
        Subscriptions.file == Subscriptions.beside(PidFile.defaultPath),
        Subscriptions.beside(Path.of("target", "state", "server.pid")) ==
          Path.of("target", "state", "subscriptions.json"),
        Subscriptions.beside(Path.of("server.pid")) == Path.of(".terret", "subscriptions.json"),
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
          dir    <- ZIO.acquireRelease(
            ZIO.attempt {
              val root = Path.of("target")
              Files.createDirectories(root)
              Files.createTempDirectory(root, "process-subscriptions")
            }
          )(root => ZIO.attempt(deleteTree(root)).ignore)
          config = ProcessConfig.local.copy(
            listen = Listen.Http("127.0.0.1", 0),
            subscriptions = dir.resolve("subscriptions.json"),
          )
          server <- Process.bind(config, memory).provideSome[Scope](ZLayer.succeed(ModelCatalog.empty))
          port   <- server.port
          status <- listed(port)
        yield assertTrue(status == Right(Set("open", "run", "status", "resume", "export", "cancel", "find", "call")))
      }
    },
    test("the local listener stays open and the tunnel listener requires a JWT") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          dir    <- ZIO.acquireRelease(
            ZIO.attempt {
              val root = Path.of("target")
              Files.createDirectories(root)
              Files.createTempDirectory(root, "process-subscriptions")
            }
          )(root => ZIO.attempt(deleteTree(root)).ignore)
          tunnel <- freePort
          access <- ZIO.fromEither(tunnelAccess(tunnel))
          config = ProcessConfig.local.copy(
            listen = Listen.Http("127.0.0.1", 0),
            subscriptions = dir.resolve("subscriptions.json"),
            access = Some(access),
          )
          server      <- Process.bind(config, memory).provideSome[Scope](ZLayer.succeed(ModelCatalog.empty))
          local       <- server.port
          open        <- sessions(local)
          denied      <- sessions(tunnel)
          localWatch  <- exchange(local, "GET", "/watch", None, "CF-Connecting-IP" -> "203.0.113.4")
          tunnelWatch <- exchange(tunnel, "GET", "/watch", None, "CF-Connecting-IP" -> "203.0.113.4")
          mcp         <- listed(tunnel)
        yield assertTrue(
          open.status == 200,
          open.body.contains("["),
          open.headers.get("X-Content-Type-Options").contains("nosniff"),
          open.headers.get("X-Frame-Options").contains("DENY"),
          open.headers.get("Referrer-Policy").contains("strict-origin"),
          open.headers.get("Strict-Transport-Security").isEmpty,
          open.headers.get("Access-Control-Allow-Origin").isEmpty,
          denied.status == 401,
          denied.body.contains("access denied"),
          denied.headers.get("Strict-Transport-Security").exists(_.contains("max-age=31536000")),
          localWatch.status == 404,
          localWatch.headers.get("Strict-Transport-Security").isEmpty,
          localWatch.headers.get("X-Content-Type-Options").contains("nosniff"),
          tunnelWatch.status == 401,
          tunnelWatch.body.contains("access denied"),
          tunnelWatch.headers.get("X-Content-Type-Options").contains("nosniff"),
          tunnelWatch.headers.get("X-Frame-Options").contains("DENY"),
          tunnelWatch.headers.get("Referrer-Policy").contains("strict-origin"),
          tunnelWatch.headers.get("Content-Security-Policy").exists(_.contains("frame-ancestors 'none'")),
          tunnelWatch.headers.get("Strict-Transport-Security").exists(_.contains("max-age=31536000")),
          tunnelWatch.headers.get("Access-Control-Allow-Origin").isEmpty,
          mcp == Right(Set("open", "run", "status", "resume", "export", "cancel", "find", "call")),
        )
      }
    },
    test("a cross-site run does not start a turn") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          dir    <- ZIO.acquireRelease(
            ZIO.attempt {
              val root = Path.of("target")
              Files.createDirectories(root)
              Files.createTempDirectory(root, "process-subscriptions")
            }
          )(root => ZIO.attempt(deleteTree(root)).ignore)
          space <- ZIO.acquireRelease(
            ZIO.attempt {
              val root = Path.of("target")
              Files.createDirectories(root)
              Files.createTempDirectory(root, "watch-workspace")
            }
          )(root => ZIO.attempt(deleteTree(root)).ignore)
          config = ProcessConfig.local.copy(
            listen = Listen.Http("127.0.0.1", 0),
            subscriptions = dir.resolve("subscriptions.json"),
          )
          server <- Process.bind(config, memory).provideSome[Scope](ZLayer.succeed(ModelCatalog.empty))
          port   <- server.port
          opened <- exchange(
            port,
            "POST",
            "/sessions",
            Some(OpenJob(space.toAbsolutePath.toString, Chunk.empty[Permission]).toJson),
            "Content-Type" -> "application/json",
            "Accept"       -> "application/json",
          )
          session <- decoded[Opened](opened.body)
          run = RunJob(session.session, "go").toJson
          refused <- exchange(
            port,
            "POST",
            "/sessions/run",
            Some(run),
            "Content-Type"   -> "application/json",
            "Origin"         -> "https://evil.example",
            "Sec-Fetch-Site" -> "cross-site",
          )
          held <- exchange(
            port,
            "POST",
            "/sessions/status",
            Some(SessionJob(session.session).toJson),
            "Content-Type" -> "application/json",
            "Accept"       -> "application/json",
          )
          view    <- decoded[StatusView](held.body)
          allowed <- exchange(
            port,
            "POST",
            "/sessions/run",
            Some(run),
            "Content-Type" -> "application/json",
            "Accept"       -> "application/json",
          )
          done <- untilEnded(port, session.session, 40)
        yield assertTrue(
          opened.status == 200,
          refused.status == 403,
          refused.body.contains("access denied"),
          view.state == WireState.Open,
          allowed.status == 200,
          (done.state match
            case WireState.Ended(_)                 => true
            case WireState.Open | WireState.Running => false
          ),
        )
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

  private def tunnelAccess(port: Int): Either[ProcessError, AccessConfig] =
    for
      team <- TeamHost.from("example.cloudflareaccess.com").left.map(ProcessError.Access(_))
      tag  <- Audience.from("aud").left.map(ProcessError.Access(_))
    yield AccessConfig(team, tag, port)

  private def freePort: ZIO[Any, Throwable, Int] =
    ZIO.attempt {
      val socket = new java.net.ServerSocket(0)
      val port   = socket.getLocalPort
      socket.close()
      port
    }

  private final case class HttpText(status: Int, body: String, headers: Map[String, String])

  private def sessions(port: Int): ZIO[Any, Throwable, HttpText] =
    exchange(
      port,
      "GET",
      "/sessions",
      None,
      "Accept"           -> "application/json",
      "CF-Connecting-IP" -> "203.0.113.4",
    )

  private def exchange(
      port: Int,
      method: String,
      path: String,
      body: Option[String],
      headers: (String, String)*
  ): ZIO[Any, Throwable, HttpText] =
    ZIO.scoped {
      ZIO
        .acquireRelease(ZIO.attempt(HttpClient.newHttpClient()))(client => ZIO.attempt(client.close()).ignore)
        .flatMap { client =>
          ZIO.attemptBlocking {
            val builder = HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$path"))
            headers.foreach((name, value) => builder.header(name, value))
            val publisher = body match
              case Some(text) => HttpRequest.BodyPublishers.ofString(text)
              case None       => HttpRequest.BodyPublishers.noBody()
            method match
              case "GET"  => builder.GET()
              case "POST" => builder.POST(publisher)
              case other  => builder.method(other, publisher)
            val response = client.send(builder.build(), HttpResponse.BodyHandlers.ofString())
            val kept     = List(
              "X-Content-Type-Options",
              "X-Frame-Options",
              "Referrer-Policy",
              "Content-Security-Policy",
              "Strict-Transport-Security",
              "Access-Control-Allow-Origin",
            ).flatMap(name => response.headers().firstValue(name).toScala.map(name -> _)).toMap
            HttpText(response.statusCode(), Option(response.body()).getOrElse(""), kept)
          }
        }
    }

  private def decoded[A: JsonDecoder](body: String): ZIO[Any, Throwable, A] =
    ZIO.fromEither(body.fromJson[A]).mapError(message => new IllegalArgumentException(message))

  private def untilEnded(port: Int, session: String, left: Int): ZIO[Any, Throwable, StatusView] =
    exchange(
      port,
      "POST",
      "/sessions/status",
      Some(SessionJob(session).toJson),
      "Content-Type" -> "application/json",
      "Accept"       -> "application/json",
    ).flatMap { reply =>
      decoded[StatusView](reply.body).flatMap { view =>
        view.state match
          case WireState.Ended(_)                 => ZIO.succeed(view)
          case WireState.Open | WireState.Running =>
            if left <= 0 then ZIO.fail(new IllegalStateException(reply.body))
            else ZIO.sleep(25.millis) *> untilEnded(port, session, left - 1)
      }
    }

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

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walked = Files.walk(root)
      try
        val paths = walked.iterator.asScala.toVector.sortBy(_.getNameCount)(using Ordering[Int].reverse)
        paths.foreach(Files.deleteIfExists(_))
      finally walked.close()
end ProcessSpec
