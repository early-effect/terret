package terret.app.watch

import heddle.http.Request
import heddle.sse.SseCodec
import terret.Terret
import terret.app.AuthLog
import terret.config.TerretConfig
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.prompt.Prompt
import terret.session.{SessionId, SessionState}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.durationInt
import zio.{Chunk, Queue, Ref, Scope, ZIO}

import java.time.Instant

object WatchSpec extends ZIOSpecDefault:
  def spec = suite("watch page")(
    test("the query round-trips, and only a non-default control is written") {
      val query = WatchQuery.default.copy(
        keys = Keys.Plain,
        layout = Layout.Panel,
        output = OutputSize.Lines,
        inactive = Inactive.Hide,
        kinds = WatchQuery.DefaultKinds + WatchKind.Thought,
        tool = Some("shell"),
        status = Some("ok"),
        q = Some("find <me>"),
        gone = Set("s1"),
        open = Set("row-1"),
        row = Some("row-1"),
      )
      val rendered = WatchQuery.render(query)
      val parsed   = WatchQuery.parse(heddle.http.QueryParams.decode(rendered))
      assertTrue(
        parsed == Right(query),
        rendered.contains("keys=plain"),
        rendered.contains("layout=panel"),
        rendered.contains("output=20"),
        rendered.contains("inactive=hide"),
        rendered.contains("Thought"),
        WatchQuery.render(WatchQuery.default).isEmpty,
        WatchQuery.path(Some("abc"), WatchQuery.default) == "/watch/abc",
        WatchQuery.searchCss(WatchQuery.default.copy(q = Some("""</style><script>"""))).exists { css =>
          !css.contains("</style>") && !css.contains("<script>")
        },
      )
    },
    test("an oversized or unknown query is refused without the value") {
      val huge = "q" -> ("a" * 201)
      val bad  = WatchQuery.parse(heddle.http.QueryParams.of(huge))
      val odd  = WatchQuery.parse(heddle.http.QueryParams.of("nope" -> "x"))
      assertTrue(
        bad == Left(WatchQueryError.Oversized("q")),
        odd == Left(WatchQueryError.Bad("nope")),
      )
    },
    test("the outbound window drops the oldest rows") {
      val rows = Chunk.fromIterable((0 until 5001).map { n =>
        WatchRow(n.toString, None, WatchKind.Message, None, None, None, "x", Instant.EPOCH, n.toLong)
      })
      val kept = WatchFeed.slide(rows)
      val huge = "y" * (WatchFeed.MaxBytes + 1)
      val over = WatchFeed.slide(
        Chunk(
          WatchRow("old", None, WatchKind.Message, None, None, None, huge, Instant.EPOCH, 0),
          WatchRow("new", None, WatchKind.Message, None, None, None, "z", Instant.EPOCH, 1),
        )
      )
      assertTrue(
        kept.length == 5000,
        kept.headOption.map(_.id).contains("1"),
        kept.lastOption.map(_.id).contains("5000"),
        over.map(_.id) == Chunk("new"),
      )
    },
    test("a patch frame is the datastar dialect") {
      val elements = WatchPatch.elements("<div id=\"feed\">a &lt; b</div>", 4)
      val signals  = WatchPatch.signals("live")
      val bad      = WatchPatch.parse("datastar-patch-elements", "mode sideways\nelements <div></div>")
      assertTrue(
        elements.event.map(_.value).contains("datastar-patch-elements"),
        elements.id.map(_.value).contains("4"),
        WatchPatch.parse("datastar-patch-elements", elements.data) == Right(
          WatchFrame.Elements("#feed", "outer", "<div id=\"feed\">a &lt; b</div>", true)
        ),
        WatchPatch.parse("datastar-patch-signals", signals.data).exists {
          case WatchFrame.Signals(json, only) => json.contains("\"follow\"") && only
          case _                              => false
        },
        bad == Left(WatchPatchError.Mode("sideways")),
      )
    },
    test("the page redacts, escapes, and stays on GET links") {
      ZIO.scoped {
        for
          terret <- started(leaky)
          id     <- only(terret)
          _      <- awaitEnded(terret, id)
          body   <- html(terret, s"/watch/${id.value}")
          hidden <- html(terret, s"/watch/${id.value}?kinds=Thought,Message")
        yield assertTrue(
          body.contains("data-chrome=\"phone\""),
          body.contains("data-chrome-part=\"picker\""),
          body.contains("data-chrome-part=\"toolbar\""),
          body.contains("data-chip"),
          body.contains("data-composer=\"live\""),
          body.contains("data-jump=\"latest\""),
          body.contains("data-control=\"new-session\""),
          body.contains("data-control=\"clear-inactive\""),
          body.contains("data-layout=\"tree\""),
          body.contains("data-output=\"64k\""),
          body.contains("data-keys=\"vim\""),
          body.contains("[redacted]"),
          body.contains("&lt;b&gt;"),
          !body.contains("ghp_"),
          !body.contains("xai-secretvalue"),
          !body.contains("user@example.com"),
          !body.contains("hunter2"),
          !body.contains("hidden thought"),
          hidden.contains("hidden thought") && !hidden.contains("ghp_"),
          !body.contains("/sessions"),
          !body.contains("data-composer=\"inert\""),
          body.contains("action=\"/watch/open\""),
          body.contains(s"""action="/watch/${id.value}/run""""),
          body.contains(s"""action="/watch/${id.value}/cancel""""),
          body.contains(s"""action="/watch/${id.value}/resume""""),
          body.contains("name=\"csrf\""),
          body.contains("Stop and send"),
          body.contains("data-keys-choice=\"plain\""),
          phone(body).contains("data-chrome-part=\"picker\""),
          !phone(body).contains("data-composer"),
        )
      }
    },
    test("layout, output, and keys are in the markup") {
      ZIO.scoped {
        for
          terret <- started(leaky)
          id     <- only(terret)
          panel  <- html(terret, s"/watch/${id.value}?layout=panel&output=full&keys=plain")
          lines  <- html(terret, s"/watch/${id.value}?output=20")
          full   <- html(terret, s"/watch/${id.value}?layout=timeline&output=64k")
        yield assertTrue(
          panel.contains("data-layout=\"panel\""),
          panel.contains("data-panel=\"detail\""),
          panel.contains("data-output=\"full\""),
          panel.contains("data-keys=\"plain\""),
          lines.contains("data-output=\"20\""),
          full.contains("data-layout=\"timeline\""),
          full.contains("data-output=\"64k\""),
        )
      }
    },
    test("an unknown session is a generic denial and does not take a stream") {
      ZIO.scoped {
        for
          opened  <- openOnly
          missing <- opened._1(Request.get("/watch/missing"))
          held    <- opened._2.held("anonymous")
          journal <- opened._3.list
        yield assertTrue(
          missing.status.code == 401,
          missing.body.text.contains("access denied"),
          !missing.body.text.exists(_.contains("missing")),
          held == 0,
          journal.length == 1,
          journal.forall(_.id.value != "missing"),
        )
      }
    },
    test("eight streams per viewer, then a ninth is refused") {
      ZIO.scoped {
        for
          opened  <- openOnly
          id      <- only(opened._3)
          _       <- ZIO.foreachDiscard(1 to 8)(_ => opened._2.acquire("anonymous"))
          busy    <- opened._1(Request.get(s"/watch/${id.value}/events"))
          _       <- opened._2.release("anonymous")
          allowed <- opened._1(Request.get(s"/watch/${id.value}/events"))
        yield assertTrue(busy.status.code == 429, allowed.status.code == 200)
      }
    },
    test("the events stream resumes after Last-Event-ID and heartbeats on the test clock") {
      ZIO.scoped {
        for
          terret <- started(scripted)
          id     <- only(terret)
          _      <- awaitEnded(terret, id)
          bad    <- ask(terret, Request.get(s"/watch/${id.value}/events?after=-1"))
          routes <- routesOf(terret)
          live   <- routes(Request.get(s"/watch/${id.value}/events").withHeader("Last-Event-ID", "0"))
            .provide(AuthLog.live)
          seen    <- Queue.unbounded[heddle.sse.ServerSentEvent]
          pulling <- SseCodec.stream(live.body.toStream).mapError(_.toString).foreach(seen.offer).fork
          _       <- TestClock.adjust(15.seconds)
          taken   <- seen.takeAll
          _       <- pulling.interrupt
          quiet   <- routes(Request.get(s"/watch/${id.value}/events?after=99")).provide(AuthLog.live)
          raw     <- Ref.make("")
          beating <- quiet.body.toStream.chunks
            .mapError(_.toString)
            .foreach { chunk =>
              val text = new String(chunk.toArray, java.nio.charset.StandardCharsets.US_ASCII)
              raw.update(_ + text)
            }
            .fork
          ping <- awaitPing(raw, 5)
          _    <- beating.interrupt
        yield assertTrue(
          bad.status.code == 400,
          bad.body.text.contains("bad query"),
          taken.exists(_.data.contains("visible-tail")),
          taken.exists(_.id.map(_.value).contains("1")),
          ping.contains(": ping"),
          !ping.contains("visible-tail"),
        )
      }
    } @@ TestAspect.timeout(20.seconds),
    test("clearing inactive sessions does not drop the journal") {
      ZIO.scoped {
        for
          memory  <- InMemory.make(scripted)
          terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          space   <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
          session <- terret.open(space).mapError(_.toString)
          _       <- terret.start(session.id, Prompt("go")).mapError(_.toString)
          _       <- awaitEnded(terret, session.id)
          before  <- terret.page(session.id, None).mapError(_.toString)
          page    <- ask(terret, Request.get(s"/watch?gone=${session.id.value}&inactive=show"))
          after   <- terret.page(session.id, None).mapError(_.toString)
          stopped <- memory.cancelled
          body = page.body.text.getOrElse("")
        yield assertTrue(
          before.events.nonEmpty,
          after == before,
          body.contains("data-control=\"clear-inactive\""),
          body.contains(s"gone=${session.id.value}"),
          !body.contains(s"""data-session="${session.id.value}""""),
          stopped.isEmpty,
        )
      }
    },
    test("security headers are present and CORS stays closed") {
      ZIO.scoped {
        for
          terret <- openOnly
          id     <- only(terret._3)
          page   <- terret._1(Request.get(s"/watch/${id.value}"))
          headers = page.headers
        yield assertTrue(
          page.status.code == 200,
          headers.get("X-Content-Type-Options").contains("nosniff"),
          headers.get("X-Frame-Options").contains("DENY"),
          headers.get("Referrer-Policy").contains("strict-origin"),
          headers.get("Content-Security-Policy").exists { policy =>
            policy.contains("default-src 'self'") &&
            policy.contains("frame-ancestors 'none'") &&
            policy.contains("script-src 'self'") &&
            policy.contains("form-action 'self'") &&
            !policy.contains("unsafe-inline")
          },
          headers.get("Strict-Transport-Security").exists(_.contains("max-age=31536000")),
          headers.get("Access-Control-Allow-Origin").isEmpty,
          !WatchAssets.js.contains("/sessions"),
          !WatchAssets.js.contains("cancel"),
        )
      }
    },
    test("a cross-site mutation is refused and a bare mcp post is not") {
      val cross = Request
        .post("/sessions/run", heddle.http.Body.empty)
        .withHeader("Origin", "https://evil.example")
        .withHeader("Host", "127.0.0.1:8080")
        .withHeader("Sec-Fetch-Site", "cross-site")
      val local = Request.post("/mcp", heddle.http.Body.empty)
      val same  = Request
        .post("/sessions/run", heddle.http.Body.empty)
        .withHeader("Host", "127.0.0.1:9")
      assertTrue(
        WatchSecurity.crossSite(cross).exists(_.status.code == 403),
        WatchSecurity.crossSite(local).isEmpty,
        WatchSecurity.crossSite(same).isEmpty,
        WatchSecurity.mcp(Request.get("/mcp")),
        !WatchSecurity.mcp(Request.get("/watch")),
      )
    },
  )

  private val leaky: Script = Script: (_, _) =>
    ZStream(
      Event.Thought("hidden thought ghp_Abcdefghij"),
      Event.Assistant("ghp_Abcdefghij Bearer hunter2token xai-secretvalue x-terret-key: hunter2 user@example.com <b>"),
      Event.TurnFinished(TurnOutcome.Completed),
    )

  private val scripted: Script = Script: (_, _) =>
    ZStream(
      Event.ToolStarted(ToolCall(CallId("c1"), ToolName("shell"), Json.Obj())),
      Event.ToolUpdated(
        ToolCall(CallId("c1"), ToolName("shell"), Json.Obj("output" -> Json.Str("visible-tail")))
      ),
      Event.TurnFinished(TurnOutcome.Completed),
    )

  private def started(script: Script): ZIO[Scope, String, Terret] =
    for
      memory  <- InMemory.make(script)
      terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
      space   <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
      session <- terret.open(space).mapError(_.toString)
      _       <- terret.start(session.id, Prompt("go")).mapError(_.toString)
    yield terret

  private def openOnly: ZIO[Scope, String, (Request => ZIO[Any, Nothing, heddle.http.Response], WatchStreams, Terret)] =
    for
      memory  <- InMemory.make(scripted)
      terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
      streams <- WatchStreams.make
      space   <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
      _       <- terret.open(space).mapError(_.toString)
      routes  <- wired(terret, streams)
    yield (
      (request: Request) => routes(request).provide(AuthLog.live),
      streams,
      terret,
    )

  private def routesOf(terret: Terret): ZIO[Scope, Nothing, heddle.route.Routes[AuthLog, Nothing]] =
    WatchStreams.make.flatMap(wired(terret, _))

  private def wired(
      terret: Terret,
      streams: WatchStreams,
  ): ZIO[Scope, Nothing, heddle.route.Routes[AuthLog, Nothing]] =
    for
      csrf  <- CsrfTokens.make
      steer <- Steer.make(terret, Chunk.empty)
    yield WatchRoutes.routes(terret, streams, steer, csrf)

  private def ask(terret: Terret, request: Request): ZIO[Scope, Nothing, heddle.http.Response] =
    routesOf(terret).flatMap(routes => routes(request).provide(AuthLog.live))

  private def html(terret: Terret, path: String): ZIO[Scope, Nothing, String] =
    ask(terret, Request.get(path)).map(_.body.text.getOrElse(""))

  private def only(terret: Terret): ZIO[Any, String, SessionId] =
    terret.list.flatMap { rows =>
      ZIO.fromOption(rows.headOption.map(_.id)).orElseFail("no session")
    }

  private def phone(html: String): String =
    html.split("data-chrome=\"desk\"").headOption.getOrElse("")

  /** `: ping` is a comment frame. The event decoder drops it, so the proof reads the bytes. */
  private def awaitPing(raw: Ref[String], left: Int): ZIO[Any, String, String] =
    raw.get.flatMap { got =>
      if got.contains(": ping") then ZIO.succeed(got)
      else if left <= 0 then ZIO.fail("no heartbeat")
      else ZIO.yieldNow *> TestClock.adjust(15.seconds) *> awaitPing(raw, left - 1)
    }

  /** The drain is forked onto Terret's scope. Polling with `repeatUntil` suspends on the test clock
    * between reads, which is what lets that fiber run. A bare `yieldNow` loop does not.
    */
  private def awaitEnded(terret: Terret, id: SessionId): ZIO[Any, String, Unit] =
    terret
      .status(id)
      .mapError(_.toString)
      .repeatUntil { session =>
        session.state match
          case SessionState.Ended(_) => true
          case _                     => false
      }
      .unit
end WatchSpec
