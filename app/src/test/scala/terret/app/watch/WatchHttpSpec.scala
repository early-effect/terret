package terret.app.watch

import heddle.http.{Body, Form, MediaType, Method, Request, Response}
import heddle.route.{Handler, PathDsl, Routes}
import slayer.*
import terret.Terret
import terret.app.{AccessGate, AuthLog, Ingress}
import terret.mcp.EventsKey
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.session.SessionId
import terret.workspace.Workspace
import zio.test.*
import zio.{Chunk, Ref, ZIO}

object WatchHttpSpec extends ZIOSpecDefault:
  def spec = suite("watch http")(
    test("a cross-site post is refused before the handler") {
      val cross = posted("/watch/open")
        .withHeader("Origin", "https://evil.example")
        .withHeader("Host", "127.0.0.1:8081")
        .withHeader("Sec-Fetch-Site", "cross-site")
      val plain = Request
        .post("/sessions/run", Body.text("{}", MediaType.Json))
        .withHeader("Content-Type", "text/plain")
      val json = Request
        .post("/sessions/run", Body.text("{}", MediaType.Json))
        .withHeader("Content-Type", "application/json")
      val mcp = Request
        .post("/mcp", Body.text("{}", MediaType.Json))
        .withHeader("Content-Type", "application/json")
      assertTrue(
        WatchSecurity.screen(cross).exists(_.status.code == 403),
        WatchSecurity.screen(plain).exists(_.status.code == 415),
        WatchSecurity.screen(json).isEmpty,
        WatchSecurity.screen(mcp).isEmpty,
      )
    },
    test("a watch form needs a same-site origin and a urlencoded body") {
      val naked = Request
        .post("/watch/open", Body.form(Form("workspace" -> "/allowed")))
        .withHeader("Content-Type", "application/x-www-form-urlencoded")
      val json = sameSite("/watch/open").withHeader("Content-Type", "application/json")
      assertTrue(
        WatchSecurity.screen(naked).exists(_.status.code == 403),
        WatchSecurity.screen(json).exists(_.status.code == 415),
        WatchSecurity.screen(sameSite("/watch/open")).isEmpty,
      )
    },
    test("a cross-site watch post does not call steer") {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          streams <- WatchStreams.make
          csrf    <- CsrfTokens.make
          steer   <- ZIO.service[Steer]
          routes = WatchRoutes.routes(terret, streams, steer, csrf)
          response <- routes(posted("/watch/open")
            .withHeader("Origin", "https://evil.example")
            .withHeader("Host", "127.0.0.1:8081")
            .withHeader("Sec-Fetch-Site", "cross-site")).provide(AuthLog.live)
        yield assertTrue(response.status.code == 403, response.header("Access-Control-Allow-Origin").isEmpty)
      }.provide(stubbed[Steer])
    },
    test("a watch post without the synchronizer token does not call steer") {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          streams <- WatchStreams.make
          csrf    <- CsrfTokens.make
          steer   <- ZIO.service[Steer]
          routes = WatchRoutes.routes(terret, streams, steer, csrf)
          response <- routes(sameSite("/watch/open")).provide(AuthLog.live)
        yield assertTrue(response.status.code == 401)
      }.provide(stubbed[Steer])
    },
    test("the token that the page minted accepts the form") {
      ZIO.scoped {
        for
          memory  <- InMemory.make(Script.echo)
          terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          space   <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
          session <- terret.open(space).mapError(_.toString)
          streams <- WatchStreams.make
          csrf    <- CsrfTokens.make
          token   <- csrf.issue("anonymous")
          seen    <- Ref.make(Option.empty[String])
          _       <- stub[Steer](_.queue(any[SessionId]))((_: SessionId) => ZIO.succeed(Chunk.empty))
          _       <- stub[Steer](_.send(any[SessionId], any[String], any[String]))((_, text: String, _) =>
            seen.set(Some(text))
          )
          steer  <- ZIO.service[Steer]
          routes = WatchRoutes.routes(terret, streams, steer, csrf)
          sent   <- routes(sameSite(s"/watch/${session.id.value}/run", "csrf" -> token, "prompt" -> "hello"))
            .provide(AuthLog.live)
          got <- seen.get
        yield assertTrue(sent.status.code == 303, got.contains("hello"))
      }.provide(stubbed[Steer])
    },
    test("the local listener does not serve a watch post") {
      for
        hits <- Ref.make(0)
        key <- watchKey
        routes = IngressRoutes.counting(hits)
        guarded = Ingress.local(routes, key)
        response <- guarded(sameSite("/watch/open")).provide(AccessGate.open, AuthLog.live)
        calls    <- hits.get
      yield assertTrue(response.status.code == 404, calls == 0)
    },
    test("a non-json mutation does not reach the handler") {
      for
        hits <- Ref.make(0)
        key <- watchKey
        routes = IngressRoutes.counting(hits)
        guarded = Ingress.local(routes, key)
        response <- guarded(
          Request
            .post("/sessions/run", Body.text("{}", MediaType.Json))
            .withHeader("Content-Type", "text/plain")
        ).provide(AccessGate.open, AuthLog.live)
        calls <- hits.get
      yield assertTrue(response.status.code == 415, calls == 0)
    },
  )

  private def watchKey: ZIO[Any, Nothing, EventsKey] =
    ZIO
      .fromEither(EventsKey.from("watch-test-key").toRight(new IllegalArgumentException("watch key")))
      .orDie

  private def posted(path: String): Request =
    Request
      .post(path, Body.form(Form("workspace" -> "/allowed")))
      .withHeader("Content-Type", "application/x-www-form-urlencoded")

  private def sameSite(path: String, fields: (String, String)*): Request =
    posted(path)
      .copy(body = Body.form(Form(fields*)))
      .withHeader("Host", "127.0.0.1:8081")
      .withHeader("Origin", "http://127.0.0.1:8081")
      .withHeader("Sec-Fetch-Site", "same-origin")
      .withHeader("Content-Type", "application/x-www-form-urlencoded")

end WatchHttpSpec

private object IngressRoutes:
  def counting(hits: Ref[Int]): Routes[Any, Nothing] =
    import PathDsl.*
    Routes(
      Method.POST / "sessions" / "run" -> Handler.fromFunctionZIO { _ =>
        hits.update(_ + 1).as(Response.text("ran"))
      }
    )
end IngressRoutes
