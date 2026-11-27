package terret.mcp

import heddle.{Body, LinePipe, Request, Response, Status}
import heddle.mcp.Mcp
import heddle.mcp.events.{Events, WebhookTransport}
import heddle.mcp.protocol.{
  Cancelled,
  ClientRequest,
  Message,
  Methods,
  Notifications,
  ProtocolVersion,
  RequestId,
  RpcError,
  ToolName,
}
import heddle.mcp.transport.Http
import heddle.sse.{ServerSentEvent, SseCodec}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{Event, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.Milestones
import terret.model.{CatalogError, ModelCatalog, ModelKey}
import terret.find.{
  Access,
  Enable,
  Find,
  FindError,
  FindResult,
  Grant,
  NameError,
  Runner,
  ServerName,
  ToolId,
  ToolRef,
  Withheld,
}
import terret.permission.{Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.durationInt
import zio.{Chunk, IO, Promise, Queue, Ref, Scope, UIO, ZIO, ZLayer}

object McpJobsSpec extends ZIOSpecDefault:
  private val jobs = Set("open", "run", "status", "resume", "export", "cancel", "find", "call")

  private type Env = Milestones & Events.Env

  def spec = suite("MCP jobs")(
    test("an empty server version is refused") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          failed = TerretMcp.mcp(terret, "")
        yield assertTrue(failed == Left(TerretMcpError.EmptyVersion))
      }
    },
    test("the pipe lists the jobs, run accepts, and export matches resume") {
      for
        calls <- Ref.make(0)
        out   <- hosted(counting(calls)) { (_, host) =>
          for
            listed   <- host.list
            opened   <- host.open
            ran      <- host.call("run", RunJob(opened.session, "hello"))
            ended    <- host.untilEnded(opened.session)
            exported <- host.call("export", SessionJob(opened.session))
            resumed  <- host.call("resume", SessionJob(opened.session))
            prompts  <- calls.get
          yield assertTrue(
            listed.toSet == jobs,
            accepted(ran) == Right(RunAccepted(opened.session, WireState.Running)),
            ended.state == WireState.Ended(WireOutcome.Completed),
            log(exported) == Right(Log(opened.session, Chunk(WireEvent.Assistant("hello"), finished), Some(1), done)),
            log(exported) == log(resumed),
            prompts == 1,
          )
        }
      yield out
    },
    test("a second run is busy until cancel, then another run starts") {
      for
        started <- Ref.make(0)
        hold    <- Promise.make[Nothing, Unit]
        seen    <- Promise.make[Nothing, Unit]
        out     <- hosted(holding(started, seen, hold)) { (memory, host) =>
          for
            opened  <- host.open
            ran     <- host.call("run", RunJob(opened.session, "stay"))
            _       <- seen.await
            busy    <- host.call("run", RunJob(opened.session, "no"))
            _       <- host.call("cancel", SessionJob(opened.session))
            _       <- memory.cancelled.repeatUntil(_.nonEmpty)
            again   <- host.call("run", RunJob(opened.session, "next"))
            _       <- untilCount(started, 2)
            _       <- hold.succeed(())
            ended   <- host.untilEnded(opened.session)
            stopped <- memory.cancelled
          yield assertTrue(
            accepted(ran) == Right(RunAccepted(opened.session, WireState.Running)),
            failure(busy) == Right(JobFailure.Busy(opened.session)),
            accepted(again) == Right(RunAccepted(opened.session, WireState.Running)),
            ended.state == WireState.Ended(WireOutcome.Completed),
            stopped.length == 1,
          )
        }
      yield out
    },
    test("notifications/cancelled leaves the turn running until it finishes") {
      for
        started <- Ref.make(0)
        hold    <- Promise.make[Nothing, Unit]
        seen    <- Promise.make[Nothing, Unit]
        out     <- hosted(holding(started, seen, hold)) { (memory, host) =>
          for
            opened   <- host.open
            id       <- host.begin("run", RunJob(opened.session, "stay"))
            _        <- seen.await
            ran      <- host.untilResult(id, 8)
            _        <- host.cancel(id)
            mid      <- memory.cancelled
            running  <- host.call("status", SessionJob(opened.session))
            _        <- hold.succeed(())
            ended    <- host.untilEnded(opened.session)
            exported <- host.call("export", SessionJob(opened.session))
          yield assertTrue(
            accepted(ran) == Right(RunAccepted(opened.session, WireState.Running)),
            mid.isEmpty,
            asStatus(running) == Right(StatusView(opened.session, WireState.Running)),
            ended.state == WireState.Ended(WireOutcome.Completed),
            log(exported) == Right(
              Log(opened.session, Chunk(WireEvent.Assistant("stay"), finished), Some(1), done)
            ),
          )
        }
      yield out
    },
    test("routes return the acceptance as an event stream") {
      ZIO.scoped {
        for
          (_, mcp) <- server(Script.echo)
          id       <- sessionId(mcp)
          streamed <- invoke(mcp, "run", RunJob(id, "hello"), 2, sse = true)
          events   <- SseCodec.stream(streamed.body.toStream).runCollect.mapError(Fail.Failed(_))
          messages = decoded(events)
          results  = messages.collect { case Message.Result(_, body) => accepted(Reply(body)) }
          notes    = messages.collect { case _: Message.Notification => () }
        yield assertTrue(
          streamed.status == Status.Ok,
          streamed.headers.contentType.exists(_.isEventStream),
          results == Chunk(Right(RunAccepted(id, WireState.Running))),
          notes.isEmpty,
        )
      }
    },
    test("routes return the acceptance as JSON when the client does not ask for an event stream") {
      ZIO.scoped {
        for
          (_, mcp) <- server(Script.echo)
          id       <- sessionId(mcp)
          plain    <- invoke(mcp, "run", RunJob(id, "hello"), 2, sse = false)
          text     <- plain.body.utf8.mapError(Fail.Failed(_))
          message = text.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption)
        yield assertTrue(
          plain.status == Status.Ok,
          plain.headers.contentType.exists(_.isJson),
          !text.contains("notifications/message"),
          message
            .collect { case Message.Result(_, body) => accepted(Reply(body)) }
            .contains(
              Right(RunAccepted(id, WireState.Running))
            ),
        )
      }
    },
    test("export after a cursor is the tail, and a negative cursor is refused") {
      hosted(Script.echo) { (_, host) =>
        for
          opened <- host.open
          empty  <- host.call("export", SessionJob(opened.session))
          _      <- host.call("run", RunJob(opened.session, "hello"))
          _      <- host.untilEnded(opened.session)
          whole  <- host.call("export", SessionJob(opened.session))
          tail   <- host.call("export", ExportJob(opened.session, Some(0)))
          rest   <- host.call("export", ExportJob(opened.session, Some(1)))
          ahead  <- host.call("export", ExportJob(opened.session, Some(9)))
          bad    <- host.call("export", ExportJob(opened.session, Some(-1)))
        yield assertTrue(
          log(empty) == Right(Log(opened.session, Chunk.empty, None, WireState.Open)),
          log(whole) == Right(Log(opened.session, Chunk(WireEvent.Assistant("hello"), finished), Some(1), done)),
          log(tail) == Right(Log(opened.session, Chunk(finished), Some(1), done)),
          log(rest) == Right(Log(opened.session, Chunk.empty, Some(1), done)),
          log(ahead) == log(rest),
          failure(bad) == Right(JobFailure.BadCursor),
        )
      }
    },
    test("follow is a subscription and the tool list is the jobs") {
      ZIO.scoped {
        for
          (_, mcp) <- server(Script.echo)
          listed   <- supply(mcp)(mcp.handle(Message.stateless(RequestId.Num(1), ClientRequest.ListTools(None)).json))
          found    <- supply(mcp)(mcp.handle(Message.stateless(RequestId.Num(2), ClientRequest.Discover).json))
          tools = listed.fold("")(_.toJson)
          disc  = found.fold("")(_.toJson)
        yield assertTrue(
          tools.contains("\"export\""),
          tools.contains("\"find\""),
          tools.contains("\"call\""),
          !tools.contains("\"follow\""),
          disc.contains("\"follow\""),
          disc.contains("subscriptions"),
        )
      }
    },
    test("listen catches up, then a live event, and dropping it leaves the turn running") {
      for
        caught  <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        live    <- Promise.make[Nothing, Unit]
        done    <- Promise.make[Nothing, Unit]
        out     <- ZIO.scoped {
          for
            (memory, mcp) <- server(paced(caught, release, live, done))
            id            <- sessionId(mcp)
            _             <- invoke(mcp, "run", RunJob(id, "go"), 2, sse = false)
            _             <- caught.await
            _             <- ZIO.scoped {
              for
                queue <- watch(mcp, id, None, 9)
                _     <- awaitEvent(queue, "0", "ToolStarted")
                _     <- release.succeed(())
                _     <- awaitEvent(queue, "1", "ToolUpdated")
              yield ()
            }
            mid   <- memory.cancelled
            _     <- done.succeed(())
            ids   <- Ref.make(10L)
            _     <- untilEnded(mcp, id, ids)
            later <- memory.cancelled
          yield assertTrue(mid.isEmpty, later.isEmpty)
        }
      yield out
    },
    test("a negative follow cursor fails the listen and does not acknowledge") {
      ZIO.scoped {
        for
          (_, mcp) <- server(Script.echo)
          id       <- sessionId(mcp)
          res      <- listen(mcp, id, Some(-1), 4)
          events   <- SseCodec.stream(res.body.toStream).runCollect.mapError(Fail.Failed(_))
          messages = decoded(events)
        yield assertTrue(
          messages.length == 1,
          messages.lift(0).exists {
            case Message.Error(_, RpcError.OperationFailed(data)) => data.exists(_.toJson.contains("BadCursor"))
            case _                                                => false
          },
          !messages.exists {
            case Message.Notification(Notifications.SubscriptionsAcknowledged, _) => true
            case _                                                                => false
          },
        )
      }
    },
    test("GET events is the follow stream, and a negative cursor is a bad request") {
      for
        caught  <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        live    <- Promise.make[Nothing, Unit]
        done    <- Promise.make[Nothing, Unit]
        out     <- ZIO.scoped {
          for
            memory <- InMemory.make(paced(caught, release, live, done))
            terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
            served <- ZIO.fromEither(TerretMcp.served(terret, "test")).mapError(Fail.Build(_))
            id     <- sessionId(served.mcp)
            _      <- invoke(served.mcp, "run", RunJob(id, "go"), 2, sse = false)
            _      <- caught.await
            bad    <- supply(served.mcp)(served.routes(Request.get(s"/sessions/$id/events?after=-1")))
              .mapError(Fail.Http(_))
            badId <- supply(served.mcp)(
              served.routes(Request.get(s"/sessions/$id/events").withHeader("Last-Event-ID", "-1"))
            ).mapError(Fail.Http(_))
            denied <- bad.body.utf8
              .mapError(Fail.Failed(_))
              .timeoutFail(Fail.Decode("the bad-cursor body did not end"))(5.seconds)
            streamed <- ZIO.scoped {
              for
                res   <- supply(served.mcp)(served.routes(Request.get(s"/sessions/$id/events"))).mapError(Fail.Http(_))
                queue <- wireEvents(res)
                _     <- awaitWire(queue, "0", "ToolStarted")
                resumed <- supply(served.mcp)(
                  served.routes(Request.get(s"/sessions/$id/events").withHeader("Last-Event-ID", "0"))
                ).mapError(Fail.Http(_))
                later <- wireEvents(resumed)
                _     <- release.succeed(())
                _     <- awaitWire(queue, "1", "ToolUpdated")
                _     <- awaitFirst(later, "1", "ToolUpdated")
              yield res
            }
            mid   <- memory.cancelled
            _     <- done.succeed(())
            ids   <- Ref.make(10L)
            _     <- untilEnded(served.mcp, id, ids)
            later <- memory.cancelled
          yield assertTrue(
            bad.status == Status.BadRequest,
            badId.status == Status.BadRequest,
            !bad.headers.contentType.exists(_.isEventStream),
            denied.contains("BadCursor"),
            streamed.status == Status.Ok,
            streamed.headers.contentType.exists(_.isEventStream),
            mid.isEmpty,
            later.isEmpty,
          )
        }
      yield out
    },
    test("an unknown session is the typed error") {
      hosted(Script.echo) { (_, host) =>
        for missed <- host.call("run", RunJob("missing", "hello"))
        yield assertTrue(failure(missed) == Right(JobFailure.UnknownSession("missing")))
      }
    },
    test("an empty session id is refused") {
      hosted(Script.echo) { (_, host) =>
        for missed <- host.call("export", SessionJob(""))
        yield assertTrue(failure(missed) == Right(JobFailure.EmptySession))
      }
    },
    test("a workspace outside the root is denied") {
      for
        root <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
        out  <- hosted(Script.echo, TerretConfig(PermissionPolicy.Closed, Some(root))) { (_, host) =>
          for denied <- host.call("open", OpenJob("/other", Chunk(Permission.ReadWorkspace)))
          yield assertTrue(
            failure(denied) == Right(JobFailure.WorkspaceDenied("/other", "/work"))
          )
        }
      yield out
    },
    test("an omitted model does not read the catalog") {
      catalogued(Script.echo, stubbed[ModelCatalog]) { host =>
        host.open.map(opened => assertTrue(opened.model.isEmpty))
      }
    },
    test("an unknown model names itself and lists the known keys") {
      catalogued(Script.echo, stubbed[ModelCatalog]) { host =>
        for
          _ <- stub[ModelCatalog](_.known)(
            ZIO.succeed(Chunk(ModelKey("grok-4.7"), ModelKey("grok-4.5")))
          )
          reply <- host.call("open", OpenJob("/work", Chunk(Permission.ReadWorkspace), Some("missing")))
        yield assertTrue(
          failure(reply) == Right(JobFailure.UnknownModel("missing", Chunk("grok-4.5", "grok-4.7")))
        )
      }
    },
    test("a known model is reported on the opened session") {
      catalogued(Script.echo, stubbed[ModelCatalog]) { host =>
        for
          _     <- stub[ModelCatalog](_.known)(ZIO.succeed(Chunk(ModelKey("grok-4.7"))))
          reply <- host.call("open", OpenJob("/work", Chunk(Permission.ReadWorkspace), Some("grok-4.7")))
          found <- ZIO.fromEither(structured(reply.result).flatMap(_.as[Opened])).mapError(Fail.Decode(_))
        yield assertTrue(found.model == Some("grok-4.7"))
      }
    },
    test("an unreadable model file fails open") {
      catalogued(Script.echo, stubbed[ModelCatalog]) { host =>
        for
          _     <- stub[ModelCatalog](_.known)(ZIO.fail(CatalogError.Unreadable("/var/terret/missing.toml")))
          reply <- host.call("open", OpenJob("/work", Chunk(Permission.ReadWorkspace), Some("grok-4.7")))
        yield assertTrue(
          failure(reply) == Right(JobFailure.ModelsUnreadable("/var/terret/missing.toml"))
        )
      }
    },
    test("open without a model field decodes, and a malformed catalog is a gateway error") {
      val job     = OpenJob("/work", Chunk.empty)
      val decoded = job.toJsonAST match
        case Right(obj: Json.Obj) =>
          Json.Obj(obj.fields.filter((name, _) => name != "model")).toJson.fromJson[OpenJob]
        case Right(other) => Left(other.toJson)
        case Left(error)  => Left(error)
      val opened = Opened("s", Chunk.empty, Chunk.empty, WireState.Open, Some("grok-4.7"))
      assertTrue(
        decoded == Right(job),
        opened.toJson.fromJson[Opened] == Right(opened),
        JobFailure.from(TerretError.Models(CatalogError.Malformed("/var/terret/grok.toml", 3))) ==
          JobFailure.ModelsMalformed("/var/terret/grok.toml", 3),
      )
    },
    test("call refuses a missing server, an ungranted tool, and a seed before the runner") {
      for
        built  <- ZIO.fromEither(scenario)
        runner <- ZIO.service[Runner].provide(stubbed[Runner])
        (access, searching) = built
        out <- toolsOf(ToolSurface.direct(searching, runner, access)) { host =>
          for
            unknown <- host.call("call", CallJob(Some("linear"), "list_issues", "{}"))
            denied  <- host.call("call", CallJob(Some("github"), "create_pr", "{}"))
            seed    <- host.call("call", CallJob(None, "triage-issue", "{}"))
            blank   <- host.call("call", CallJob(Some("  "), "create_pr", "{}"))
            badTool <- host.call("call", CallJob(Some("github"), " ", "{}"))
            badArgs <- host.call("call", CallJob(Some("github"), "search_code", "nope"))
            found   <- host.call("find", FindJob("open a pull request", 1))
          yield assertTrue(
            failure(unknown) == Right(JobFailure.UnknownServer("linear")),
            failure(denied) == Right(JobFailure.NotGranted("github", "create_pr")),
            failure(seed) == Right(JobFailure.NotCallable),
            failure(blank) == Right(JobFailure.NotCallable),
            failure(badTool) == Right(JobFailure.BadTool),
            failure(badArgs) == Right(JobFailure.BadArguments),
            foundOf(found) == Right(
              Found(
                Chunk(FoundTool("github", "search_code", "search code in the repository")),
                Chunk(WithheldTool("github", "create_pr", WireEnable.AddGrant("github/create_pr"))),
                None,
              )
            ),
          )
        }
      yield out
    },
    test("a granted call returns the runner text") {
      for
        built  <- ZIO.fromEither(scenario)
        runner <- (
          stub[Runner](_.call(any[ServerName], any[ToolId], any[String]))(ZIO.succeed("ok")) *>
            ZIO.service[Runner]
        ).provide(stubbed[Runner])
        (access, searching) = built
        out <- toolsOf(ToolSurface.direct(searching, runner, access)) { host =>
          host.call("call", CallJob(Some("histos"), "test", """{"suite":"grants"}""")).map { reply =>
            assertTrue(calledOf(reply) == Right(Called("ok")))
          }
        }
      yield out
    },
    test("an empty grant is NoGrants on the wire and call is NotGranted") {
      toolsOf(ToolSurface.closed) { host =>
        for
          found  <- host.call("find", FindJob("open a pull request", 1))
          denied <- host.call("call", CallJob(Some("linear"), "list_issues", "{}"))
        yield assertTrue(
          foundOf(found) == Right(Found(Chunk.empty, Chunk.empty, Some(WireHint.NoGrants))),
          failure(denied) == Right(JobFailure.NotGranted("linear", "list_issues")),
        )
      }
    },
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val openAll: OpenJob =
    OpenJob("/work", Chunk.fromIterable(Permission.values))

  private val finished: WireEvent = WireEvent.TurnFinished(WireOutcome.Completed)

  private val done: WireState = WireState.Ended(WireOutcome.Completed)

  /** Yields between reads. `repeatUntil` on the live clock does not, so it starves the forked turn. */
  private def untilCount(started: Ref[Int], n: Int): UIO[Unit] =
    started.get.flatMap {
      case got if got == n => ZIO.unit
      case _               => ZIO.yieldNow *> untilCount(started, n)
    }

  private val readCall =
    terret.event.ToolCall(terret.event.CallId("c1"), terret.event.ToolName("read"), Json.Str("body"))

  private def paced(
      caught: Promise[Nothing, Unit],
      release: Promise[Nothing, Unit],
      live: Promise[Nothing, Unit],
      done: Promise[Nothing, Unit],
  ): Script =
    Script((_, _) =>
      ZStream(Event.ToolStarted(readCall)) ++
        ZStream.fromZIO(caught.succeed(()) *> release.await).drain ++
        ZStream(Event.ToolUpdated(readCall)) ++
        ZStream.fromZIO(live.succeed(()) *> done.await).drain ++
        ZStream(Event.TurnFinished(TurnOutcome.Completed))
    )

  private def listen(mcp: Mcp[Env], session: String, after: Option[Long], id: Long): ZIO[Scope, Fail, Response] =
    for
      tool <- ZIO.fromEither(ToolName.from("follow")).mapError(_ => Fail.BadTool("follow"))
      args = after match
        case Some(n) => Json.Obj("session" -> Json.Str(session), "after" -> Json.Num(n))
        case None    => Json.Obj("session" -> Json.Str(session))
      message = Message.stateless(RequestId.Num(id), ClientRequest.Listen(tool, args))
      req     = Request
        .post("/mcp", Body.json(message.json.toJson))
        .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)
        .withHeader(Http.MethodHeader, Methods.Listen)
        .withHeader("Accept", "text/event-stream")
      res <- supply(mcp)(mcp.routes(req))
    yield res

  private def watch(
      mcp: Mcp[Env],
      session: String,
      after: Option[Long],
      id: Long,
  ): ZIO[Scope, Fail, Queue[Message]] =
    listen(mcp, session, after, id).flatMap(sse)

  private def sse(res: Response): ZIO[Scope, Fail, Queue[Message]] =
    for
      _ <- ZIO.when(res.status != Status.Ok) {
        res.body.utf8.mapError(Fail.Failed(_)).flatMap(text => ZIO.fail(Fail.Decode(text)))
      }
      queue <- Queue.unbounded[Message]
      _     <- SseCodec
        .stream(res.body.toStream)
        .mapError(Fail.Failed(_))
        .foreach { event =>
          event.data.fromJson[Json] match
            case Left(_)     => ZIO.unit
            case Right(json) =>
              Message.decode(json) match
                case Left(_)    => ZIO.unit
                case Right(msg) => queue.offer(msg).unit
        }
        .forkScoped
    yield queue

  private def wireEvents(res: Response): ZIO[Scope, Fail, Queue[Either[String, (String, WireEvent)]]] =
    for
      _ <- ZIO.when(res.status != Status.Ok) {
        res.body.utf8.mapError(Fail.Failed(_)).flatMap(text => ZIO.fail(Fail.Decode(text)))
      }
      queue <- Queue.unbounded[Either[String, (String, WireEvent)]]
      _     <- SseCodec
        .stream(res.body.toStream)
        .mapError(Fail.Failed(_))
        .foreach { event =>
          event.id match
            case None     => ZIO.unit
            case Some(id) =>
              event.data.fromJson[WireEvent] match
                case Left(detail) => queue.offer(Left(detail)).unit
                case Right(wire)  => queue.offer(Right(id.value -> wire)).unit
        }
        .forkScoped
    yield queue

  private def awaitFirst(
      queue: Queue[Either[String, (String, WireEvent)]],
      id: String,
      kind: String,
  ): ZIO[Any, Fail, Unit] =
    def go: ZIO[Any, Fail, Unit] =
      queue.poll.flatMap {
        case Some(Left(detail))                                                  => ZIO.fail(Fail.Decode(detail))
        case Some(Right(found -> wire)) if found == id && wireKind(wire) == kind => ZIO.unit
        case Some(Right(found -> wire))                                          =>
          ZIO.fail(Fail.Decode(s"first event was ${wireKind(wire)} id $found"))
        case None => ZIO.yieldNow *> go
      }
    go.timeoutFail(Fail.Decode(s"no first $kind event id $id"))(5.seconds)

  private def awaitWire(
      queue: Queue[Either[String, (String, WireEvent)]],
      id: String,
      kind: String,
  ): ZIO[Any, Fail, Unit] =
    def go: ZIO[Any, Fail, Unit] =
      queue.poll.flatMap {
        case Some(Left(detail))                                                  => ZIO.fail(Fail.Decode(detail))
        case Some(Right(found -> wire)) if found == id && wireKind(wire) == kind => ZIO.unit
        case Some(Right(_))                                                      => go
        case None                                                                => ZIO.yieldNow *> go
      }
    go.timeoutFail(Fail.Decode(s"no $kind event id $id"))(5.seconds)

  private def wireKind(event: WireEvent): String =
    event match
      case _: WireEvent.Assistant    => "Assistant"
      case _: WireEvent.Thought      => "Thought"
      case _: WireEvent.ToolStarted  => "ToolStarted"
      case _: WireEvent.ToolUpdated  => "ToolUpdated"
      case _: WireEvent.Plan         => "Plan"
      case _: WireEvent.Refused      => "Refused"
      case _: WireEvent.TurnFinished => "TurnFinished"
      case _: WireEvent.Activity     => "Activity"

  private def awaitEvent(queue: Queue[Message], id: String, kind: String): UIO[Unit] =
    queue.poll.flatMap {
      case Some(Message.Notification(Notifications.OperationEvent, params))
          if params.get("id").contains(Json.Str(id)) && params.get("data").exists(_.toJson.contains(kind)) =>
        ZIO.unit
      case Some(_) => awaitEvent(queue, id, kind)
      case None    => ZIO.yieldNow *> awaitEvent(queue, id, kind)
    }

  private def untilEnded(mcp: Mcp[Env], session: String, ids: Ref[Long]): ZIO[Scope, Fail, Unit] =
    ids.updateAndGet(_ + 1).flatMap { n =>
      invoke(mcp, "status", SessionJob(session), n, sse = false).flatMap { res =>
        res.body.utf8.mapError(Fail.Failed(_)).flatMap { text =>
          text.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption) match
            case Some(Message.Result(_, body)) if terminalStatus(body) => ZIO.unit
            case _                                                     => ZIO.yieldNow *> untilEnded(mcp, session, ids)
        }
      }
    }

  private def terminalStatus(body: Json.Obj): Boolean =
    asStatus(Reply(body)).toOption.exists(terminal)

  private def counting(calls: Ref[Int]): Script =
    Script((_, prompt) =>
      ZStream.unwrap(
        calls
          .update(_ + 1)
          .as(ZStream(Event.Assistant(prompt.text), Event.TurnFinished(TurnOutcome.Completed)))
      )
    )

  private def holding(started: Ref[Int], seen: Promise[Nothing, Unit], hold: Promise[Nothing, Unit]): Script =
    Script((_, prompt) =>
      ZStream.unwrap(
        started
          .updateAndGet(_ + 1)
          .as(
            ZStream(Event.Assistant(prompt.text)) ++
              ZStream.fromZIO(seen.succeed(()) *> hold.await).drain ++
              ZStream(Event.TurnFinished(TurnOutcome.Completed))
          )
      )
    )

  private def server(
      script: Script,
      config: TerretConfig = TerretConfig.closed,
      catalog: ModelCatalog = ModelCatalog.empty,
  ): ZIO[Scope, Fail, (InMemory, Mcp[Env])] =
    for
      memory <- InMemory.make(script)
      terret <- Terret.make(memory, config, Hooks.identity, catalog = catalog)
      mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(Fail.Build(_))
    yield (memory, mcp)

  private def toolsOf(tools: ToolSurface)(
      use: Host => ZIO[Any, Fail, TestResult]
  ): ZIO[Any, Fail, TestResult] =
    ZIO.scoped {
      for
        memory <- InMemory.make(Script.echo)
        terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
        mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test", tools = tools)).mapError(Fail.Build(_))
        pipes  <- LinePipe.connected
        (client, remote) = pipes
        fiber  <- supply(mcp)(mcp.stdio(remote)).forkScoped
        host   <- Host.make(client)
        result <- use(host).ensuring(client.close *> fiber.interrupt.ignore)
      yield result
    }

  private def scenario: Either[NameError, (Access, Find)] =
    for
      histos <- ServerName.from("histos")
      github <- ServerName.from("github")
      linear <- ServerName.from("linear")
      search <- ToolId.from("search_code")
      create <- ToolId.from("create_pr")
    yield
      val access = Access(
        Chunk(
          Grant(histos, None),
          Grant(github, Some(Set(search))),
          Grant(linear, None),
        ),
        Set(histos, github),
      )
      val searching = new Find:
        def find(query: String, k: Int, grant: Access): IO[FindError, FindResult] =
          Find.screen(query, k, grant).flatMap {
            case Find.Screened.Done(result) => ZIO.succeed(result)
            case Find.Screened.Query(_)     =>
              ZIO.succeed(
                FindResult(
                  Chunk(ToolRef(Some(github), search, "search code in the repository")),
                  Chunk(Withheld(github, create, Enable.AddGrant("github/create_pr"))),
                  None,
                )
              )
          }
      (access, searching)

  private def hosted(
      script: Script,
      config: TerretConfig = TerretConfig.closed,
      catalog: ModelCatalog = ModelCatalog.empty,
  )(
      use: (InMemory, Host) => ZIO[Any, Fail, TestResult]
  ): ZIO[Any, Fail, TestResult] =
    ZIO.scoped {
      for
        memory <- InMemory.make(script)
        terret <- Terret.make(memory, config, Hooks.identity, catalog = catalog)
        mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(Fail.Build(_))
        pipes  <- LinePipe.connected
        (client, remote) = pipes
        fiber  <- supply(mcp)(mcp.stdio(remote)).forkScoped
        host   <- Host.make(client)
        result <- use(memory, host).ensuring(client.close *> fiber.interrupt.ignore)
      yield result
    }

  private def catalogued[R <: ModelCatalog](script: Script, layer: ZLayer[Any, Nothing, R])(
      use: Host => ZIO[R, Fail, TestResult]
  ): ZIO[Any, Fail, TestResult] =
    ZIO
      .scoped {
        for
          catalog <- ZIO.service[ModelCatalog]
          memory  <- InMemory.make(script)
          terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity, catalog = catalog)
          mcp     <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(Fail.Build(_))
          pipes   <- LinePipe.connected
          (client, remote) = pipes
          fiber  <- supply(mcp)(mcp.stdio(remote)).forkScoped
          host   <- Host.make(client)
          result <- use(host).ensuring(client.close *> fiber.interrupt.ignore)
        yield result
      }
      .provide(layer)

  private def sessionId(mcp: Mcp[Env]): ZIO[Scope, Fail, String] =
    for
      opened <- invoke(mcp, "open", openAll, 1, sse = false)
      text   <- opened.body.utf8.mapError(Fail.Failed(_))
      id     <- ZIO.fromEither(sessionOf(text)).mapError(Fail.Decode(_))
    yield id

  private def sessionOf(text: String): Either[String, String] =
    for
      json    <- text.fromJson[Json]
      message <- Message.decode(json).left.map(_.json.toJson)
      body    <- message match
        case Message.Result(_, result) => Right(result)
        case other                     => Left(other.json.toJson)
      opened <- structured(body).flatMap(_.as[Opened])
    yield opened.session

  private def invoke[A: JsonEncoder](
      mcp: Mcp[Env],
      name: String,
      args: A,
      id: Long,
      sse: Boolean,
  ): ZIO[Scope, Fail, Response] =
    for
      encoded <- ZIO.fromEither(asObj(args)).mapError(_ => Fail.Undecodable)
      tool    <- ZIO.fromEither(ToolName.from(name)).mapError(_ => Fail.BadTool(name))
      res     <- supply(mcp)(mcp.routes(callRequest(tool, encoded, id, sse)))
    yield res

  private def supply[E, A](mcp: Mcp[Env])(effect: ZIO[Env, E, A]): ZIO[Scope, E, A] =
    (mcp.eventsLayer ++ WebhookTransport.unavailable ++ Milestones.live).build.flatMap { environment =>
      effect.provideEnvironment(environment)
    }

  private def callRequest(tool: ToolName, args: Json.Obj, id: Long, sse: Boolean): Request =
    val message = Message.stateless(RequestId.Num(id), ClientRequest.CallTool(tool, args))
    val base    = Request
      .post("/mcp", Body.json(message.json.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)
      .withHeader(Http.MethodHeader, Methods.CallTool)
      .withHeader(Http.NameHeader, tool.value)
    if sse then base.withHeader("Accept", "application/json, text/event-stream")
    else base.withHeader("Accept", "application/json")

  private def asObj[A: JsonEncoder](value: A): Either[String, Json.Obj] =
    value.toJsonAST.flatMap {
      case obj: Json.Obj => Right(obj)
      case other         => Left(other.toJson)
    }

  private def decoded(events: Chunk[ServerSentEvent]): Chunk[Message] =
    events.flatMap(event => event.data.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption))

  private def accepted(reply: Reply): Either[String, RunAccepted] =
    structured(reply.result).flatMap(_.as[RunAccepted])

  private def asStatus(reply: Reply): Either[String, StatusView] =
    structured(reply.result).flatMap(_.as[StatusView])

  private def log(reply: Reply): Either[String, Log] =
    structured(reply.result).flatMap(_.as[Log])

  private def foundOf(reply: Reply): Either[String, Found] =
    structured(reply.result).flatMap(_.as[Found])

  private def calledOf(reply: Reply): Either[String, Called] =
    structured(reply.result).flatMap(_.as[Called])

  private def failure(reply: Reply): Either[String, JobFailure] =
    structured(reply.result).flatMap(_.get("value") match
      case Some(json) => json.as[JobFailure]
      case None       => Left("missing value"))

  private def structured(body: Json.Obj): Either[String, Json.Obj] =
    body.get("structuredContent") match
      case Some(obj: Json.Obj) => Right(obj)
      case _                   => Left("missing structuredContent")

  final case class Reply(result: Json.Obj)

  final class Host(pipe: LinePipe, ids: Ref[Long]):
    def list: ZIO[Any, Fail, Chunk[String]] =
      ids.updateAndGet(_ + 1).flatMap { n =>
        val id = RequestId.Num(n)
        write(Message.stateless(id, ClientRequest.ListTools(None))) *>
          untilResult(id, 4).map(reply => toolNames(reply.result))
      }

    def open: ZIO[Any, Fail, Opened] =
      call("open", openAll).flatMap { reply =>
        structured(reply.result).flatMap(_.as[Opened]) match
          case Right(opened) => ZIO.succeed(opened)
          case Left(detail)  => ZIO.fail(Fail.Decode(detail))
      }

    def call[A: JsonEncoder](name: String, args: A): ZIO[Any, Fail, Reply] =
      begin(name, args).flatMap(untilResult(_, 8))

    def untilEnded(session: String): ZIO[Any, Fail, StatusView] =
      call("status", SessionJob(session)).flatMap { reply =>
        ZIO.fromEither(asStatus(reply)).mapError(Fail.Decode(_)).flatMap {
          case status if terminal(status) => ZIO.succeed(status)
          case _                          => untilEnded(session)
        }
      }

    def begin[A: JsonEncoder](name: String, args: A): ZIO[Any, Fail, RequestId] =
      for
        encoded <- ZIO.fromEither(asObj(args)).mapError(_ => Fail.Undecodable)
        tool    <- ZIO.fromEither(ToolName.from(name)).mapError(_ => Fail.BadTool(name))
        n       <- ids.updateAndGet(_ + 1)
        id = RequestId.Num(n)
        _ <- write(Message.stateless(id, ClientRequest.CallTool(tool, encoded)))
      yield id

    def cancel(id: RequestId): ZIO[Any, Fail, Unit] =
      write(Cancelled(id, Some("stop")).message)

    def untilResult(id: RequestId, left: Int): ZIO[Any, Fail, Reply] =
      if left == 0 then ZIO.fail(Fail.TooMuch)
      else
        read.flatMap {
          case _: Message.Notification                => untilResult(id, left - 1)
          case Message.Result(got, body) if got == id => ZIO.succeed(Reply(body))
          case other                                  => ZIO.fail(Fail.Unexpected(other))
        }

    private def write(message: Message): ZIO[Any, Fail, Unit] =
      pipe.writeLine(message.json.toJson).mapError(Fail.Failed(_))

    private def read: ZIO[Any, Fail, Message] =
      pipe.readLine.mapError(Fail.Failed(_)).flatMap {
        case None       => ZIO.fail(Fail.Closed)
        case Some(line) =>
          line.fromJson[Json] match
            case Left(_)     => ZIO.fail(Fail.Undecodable)
            case Right(json) =>
              Message.decode(json) match
                case Left(err)  => ZIO.fail(Fail.Unexpected(err))
                case Right(msg) => ZIO.succeed(msg)
      }
  end Host

  object Host:
    def make(pipe: LinePipe): ZIO[Any, Nothing, Host] =
      Ref.make(0L).map(ids => new Host(pipe, ids))

  private def terminal(status: StatusView): Boolean =
    status.state match
      case WireState.Ended(_)                 => true
      case WireState.Open | WireState.Running => false

  private def toolNames(body: Json.Obj): Chunk[String] =
    body.get("tools") match
      case Some(Json.Arr(tools)) =>
        tools.flatMap {
          case obj: Json.Obj =>
            obj.get("name") match
              case Some(Json.Str(name)) => Chunk(name)
              case _                    => Chunk.empty
          case _ => Chunk.empty
        }
      case _ => Chunk.empty

  enum Fail:
    case Build(error: TerretMcpError)
    case Closed
    case TooMuch
    case Unexpected(message: Message)
    case Undecodable
    case Decode(detail: String)
    case BadTool(name: String)
    case Http(response: Response)
    case Failed(error: Throwable)
end McpJobsSpec
