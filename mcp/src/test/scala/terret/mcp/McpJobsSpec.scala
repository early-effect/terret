package terret.mcp

import heddle.{Body, LinePipe, Request, Response, Status}
import heddle.mcp.Mcp
import heddle.mcp.protocol.{Cancelled, ClientRequest, Message, Methods, ProtocolVersion, RequestId, ToolName}
import heddle.mcp.transport.Http
import heddle.sse.{ServerSentEvent, SseCodec}
import terret.config.TerretConfig
import terret.event.{Event, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.permission.{Permission, PermissionPolicy}
import terret.workspace.Workspace
import terret.Terret
import zio.json.*
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.durationInt
import zio.{Chunk, Promise, Ref, Scope, UIO, ZIO}

object McpJobsSpec extends ZIOSpecDefault:
  private val jobs = Set("open", "run", "status", "resume", "export", "cancel")

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
            log(exported) == Right(Log(opened.session, Chunk(WireEvent.Assistant("hello"), finished))),
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
              Log(opened.session, Chunk(WireEvent.Assistant("stay"), finished))
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
  ) @@ TestAspect.timeout(20.seconds) @@ TestAspect.withLiveClock

  private val openAll: OpenJob =
    OpenJob("/work", Chunk.fromIterable(Permission.values))

  private val finished: WireEvent = WireEvent.TurnFinished(WireOutcome.Completed)

  /** Yields between reads. `repeatUntil` on the live clock does not, so it starves the forked turn. */
  private def untilCount(started: Ref[Int], n: Int): UIO[Unit] =
    started.get.flatMap {
      case got if got == n => ZIO.unit
      case _               => ZIO.yieldNow *> untilCount(started, n)
    }

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
  ): ZIO[Scope, Fail, (InMemory, Mcp[Any])] =
    for
      memory <- InMemory.make(script)
      terret <- Terret.make(memory, config, Hooks.identity)
      mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(Fail.Build(_))
    yield (memory, mcp)

  private def hosted(script: Script, config: TerretConfig = TerretConfig.closed)(
      use: (InMemory, Host) => ZIO[Any, Fail, TestResult]
  ): ZIO[Any, Fail, TestResult] =
    ZIO.scoped {
      for
        memory <- InMemory.make(script)
        terret <- Terret.make(memory, config, Hooks.identity)
        mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(Fail.Build(_))
        pipes  <- LinePipe.connected
        (client, remote) = pipes
        fiber  <- mcp.stdio(remote).forkScoped
        host   <- Host.make(client)
        result <- use(memory, host).ensuring(client.close *> fiber.interrupt.ignore)
      yield result
    }

  private def sessionId(mcp: Mcp[Any]): ZIO[Any, Fail, String] =
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
      mcp: Mcp[Any],
      name: String,
      args: A,
      id: Long,
      sse: Boolean,
  ): ZIO[Any, Fail, Response] =
    for
      encoded <- ZIO.fromEither(asObj(args)).mapError(_ => Fail.Undecodable)
      tool    <- ZIO.fromEither(ToolName.from(name)).mapError(_ => Fail.BadTool(name))
      res     <- mcp.routes(callRequest(tool, encoded, id, sse))
    yield res

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
    case Failed(error: Throwable)
end McpJobsSpec
