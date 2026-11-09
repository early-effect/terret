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
import zio.{Chunk, Promise, Ref, ZIO}

object McpJobsSpec extends ZIOSpecDefault:
  private val jobs = Set("open", "run", "resume", "export", "cancel")

  def spec = suite("MCP jobs")(
    test("an empty server version is refused") {
      for
        memory <- InMemory.make(Script.echo)
        terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
        failed = TerretMcp.mcp(terret, "")
      yield assertTrue(failed == Left(TerretMcpError.EmptyVersion))
    },
    test("the pipe lists the jobs, publishes the turn, and export matches resume") {
      for
        calls <- Ref.make(0)
        out   <- hosted(counting(calls)) { (_, host) =>
                   for
                     listed   <- host.list
                     opened   <- host.open
                     ran      <- host.call("run", RunJob(opened.session, "hello"))
                     exported <- host.call("export", SessionJob(opened.session))
                     resumed  <- host.call("resume", SessionJob(opened.session))
                     prompts  <- calls.get
                   yield assertTrue(
                     listed.toSet == jobs,
                     notes(ran) == Right(Chunk(WireEvent.Assistant("hello"), finished)),
                     done(ran) == Right(RunDone(opened.session, WireOutcome.Completed)),
                     log(exported) == Right(Log(opened.session, Chunk(WireEvent.Assistant("hello"), finished))),
                     log(exported) == log(resumed),
                     prompts == 1,
                   )
                 }
      yield out
    },
    test("a second run is busy until notifications/cancelled, then another run starts") {
      for
        started <- Ref.make(0)
        hold    <- Promise.make[Nothing, Unit]
        seen    <- Promise.make[Nothing, Unit]
        out     <- hosted(holding(started, seen, hold)) { (memory, host) =>
                     for
                       opened <- host.open
                       id     <- host.begin("run", RunJob(opened.session, "stay"))
                       _      <- seen.await
                       busy   <- host.call("run", RunJob(opened.session, "no"))
                       _      <- host.cancel(id)
                       _      <- memory.cancelled.repeatUntil(_.nonEmpty)
                       again  <- host.begin("run", RunJob(opened.session, "next"))
                       _      <- started.get.repeatUntil(_ == 2)
                       _      <- hold.succeed(())
                       second <- host.untilResult(again, 8)
                       stopped <- memory.cancelled
                     yield assertTrue(
                       failure(busy) == Right(JobFailure.Busy(opened.session)),
                       done(second) == Right(RunDone(opened.session, WireOutcome.Completed)),
                       stopped.length == 1,
                     )
                   }
      yield out
    },
    test("routes publish each event on an event stream") {
      for
        (_, mcp) <- server(Script.echo)
        id       <- sessionId(mcp)
        streamed <- invoke(mcp, "run", RunJob(id, "hello"), 2, sse = true)
        events   <- SseCodec.stream(streamed.body.toStream).runCollect.mapError(Fail.Failed(_))
        messages = decoded(events)
      yield assertTrue(
        streamed.status == Status.Ok,
        streamed.headers.contentType.exists(_.isEventStream),
        notesOf(messages) == Right(Chunk(WireEvent.Assistant("hello"), finished)),
        messages.collect { case result: Message.Result => result }.length == 1,
      )
    },
    test("routes return the outcome as JSON when the client does not ask for an event stream") {
      for
        (_, mcp) <- server(Script.echo)
        id       <- sessionId(mcp)
        plain    <- invoke(mcp, "run", RunJob(id, "hello"), 2, sse = false)
        text     <- plain.body.utf8.mapError(Fail.Failed(_))
        message  = text.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption)
      yield assertTrue(
        plain.status == Status.Ok,
        plain.headers.contentType.exists(_.isJson),
        !text.contains(Notice.Method),
        message.collect { case Message.Result(_, body) => done(Reply(Chunk.empty, body)) }.contains(
          Right(RunDone(id, WireOutcome.Completed))
        ),
      )
    },
    test("routes turn notifications/cancelled into a harness cancel") {
      for
        started <- Ref.make(0)
        hold    <- Promise.make[Nothing, Unit]
        seen    <- Promise.make[Nothing, Unit]
        (memory, mcp) <- server(holding(started, seen, hold))
        id            <- sessionId(mcp)
        live   <- invoke(mcp, "run", RunJob(id, "stay"), 2, sse = true)
        events <- SseCodec.stream(live.body.toStream).runCollect.mapError(Fail.Failed(_)).fork
        _      <- seen.await
        stop   <- mcp.routes(cancelRequest(2))
        _      <- memory.cancelled.repeatUntil(_.nonEmpty)
        got    <- events.join
        halted <- memory.cancelled
      yield assertTrue(
        stop.status == Status.Accepted,
        halted.length == 1,
        notesOf(decoded(got)) == Right(Chunk(WireEvent.Assistant("stay"))),
        !decoded(got).exists {
          case Message.Result(RequestId.Num(2), _) => true
          case _                                   => false
        },
      )
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
        out <- hosted(Script.echo, TerretConfig(PermissionPolicy.Closed, Some(root))) { (_, host) =>
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
        started.updateAndGet(_ + 1).as(
          ZStream(Event.Assistant(prompt.text)) ++
            ZStream.fromZIO(seen.succeed(()) *> hold.await).drain ++
            ZStream(Event.TurnFinished(TurnOutcome.Completed))
        )
      )
    )

  private def server(script: Script, config: TerretConfig = TerretConfig.closed): ZIO[Any, Fail, (InMemory, Mcp[Any])] =
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
        fiber <- mcp.stdio(remote).forkScoped
        host  <- Host.make(client)
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
      opened  <- structured(body).flatMap(_.as[Opened])
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

  private def cancelRequest(id: Long): Request =
    Request
      .post("/mcp", Body.json(Cancelled(RequestId.Num(id), Some("stop")).message.json.toJson))
      .withHeader(Http.ProtocolHeader, ProtocolVersion.Current.value)

  private def asObj[A: JsonEncoder](value: A): Either[String, Json.Obj] =
    value.toJsonAST.flatMap {
      case obj: Json.Obj => Right(obj)
      case other         => Left(other.toJson)
    }

  private def decoded(events: Chunk[ServerSentEvent]): Chunk[Message] =
    events.flatMap(event => event.data.fromJson[Json].toOption.flatMap(json => Message.decode(json).toOption))

  private def notesOf(messages: Chunk[Message]): Either[String, Chunk[WireEvent]] =
    messages.collect { case note: Message.Notification => note }.map(eventOf).foldLeft[Either[String, Chunk[WireEvent]]](
      Right(Chunk.empty)
    ) {
      case (Left(err), _)            => Left(err)
      case (_, Left(err))            => Left(err)
      case (Right(acc), Right(wire)) => Right(acc :+ wire)
    }

  private def notes(reply: Reply): Either[String, Chunk[WireEvent]] =
    reply.notes.map(eventOf).foldLeft[Either[String, Chunk[WireEvent]]](Right(Chunk.empty)) {
      case (Left(err), _)            => Left(err)
      case (_, Left(err))            => Left(err)
      case (Right(acc), Right(wire)) => Right(acc :+ wire)
    }

  private def eventOf(note: Message.Notification): Either[String, WireEvent] =
    note.params.get("data") match
      case Some(json) => json.as[WireEvent]
      case None       => Left("missing data")

  private def done(reply: Reply): Either[String, RunDone] =
    structured(reply.result).flatMap(_.as[RunDone])

  private def log(reply: Reply): Either[String, Log] =
    structured(reply.result).flatMap(_.as[Log])

  private def failure(reply: Reply): Either[String, JobFailure] =
    structured(reply.result).flatMap(_.get("value") match
      case Some(json) => json.as[JobFailure]
      case None       => Left("missing value")
    )

  private def structured(body: Json.Obj): Either[String, Json.Obj] =
    body.get("structuredContent") match
      case Some(obj: Json.Obj) => Right(obj)
      case _                   => Left("missing structuredContent")

  final case class Reply(notes: Chunk[Message.Notification], result: Json.Obj)

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

    def untilResult(id: RequestId, left: Int, notes: Chunk[Message.Notification] = Chunk.empty): ZIO[Any, Fail, Reply] =
      if left == 0 then ZIO.fail(Fail.TooMuch)
      else
        read.flatMap {
          case note: Message.Notification     => untilResult(id, left - 1, notes :+ note)
          case Message.Result(got, body) if got == id => ZIO.succeed(Reply(notes, body))
          case other                          => ZIO.fail(Fail.Unexpected(other))
        }

    private def write(message: Message): ZIO[Any, Fail, Unit] =
      pipe.writeLine(message.json.toJson).mapError(Fail.Failed(_))

    private def read: ZIO[Any, Fail, Message] =
      pipe.readLine.mapError(Fail.Failed(_)).flatMap {
        case None => ZIO.fail(Fail.Closed)
        case Some(line) =>
          line.fromJson[Json] match
            case Left(_) => ZIO.fail(Fail.Undecodable)
            case Right(json) =>
              Message.decode(json) match
                case Left(err)  => ZIO.fail(Fail.Unexpected(err))
                case Right(msg) => ZIO.succeed(msg)
      }
  end Host

  object Host:
    def make(pipe: LinePipe): ZIO[Any, Nothing, Host] =
      Ref.make(0L).map(ids => new Host(pipe, ids))

  private def toolNames(body: Json.Obj): Chunk[String] =
    body.get("tools") match
      case Some(Json.Arr(tools)) =>
        tools.flatMap {
          case obj: Json.Obj =>
            obj.get("name") match
              case Some(Json.Str(name)) => Chunk(name)
              case _                     => Chunk.empty
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
