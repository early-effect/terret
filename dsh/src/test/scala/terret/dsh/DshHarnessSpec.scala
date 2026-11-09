package terret.dsh

import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, IO, Ref, UIO, ZIO}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets

object DshHarnessSpec extends ZIOSpecDefault:
  private val completed =
    """{"type":"session","sessionId":"session-1","cwd":"/work"}
      |{"type":"status","phase":"turn_start","turn":1}
      |{"type":"thinking","text":"thinking"}
      |{"type":"text","text":"hi"}
      |{"type":"tool_call","callId":"call_1","tool":"read_file","input":{"path":"a"}}
      |{"type":"tool_result","callId":"call_1","status":"error","result":"missing"}
      |{"type":"tool_result","callId":"missing","status":"completed","result":"nope"}
      |{"type":"made_up"}
      |{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"aborted","reason":{"kind":"user"}}}
      |{"type":"status","phase":"turn_end","turn":2,"reason":{"kind":"completed"}}
      |{"type":"final","text":"hi"}
      |""".stripMargin

  private val resumed =
    """{"type":"session","sessionId":"session-1","cwd":"/work"}
      |{"type":"text","text":"next"}
      |{"type":"status","phase":"turn_end","turn":3,"reason":{"kind":"completed"}}
      |{"type":"final","text":"next"}
      |""".stripMargin

  private val call = ToolCall(
    CallId("call_1"),
    ToolName("read_file"),
    Json.Obj("path" -> Json.Str("a")),
  )
  private val updated = ToolCall(
    CallId("call_1"),
    ToolName("read_file"),
    Json.Obj("status" -> Json.Str("error"), "result" -> Json.Str("missing")),
  )

  def spec = suite("Dsh harness")(
    test("the first prompt omits --session-id and the next prompt adopts the reported id") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk((completed, 1), (resumed, 1)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          kept    <- harness.applied(id)
          streamed <- harness.prompt(id, Prompt("hello")).runCollect
          exported <- harness.transcript(id)
          replayed <- harness.resume(id).runCollect
          mid      <- calls.get
          again    <- harness.prompt(id, Prompt("again")).runCollect
          spawned  <- calls.get
          first = Launch.argv(DshConfig.headless, Prompt("hello"), None)
          second = Launch.argv(DshConfig.headless, Prompt("again"), Some("session-1"))
        yield assertTrue(
          kept == (Ceiling.closed, PermissionPolicy.Closed),
          streamed == Chunk(
            Event.Thought("thinking"),
            Event.Assistant("hi"),
            Event.ToolStarted(call),
            Event.ToolUpdated(updated),
            Event.TurnFinished(TurnOutcome.Completed),
          ),
          exported.events == streamed,
          replayed == streamed,
          mid.length == 1,
          mid.lift(0).exists(call => call.argv == first.rendered && call.cwd == root),
          again == Chunk(Event.Assistant("next"), Event.TurnFinished(TurnOutcome.Completed)),
          spawned.lift(1).exists(_.argv == second.rendered),
        )
      }
    },
    test("a turn_end decides the turn, so a non-zero exit is not a second failure") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          body = """{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"completed"}}"""
          spawner <- recording(Chunk((body, 1)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events  <- harness.prompt(id, Prompt("hello")).runCollect
        yield assertTrue(events == Chunk(Event.TurnFinished(TurnOutcome.Completed)))
      }
    },
    test("an error event fails the turn and ignores the exit code and the message") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk(("""{"type":"error","message":"boom"}""", 1)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events  <- harness.prompt(id, Prompt("hello")).runCollect
        yield assertTrue(
          events == Chunk(
            Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete))),
          ),
        )
      }
    },
    test("an aborted turn is cancelled") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          body =
            """{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"aborted","reason":{"kind":"user"}}}
              |{"type":"final","text":""}
              |""".stripMargin
          spawner <- recording(Chunk((body, 1)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events  <- harness.prompt(id, Prompt("hello")).runCollect
        yield assertTrue(events == Chunk(Event.TurnFinished(TurnOutcome.Cancelled)))
      }
    },
    test("a non-zero exit with no turn_end and no error event is Exited") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk(("""{"type":"text","text":"partial"}""", 3)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          events  <- harness.prompt(id, Prompt("hello")).either.runCollect
        yield assertTrue(
          events.exists {
            case Right(Event.Assistant("partial")) => true
            case _                                 => false
          },
          events.exists {
            case Left(HarnessFailure.Exited(3)) => true
            case _                              => false
          },
        )
      }
    },
    test("a zero exit with no turn_end is incomplete") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk(("", 0)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          failed  <- harness.prompt(id, Prompt("hello")).runCollect.flip
        yield assertTrue(failed == HarnessFailure.Protocol(ProtocolFault.Incomplete))
      }
    },
    test("a non-json line is malformed") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk(("not-json", 0)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          failed  <- harness.prompt(id, Prompt("hello")).runCollect.flip
        yield assertTrue(failed == HarnessFailure.Protocol(ProtocolFault.Malformed))
      }
    },
    test("a reported id that is not the resumed id is malformed") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          first = """{"type":"session","sessionId":"session-1","cwd":"/work"}
                    |{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"completed"}}
                    |""".stripMargin
          second = """{"type":"session","sessionId":"session-2","cwd":"/work"}"""
          spawner <- recording(Chunk((first, 0), (second, 0)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          _       <- harness.prompt(id, Prompt("hello")).runDrain
          failed  <- harness.prompt(id, Prompt("again")).runCollect.flip
          spawned <- calls.get
        yield assertTrue(
          failed == HarnessFailure.Protocol(ProtocolFault.Malformed),
          spawned.lift(1).exists(_.argv == Launch.argv(DshConfig.headless, Prompt("again"), Some("session-1")).rendered),
        )
      }
    },
    test("a prompt of one dash is rejected before spawn") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk.empty, calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          failed  <- harness.prompt(id, Prompt("-")).runCollect.flip
          spawned <- calls.get
        yield assertTrue(failed == HarnessFailure.Protocol(ProtocolFault.Malformed), spawned.isEmpty)
      }
    },
    test("resume does not spawn when the run never named a session") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          body = """{"type":"text","text":"hi"}
                   |{"type":"status","phase":"turn_end","turn":1,"reason":{"kind":"completed"}}
                   |""".stripMargin
          spawner <- recording(Chunk((body, 0), (body, 0)), calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          _       <- harness.prompt(id, Prompt("hello")).runDrain
          replayed <- harness.resume(id).runCollect
          _        <- harness.prompt(id, Prompt("again")).runDrain
          spawned  <- calls.get
          again = Launch.argv(DshConfig.headless, Prompt("again"), None)
        yield assertTrue(replayed.length == 2, spawned.lift(1).exists(_.argv == again.rendered))
      }
    },
    test("unknown sessions fail and an open session can be cancelled") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          spawner <- recording(Chunk.empty, calls)
          harness <- DshHarness.make(DshConfig.headless, spawner)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          missing <- ZIO.fromEither(SessionId.from("missing")).orDieWith(_ => new IllegalArgumentException("session"))
          unknown <- harness.transcript(missing).flip
          cancelled <- harness.cancel(id)
        yield assertTrue(unknown == HarnessFailure.UnknownSession(missing), cancelled == ())
      }
    },
  )

  private def workspace: IO[Nothing, Workspace] =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))

  private def recording(scripts: Chunk[(String, Int)], calls: Ref[Chunk[SpawnCall]]): UIO[Spawner] =
    Ref.make(0).map { index =>
      new:
        def spawn(command: String, args: Chunk[String], cwd: Workspace): IO[HarnessFailure, Spawned] =
          index.modify(i => (i, i + 1)).flatMap { i =>
            val (body, code) = scripts.lift(i).getOrElse(("", 1))
            calls.update(_ :+ SpawnCall(command +: args, cwd)) *> ZIO.succeed(scripted(body, code))
          }
    }

  private def scripted(body: String, code: Int): Spawned =
    val out = ByteArrayInputStream(body.getBytes(StandardCharsets.UTF_8))
    val err = ByteArrayInputStream(Array.emptyByteArray)
    val in  = ByteArrayOutputStream()
    new:
      def stdin: OutputStream           = in
      def stdout: InputStream           = out
      def stderr: InputStream           = err
      def exit: IO[HarnessFailure, Int] = ZIO.succeed(code)
      def destroy: UIO[Unit]            = ZIO.unit

  private final case class SpawnCall(argv: Chunk[String], cwd: Workspace)
end DshHarnessSpec
