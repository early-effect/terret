package terret.grok

import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, IO, Ref, Scope, UIO, ZIO}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets

object CliHarnessSpec extends ZIOSpecDefault:
  private val ndjson =
    """{"type":"thought","data":"thinking"}
      |{"type":"text","data":"hi"}
      |{"type":"tool_call","toolCallId":"call_1","toolName":"read_file","kind":"read","rawInput":{"path":"a"}}
      |{"type":"end","stopReason":"end_turn"}
      |""".stripMargin

  private val call = ToolCall(CallId("call_1"), ToolName("read_file"), Json.Obj("path" -> Json.Str("a")))

  def spec = suite("Cli harness")(
    test("closed ask spawns once per prompt, resumes from the log, and the second prompt uses -r") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          harness <- GrokHarness.make(GrokConfig.cli, recording(ndjson, 0, calls), dead)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          streamed <- harness.prompt(id, Prompt("hello")).runCollect
          exported <- harness.transcript(id)
          replayed <- harness.resume(id).runCollect
          mid      <- calls.get
          _        <- harness.prompt(id, Prompt("again")).runDrain
          spawned  <- calls.get
          first = Launch.cli(GrokConfig.cli, id, Prompt("hello"), resume = false, PermissionPolicy.Closed, Ceiling.closed)
          second = Launch.cli(GrokConfig.cli, id, Prompt("again"), resume = true, PermissionPolicy.Closed, Ceiling.closed)
        yield assertTrue(
          streamed == Chunk(
            Event.Thought("thinking"),
            Event.Assistant("hi"),
            Event.ToolStarted(call),
            Event.TurnFinished(TurnOutcome.Completed),
          ),
          exported.events == streamed,
          replayed == streamed,
          mid.length == 1,
          mid.lift(0).exists(call => call.argv == first.rendered && call.cwd == root),
          spawned.lift(1).exists(_.argv == second.rendered),
        )
      }
    },
    test("a non-zero exit with no end line is Exited") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          harness <- GrokHarness.make(GrokConfig.cli, recording("""{"type":"text","data":"partial"}""", 3, calls), dead)
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
    test("a zero exit with no end line is incomplete") {
      ZIO.scoped {
        for
          calls   <- Ref.make(Chunk.empty[SpawnCall])
          harness <- GrokHarness.make(GrokConfig.cli, recording("", 0, calls), dead)
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
          harness <- GrokHarness.make(GrokConfig.cli, recording("not-json", 0, calls), dead)
          root    <- workspace
          id      <- harness.open(root, Ceiling.closed, PermissionPolicy.Closed)
          failed  <- harness.prompt(id, Prompt("hello")).runCollect.flip
        yield assertTrue(failed == HarnessFailure.Protocol(ProtocolFault.Malformed))
      }
    },
  )

  private def workspace: IO[Nothing, Workspace] =
    ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))

  private val dead: AcpOpen = new:
    def open(workspace: Workspace, policy: PermissionPolicy, ceiling: Ceiling): ZIO[Scope, HarnessFailure, RpcPipe] =
      ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))

  private def recording(body: String, code: Int, calls: Ref[Chunk[SpawnCall]]): Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace): IO[HarnessFailure, Spawned] =
      calls.update(_ :+ SpawnCall(command +: args, cwd)) *> ZIO.succeed(scripted(body, code))

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
end CliHarnessSpec
