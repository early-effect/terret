package terret.grok

import terret.event.{Event, TurnOutcome}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, ZIO}

import java.io.{FileOutputStream, InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.util.Comparator

object LiveGrokSpec extends ZIOSpecDefault:
  private val wireLog = Path.of("/tmp/terret-live-wire.log")
  private val granted = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
  private val shell   = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
  private val write = "echo hi | tee out.txt"
  private val quiet = "Reply with the single word pong. Do not use tools."

  def spec =
    val gated = suite("live grok")(
      test("open over ACP returns the agent session") {
        ZIO.scoped {
          session(Ceiling.closed, PermissionPolicy.Closed).map { (_, _, id) =>
            assertTrue(id.value.nonEmpty)
          }
        }
      } @@ TestAspect.timeout(2.minutes),
      test("a granted shell write finishes and the file exists") {
        ZIO.scoped {
          for
            (root, harness, id) <- session(granted, PermissionPolicy.Clamp(granted))
            events              <- harness.prompt(id, Prompt(write)).runCollect
            out                  = root.path.resolve("out.txt")
            exists              <- ZIO.attemptBlocking(Files.exists(out))
            text                <- ZIO.attemptBlocking(if exists then Files.readString(out) else "")
          yield assertTrue(
            finished(events, TurnOutcome.Completed),
            !finished(events, TurnOutcome.Cancelled),
            exists,
            text.contains("hi"),
          )
        }
      } @@ TestAspect.timeout(3.minutes),
      test("an ungranted write names the missing grant and the session continues") {
        ZIO.scoped {
          for
            (root, harness, id) <- session(shell, PermissionPolicy.Clamp(shell))
            events              <- harness.prompt(id, Prompt(write)).runCollect
            followed            <- harness.prompt(id, Prompt(quiet)).runCollect
            exported            <- harness.transcript(id)
            out                  = root.path.resolve("out.txt")
            exists              <- ZIO.attemptBlocking(Files.exists(out))
          yield assertTrue(
            !exists,
            !finished(events, TurnOutcome.Cancelled),
            namesGrant(events) || namesGrant(exported.events),
            finished(followed, TurnOutcome.Completed),
          )
        }
      } @@ TestAspect.timeout(4.minutes),
      test("cancel ends the turn as Cancelled") {
        ZIO.scoped {
          for
            (_, harness, id) <- session(shell, PermissionPolicy.Clamp(shell))
            fiber            <- harness
              .prompt(id, Prompt("sleep 60"))
              .runCollect
              .fork
            _      <- ZIO.sleep(1.second)
            _      <- harness.cancel(id)
            events <- fiber.join
          yield assertTrue(finished(events, TurnOutcome.Cancelled))
        }
      } @@ TestAspect.timeout(3.minutes),
      test("a read-only prompt completes") {
        ZIO.scoped {
          for
            (_, harness, id) <- session(Ceiling.closed, PermissionPolicy.Closed)
            events           <- harness.prompt(id, Prompt(quiet)).runCollect
          yield assertTrue(finished(events, TurnOutcome.Completed), !started(events))
        }
      } @@ TestAspect.timeout(3.minutes),
    ).when(LiveGate.current == LiveGate.Decision.Ready)
    gated @@ TestAspect.beforeAll(ZIO.attemptBlocking(Files.deleteIfExists(wireLog)).orDie) @@
      TestAspect.sequential @@ TestAspect.withLiveClock

  private def session(ceiling: Ceiling, policy: PermissionPolicy) =
    for
      dir <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("terret-live")).orDie)(deleteTree)
      root <- ZIO
        .fromEither(Workspace.from(dir.toString))
        .orDieWith(_ => new IllegalArgumentException("workspace"))
      log <- ZIO.acquireRelease(ZIO.attemptBlocking(new FileOutputStream(wireLog.toFile, true)).orDie)(closeLog)
      spawner = tee(Spawner.jdk, log)
      harness <- GrokHarness.make(GrokConfig.stdio, spawner, AcpOpen.live(GrokConfig.stdio, spawner))
      id      <- harness.open(root, ceiling, policy)
    yield (root, harness, id)

  private def finished(events: Chunk[Event], outcome: TurnOutcome): Boolean =
    events.exists {
      case Event.TurnFinished(found) => found == outcome
      case _                         => false
    }

  private def started(events: Chunk[Event]): Boolean =
    events.exists {
      case Event.ToolStarted(_) => true
      case _                    => false
    }

  private def namesGrant(events: Chunk[Event]): Boolean =
    events.exists {
      case Event.ToolUpdated(call) => textOf(call.input).contains("WriteWorkspace")
      case Event.Assistant(text)   => text.contains("WriteWorkspace")
      case _                       => false
    }

  private def textOf(json: Json): String =
    json match
      case Json.Str(text) => text
      case other          => other.toJson

  private def deleteTree(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk = Files.walk(root)
        try
          val paths = walk.sorted(Comparator.reverseOrder).iterator()
          while paths.hasNext do
            Files.deleteIfExists(paths.next())
        finally walk.close()
    }.ignore

  private def closeLog(out: OutputStream): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(out.close()).ignore

  private def tee(inner: Spawner, log: OutputStream): Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      inner.spawn(command, args, cwd).map { spawned =>
        new Spawned:
          def stdin: OutputStream                     = spawned.stdin
          def stdout: InputStream                     = new TeeInput(spawned.stdout, log)
          def stderr: InputStream                     = spawned.stderr
          def exit                                    = spawned.exit
          def destroy                                 = spawned.destroy
      }

  private final class TeeInput(inner: InputStream, log: OutputStream) extends InputStream:
    override def read(): Int =
      val byte = inner.read()
      if byte >= 0 then write(Array(byte.toByte), 0, 1)
      byte

    override def read(buffer: Array[Byte], offset: Int, length: Int): Int =
      val count = inner.read(buffer, offset, length)
      if count > 0 then write(buffer, offset, count)
      count

    override def close(): Unit = inner.close()

    private def write(buffer: Array[Byte], offset: Int, length: Int): Unit =
      log.synchronized {
        log.write(buffer, offset, length)
        log.flush()
      }
end LiveGrokSpec
