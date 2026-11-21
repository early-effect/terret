package terret.grok

import terret.event.{Event, ToolCall, TurnOutcome}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.workspace.Workspace
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, ZIO}

import java.io.{FileOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.{Comparator, UUID}

object LiveGrokSpec extends ZIOSpecDefault:
  private lazy val scratch = repoRoot(Path.of("").toAbsolutePath).resolve(Path.of("target", "live-grok"))
  private lazy val wireLog = scratch.resolve("wire.log")
  private lazy val outside = scratch.resolve("outside")
  private val granted      = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
  private val shell        = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
  private val write        = "echo hi | tee out.txt"
  private val quiet        = "Reply with the single word pong. Do not use tools."
  private val fetch        =
    "Fetch https://example.com with the web_fetch tool. If web_fetch is not offered, call it through use_tool with tool_name web_fetch. Do not answer until that call has been made."
  private val search =
    "Search for terret with the web_search tool. If web_search is not offered, call it through use_tool with tool_name web_search. Do not answer until that call has been made."
  private val ticking =
    "for i in 1 2 3 4; do head -c 20000 /dev/zero | tr '\\0' 'x'; printf '\\n'; sleep 1; done"

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
            out = root.path.resolve("out.txt")
            exists <- ZIO.attemptBlocking(Files.exists(out))
            text   <- ZIO.attemptBlocking(if exists then Files.readString(out) else "")
          yield assertTrue(
            finished(events, TurnOutcome.Completed),
            !finished(events, TurnOutcome.Cancelled),
            exists,
            text.contains("hi"),
          )
        }
      } @@ TestAspect.timeout(3.minutes),
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
      test("a refused outside write names the path and the turn completes") {
        ZIO.scoped {
          for
            probe            <- fresh(outside.resolve("tee.txt"))
            (_, harness, id) <- session(granted, PermissionPolicy.Clamp(granted))
            events           <- harness.prompt(id, Prompt(s"echo hi | tee $probe")).runCollect
            followed         <- harness.prompt(id, Prompt(quiet)).runCollect
            exported         <- harness.transcript(id)
            exists           <- ZIO.attemptBlocking(Files.exists(probe))
          yield assertTrue(
            !exists,
            endings(events) == Chunk(TurnOutcome.Completed),
            names(events, "outside the workspace") || names(exported.events, "outside the workspace"),
            endings(followed) == Chunk(TurnOutcome.Completed),
          )
        }
      } @@ TestAspect.timeout(4.minutes),
      test("a running command reports only new output") {
        ZIO.scoped {
          for
            (_, harness, id) <- session(shell, PermissionPolicy.Clamp(shell))
            events           <- harness.prompt(id, Prompt(ticking)).runCollect
            exported         <- harness.transcript(id)
          yield assertTrue(
            endings(events) == Chunk(TurnOutcome.Completed),
            lengths(events).exists(_.size >= 2) || lengths(exported.events).exists(_.size >= 2),
            !outputRuns(events).exists(replays),
            !outputRuns(exported.events).exists(replays),
          )
        }
      } @@ TestAspect.timeout(4.minutes),
    ).when(LiveGate.current == LiveGate.Decision.Ready)
    gated @@ TestAspect.beforeAll(prepare) @@ TestAspect.sequential @@ TestAspect.withLiveClock

  private[grok] val ungrantedWrite =
    test("an ungranted write names the missing grant and the session continues") {
      ZIO.scoped {
        for
          (root, harness, id) <- session(shell, PermissionPolicy.Clamp(shell))
          before              <- wireSize
          events              <- harness.prompt(id, Prompt(write)).runCollect
          permissionRequested <- askedSince(before)
          followed            <- harness.prompt(id, Prompt(quiet)).runCollect
          exported            <- harness.transcript(id)
          out = root.path.resolve("out.txt")
          exists <- ZIO.attemptBlocking(Files.exists(out))
        yield assertTrue(
          permissionRequested,
          !exists,
          endings(events) == Chunk(TurnOutcome.Completed),
          names(events, "WriteWorkspace") || names(exported.events, "WriteWorkspace"),
          endings(followed) == Chunk(TurnOutcome.Completed),
        )
      }
    } @@ TestAspect.timeout(4.minutes)

  private[grok] val heredocOutside =
    test("a heredoc write outside the workspace is refused and the turn completes") {
      ZIO.scoped {
        for
          dir                 <- freshDir(outside.resolve("heredoc"))
          (_, harness, id)    <- session(granted, PermissionPolicy.Clamp(granted))
          before              <- wireSize
          events              <- harness.prompt(id, Prompt(heredoc(dir))).runCollect
          permissionRequested <- askedSince(before)
          followed            <- harness.prompt(id, Prompt(quiet)).runCollect
          exported            <- harness.transcript(id)
          written = dir.resolve("Probe.java")
          exists <- ZIO.attemptBlocking(Files.exists(written))
        yield assertTrue(
          permissionRequested,
          !exists,
          started(events),
          endings(events) == Chunk(TurnOutcome.Completed),
          names(events, "outside the workspace") || names(exported.events, "outside the workspace"),
          endings(followed) == Chunk(TurnOutcome.Completed),
        )
      }
    } @@ TestAspect.timeout(4.minutes)

  private[grok] val refusedFetch =
    test("a refused fetch without Network names the clamp and the turn completes") {
      ZIO.scoped {
        for
          (_, harness, id)    <- session(granted, PermissionPolicy.Clamp(granted))
          before              <- wireSize
          events              <- harness.prompt(id, Prompt(fetch)).runCollect
          permissionRequested <- askedSince(before)
          followed            <- harness.prompt(id, Prompt(quiet)).runCollect
          exported            <- harness.transcript(id)
        yield assertTrue(
          permissionRequested,
          endings(events) == Chunk(TurnOutcome.Completed),
          names(events, "Network") || names(exported.events, "Network"),
          names(events, "clamped by server policy") || names(exported.events, "clamped by server policy"),
          endings(followed) == Chunk(TurnOutcome.Completed),
        )
      }
    } @@ TestAspect.timeout(4.minutes)

  private[grok] val refusedSearch =
    test("a session without Network refuses hosted search and the turn completes") {
      ZIO.scoped {
        for
          (_, harness, id)    <- session(granted, PermissionPolicy.Clamp(granted))
          before              <- wireSize
          events              <- harness.prompt(id, Prompt(search)).runCollect
          permissionRequested <- askedSince(before)
          exported            <- harness.transcript(id)
        yield assertTrue(
          permissionRequested,
          endings(events) == Chunk(TurnOutcome.Completed),
          names(events, "Network") || names(exported.events, "Network"),
          names(events, "clamped by server policy") || names(exported.events, "clamped by server policy"),
          !deliveredSearch(events),
          !deliveredSearch(exported.events),
        )
      }
    } @@ TestAspect.timeout(4.minutes)

  private[grok] def prepare: ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(outside)
        Files.deleteIfExists(wireLog)
      }
      .orDie
      .unit

  private def session(ceiling: Ceiling, policy: PermissionPolicy) =
    for
      dir  <- ZIO.acquireRelease(freshRepo)(deleteTree)
      root <- ZIO
        .fromEither(Workspace.from(dir.toString))
        .orDieWith(_ => new IllegalArgumentException("workspace"))
      log <- ZIO.acquireRelease(ZIO.attemptBlocking(new FileOutputStream(wireLog.toFile, true)).orDie)(closeLog)
      spawner = tee(Spawner.jdk, log)
      harness <- GrokHarness.make(GrokConfig.stdio, spawner, AcpOpen.live(GrokConfig.stdio, spawner))
      id      <- harness.open(root, ceiling, policy)
    yield (root, harness, id)

  // Grok keys the permission store on the git root, so each case is its own repository.
  private def freshRepo: ZIO[Any, Nothing, Path] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(scratch)
        scratch.resolve(UUID.randomUUID.toString)
      }
      .orDie
      .flatMap { dir =>
        ZIO
          .attemptBlocking {
            Files.createDirectories(dir)
            val process = new ProcessBuilder("git", "init", "-q", dir.toString)
              .redirectOutput(ProcessBuilder.Redirect.DISCARD)
              .redirectError(ProcessBuilder.Redirect.DISCARD)
              .start()
            process.waitFor()
          }
          .orDie
          .flatMap { code =>
            if code != 0 then ZIO.dieMessage(s"git init exited $code for $dir")
            else guardStore(dir)
          }
      }

  private def guardStore(dir: Path): ZIO[Any, Nothing, Path] =
    grokHome match
      case None       => ZIO.dieMessage("user.home is unset, so the permission store cannot be checked")
      case Some(home) =>
        val caseKey    = encodeCwd(dir.toAbsolutePath.normalize.toString)
        val projectKey = encodeCwd(repoRoot(dir).toAbsolutePath.normalize.toString)
        (caseKey, projectKey) match
          case (Left(reason), _)                           => ZIO.dieMessage(reason)
          case (_, Left(reason))                           => ZIO.dieMessage(reason)
          case (Right(caseEncoded), Right(projectEncoded)) =>
            val file    = home.resolve(".grok").resolve("sessions").resolve(caseEncoded).resolve("permission.toml")
            val project =
              home.resolve(".grok").resolve("sessions").resolve(projectEncoded).resolve("permission.toml")
            if file == project then
              ZIO.dieMessage(
                s"prior grant would short-circuit the prompt: $dir uses the project permission store $file"
              )
            else
              ZIO.attemptBlocking(Files.exists(file)).orDie.flatMap { exists =>
                if exists then ZIO.dieMessage(s"prior grant would short-circuit the prompt: $file")
                else ZIO.succeed(dir)
              }

  private def grokHome: Option[Path] =
    Option(System.getProperty("user.home")).map(_.trim).filter(_.nonEmpty).map(Path.of(_))

  private[grok] def encodeCwd(raw: String): Either[String, String] =
    val encoded = raw
      .getBytes(StandardCharsets.UTF_8)
      .map { byte =>
        val value = byte & 0xff
        if unreserved(value) then value.toChar.toString else percent(value)
      }
      .mkString
    if encoded.length > 255 then Left("permission store key exceeds 255 encoded bytes")
    else Right(encoded)

  private def unreserved(value: Int): Boolean =
    (value >= 'A' && value <= 'Z') ||
      (value >= 'a' && value <= 'z') ||
      (value >= '0' && value <= '9') ||
      value == '-' || value == '_' || value == '.' || value == '~'

  private def percent(value: Int): String =
    s"%${hex(value >>> 4)}${hex(value & 0xf)}"

  private def hex(nibble: Int): Char =
    val code = if nibble < 10 then '0' + nibble else 'A' + (nibble - 10)
    code.toChar

  private def wireSize: ZIO[Any, Nothing, Long] =
    ZIO.attemptBlocking(if Files.exists(wireLog) then Files.size(wireLog) else 0L).orDie

  private def askedSince(offset: Long): ZIO[Any, Nothing, Boolean] =
    ZIO.attemptBlocking {
      if !Files.exists(wireLog) then false
      else
        val bytes = Files.readAllBytes(wireLog)
        val start = math.max(0L, math.min(offset, bytes.length.toLong)).toInt
        new String(bytes, start, bytes.length - start, StandardCharsets.UTF_8)
          .contains("session/request_permission")
    }.orDie

  private def fresh(path: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path.getParent)
      Files.deleteIfExists(path)
      path
    }.orDie

  private def freshDir(path: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(path)
      val written = path.resolve("Probe.java")
      Files.deleteIfExists(written)
      path
    }.orDie

  private def heredoc(dir: Path): String =
    val file = dir.resolve("Probe.java")
    s"""mkdir -p $dir && cat > $file << 'EOF'
       |class Probe { String q = "x; }
       |EOF""".stripMargin

  private def finished(events: Chunk[Event], outcome: TurnOutcome): Boolean =
    endings(events).exists(_ == outcome)

  private def endings(events: Chunk[Event]): Chunk[TurnOutcome] =
    events.collect { case Event.TurnFinished(found) => found }

  private def started(events: Chunk[Event]): Boolean =
    events.exists {
      case Event.ToolStarted(_) => true
      case _                    => false
    }

  private def names(events: Chunk[Event], needle: String): Boolean =
    events.exists {
      case Event.ToolUpdated(call) => textOf(call.input).contains(needle)
      case Event.Assistant(text)   => text.contains(needle)
      case _                       => false
    }

  private def deliveredSearch(events: Chunk[Event]): Boolean =
    events.exists {
      case Event.ToolUpdated(call) => mentionsSearch(call) && resultBody(call.input).exists(delivered)
      case _                       => false
    }

  private def mentionsSearch(call: ToolCall): Boolean =
    val name = call.name.value.toLowerCase.replace("_", "")
    val raw  = call.input.toJson.toLowerCase.replace("_", "")
    name.contains("websearch") || raw.contains("websearch")

  private def delivered(text: String): Boolean =
    text.nonEmpty && !text.contains("needs Network") && !text.contains("clamped by server policy")

  private def resultBody(json: Json): Option[String] =
    json match
      case Json.Str(text) => Some(text)
      case obj: Json.Obj  =>
        textField(obj, "output").orElse(textField(obj, "stdout")).orElse(textField(obj, "stderr"))
      case _ => None

  private def lengths(events: Chunk[Event]): Chunk[Chunk[Int]] =
    outputRuns(events).map(_.map(_.length))

  private def outputRuns(events: Chunk[Event]): Chunk[Chunk[String]] =
    val runs = events.foldLeft(Map.empty[String, Chunk[String]]) {
      case (acc, Event.ToolUpdated(call)) =>
        val id   = call.id.value
        val text = outputText(call.input)
        acc.updated(id, acc.getOrElse(id, Chunk.empty) :+ text)
      case (acc, _) => acc
    }
    Chunk.fromIterable(runs.values)

  private def outputText(json: Json): String =
    json match
      case Json.Str(text) => text
      case obj: Json.Obj  =>
        textField(obj, "output")
          .orElse(textField(obj, "stdout"))
          .orElse(textField(obj, "stderr"))
          .orElse(textField(obj, "output_for_prompt"))
          .getOrElse(obj.toJson)
      case other => other.toJson

  private def textField(obj: Json.Obj, name: String): Option[String] =
    obj.fields.collectFirst { case (key, Json.Str(text)) if key == name => text }

  private def replays(texts: Chunk[String]): Boolean =
    texts.zip(texts.drop(1)).exists { (prev, next) =>
      prev.nonEmpty && next.length > prev.length && next.startsWith(prev)
    }

  private def textOf(json: Json): String =
    json match
      case Json.Str(text) => text
      case other          => other.toJson

  private def repoRoot(start: Path): Path =
    if Files.exists(start.resolve("histos.scala")) then start
    else
      Option(start.getParent) match
        case Some(parent) => repoRoot(parent)
        case None         => start

  private def deleteTree(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk = Files.walk(root)
        try
          val paths = walk.sorted(Comparator.reverseOrder).iterator()
          while paths.hasNext do Files.deleteIfExists(paths.next())
        finally walk.close()
    }.ignore

  private def closeLog(out: OutputStream): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(out.close()).ignore

  private def tee(inner: Spawner, log: OutputStream): Spawner = new:
    def spawn(command: String, args: Chunk[String], cwd: Workspace) =
      inner.spawn(command, args, cwd).map { spawned =>
        new Spawned:
          def stdin: OutputStream = spawned.stdin
          def stdout: InputStream = new TeeInput(spawned.stdout, log)
          def stderr: InputStream = spawned.stderr
          def exit                = spawned.exit
          def destroy             = spawned.destroy
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
  end TeeInput
end LiveGrokSpec
