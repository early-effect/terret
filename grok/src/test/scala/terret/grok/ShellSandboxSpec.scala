package terret.grok

import slayer.*
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Ref, ZIO}

import java.nio.file.{Files, LinkOption, Path}
import java.util.UUID

object ShellSandboxSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  def spec = suite("shell sandbox")(
    test("a profile is deny-default, quotes paths, and opens the network only when granted") {
      val built =
        for
          nasty   <- AbsPath.from("""/work/(allow network*)""")
          work    <- AbsPath.from("/work")
          root    <- AbsPath.from("/")
          select  <- AbsPath.from("/var/select")
          priv    <- AbsPath.from("/private")
          privVar <- AbsPath.from("/private/var")
          link    <- AbsPath.from("/var")
          quoted  <- AbsPath.from("/work/a\"b\\c")
        yield
          val closed = SeatbeltProfile.render(SeatbeltPolicy(Chunk(nasty), Chunk.empty, Chunk(root), false))
          val opened = SeatbeltProfile.render(SeatbeltPolicy(Chunk(work), Chunk(work), Chunk(root), true))
          val reached = SeatbeltProfile.render(
            SeatbeltPolicy(Chunk(select), Chunk.empty, Chunk(priv, privVar), false, Chunk(link)),
          )
          assertTrue(
            closed.startsWith("(version 1)\n(deny default)\n"),
            closed.contains("(vnode-type 7)"),
            closed.contains("(allow file-read* (literal \"/\"))"),
            closed.contains("(allow file-map-executable)"),
            closed.contains("\"/work/(allow network*)\""),
            !closed.split('\n').contains("(allow network*)"),
            !closed.contains("(allow file-write* (subpath"),
            !closed.contains("(allow file-map-executable (subpath"),
            opened.split('\n').contains("(allow network*)"),
            opened.contains("(allow file-write* (subpath \"/work\"))"),
            reached.split('\n').contains("(allow file-read-data (literal \"/var\"))"),
            reached.split('\n').contains("(allow file-read-metadata (literal \"/private\"))"),
            reached.split('\n').contains("(allow file-read* (subpath \"/var/select\"))"),
            !reached.split('\n').contains("(allow file-read* (subpath \"/var\"))"),
            !reached.split('\n').contains("(allow file-read* (subpath \"/private\"))"),
            AbsPath.from("/work/\n(allow default)") == Left(AbsPathError.Illegal),
            AbsPath.from("work") == Left(AbsPathError.NotAbsolute),
            SeatbeltProfile.quote(quoted) == "\"/work/a\\\"b\\\\c\"",
          )
      built match
        case Right(result)                   => result
        case Left(AbsPathError.NotAbsolute)  => assertTrue(false)
        case Left(AbsPathError.Illegal)      => assertTrue(false)
    },
    test("the argument vector keeps the profile and the command as separate elements") {
      val profile = "(version 1)\n(deny default)\n"
      val command = Chunk("/bin/sh", "-c", "echo hi > /tmp/x")
      assertTrue(
        ShellSandbox.confined(profile, command) ==
          Chunk("/usr/bin/sandbox-exec", "-p", profile, "--", "/bin/sh", "-c", "echo hi > /tmp/x"),
        AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "printf x > \"$1\"")) ==
          Some(Chunk("/bin/sh", "-c", "printf x > \"$1\"")),
        AcpTerminal.execArgv("/bin/bash -lc 'printf %s \"a b\"'", Chunk.empty) ==
          Some(Chunk("/bin/sh", "-c", "printf %s \"a b\"")),
        AcpTerminal.execArgv("/bin/bash -lc 'a'\\''b'", Chunk.empty) == Some(Chunk("/bin/sh", "-c", "a'b")),
        AcpTerminal.execArgv("/bin/bash -lc echo hi", Chunk.empty).isEmpty,
        AcpTerminal.execArgv("/bin/bash -lc 'echo hi' extra", Chunk.empty).isEmpty,
        AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "ok\u0000")).isEmpty,
        AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "echo", "hi")).isEmpty,
        AcpTerminal.execArgv("/bin/bash", Chunk("--login", "-c", "printf x")).isEmpty,
        AcpTerminal.execArgv("/bin/bash", Chunk("--version")) == Some(Chunk("/bin/bash", "--version")),
        AcpTerminal.execArgv("/bin/bash", Chunk("-c", "printf x")) == Some(Chunk("/bin/bash", "-c", "printf x")),
        AcpTerminal.execArgv("/bin/cat", Chunk("/work/note")) == Some(Chunk("/bin/cat", "/work/note")),
        ShellWrite.effect(AcpTerminal.commandText("/bin/bash", Chunk("-lc", "echo hi > /tmp/x"))) ==
          ShellEffect.Writes(Chunk("/tmp/x")),
        ShellWrite.effect(AcpTerminal.commandText("/bin/bash -lc 'echo hi > /tmp/x'", Chunk.empty)) ==
          ShellEffect.Writes(Chunk("/tmp/x")),
        ShellSandbox.skipReason(false) == Some("sandbox-exec is missing"),
        ShellSandbox.skipReason(true).isEmpty,
        AcpTerminal.canon("x.ai/terminal/create").contains("terminal/create"),
        AcpTerminal.canon("_x.ai/terminal/kill").contains("terminal/kill"),
        AcpTerminal.canon("session/prompt").isEmpty,
      )
    },
    test("the profile request carries the ceiling's roots and network grant") {
      val version = shell("./histos --version")
      val run     = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
      val net     = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands, Permission.Network))
      val both    = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      (for
        seen    <- Ref.make(Chunk.empty[ShellBounds])
        _       <- stub[ShellSandbox](_.profile(any[ShellBounds])) { (bounds: ShellBounds) =>
          seen.update(_ :+ bounds).as("(version 1)\n(deny default)\n")
        }
        sandbox <- ZIO.service[ShellSandbox]
        root    <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
        _       <- ask(root, run, version, sandbox)
        _       <- ask(root, net, version, sandbox)
        _       <- ask(root, both, version, sandbox)
        got     <- seen.get
      yield
        val texts = got.map { bounds =>
          (bounds.reads.map(_.toString), bounds.writes.map(_.toString), bounds.network)
        }
        assertTrue(
          texts.lift(0) == Some((Chunk("/work"), Chunk.empty, false)),
          texts.lift(1) == Some((Chunk("/work"), Chunk.empty, true)),
          texts.lift(2) == Some((Chunk("/work"), Chunk("/work"), false)),
        )).provide(stubbed[ShellSandbox])
    },
    test("a profile failure refuses a shell the scan would allow and leaves a scan denial alone") {
      val version = shell("./histos --version")
      val outside = shell("echo hi > /tmp/terret-probe.log")
      val run     = Ceiling(Set(Permission.ReadWorkspace, Permission.RunCommands))
      val both    = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))
      (for
        _       <- stub[ShellSandbox](_.profile(any[ShellBounds]))(ZIO.fail(SandboxError.Unavailable))
        sandbox <- ZIO.service[ShellSandbox]
        root    <- ZIO.fromEither(Workspace.from("/work")).orDieWith(_ => new IllegalArgumentException("workspace"))
        denied  <- ask(root, run, version, sandbox)
        escaped <- ask(root, both, outside, sandbox)
      yield assertTrue(
        denied == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "run_terminal_command cannot be sandboxed on this host",
        ),
        escaped == PermissionAnswer.Refused(
          "reject-once",
          "call_1",
          "run_terminal_command writes /tmp/terret-probe.log, outside the workspace; grant it in terret.writeRoots",
        ),
      )).provide(stubbed[ShellSandbox])
    },
    test("a hidden write outside the workspace does not land") {
      ZIO.scoped {
        ZIO.acquireRelease(scratch)(dirs => deleteTree(dirs.root)).flatMap(prove)
      }
    } @@ TestAspect.timeout(30.seconds),
    suite("macos sandbox")(
      test("git --version, a quoted echo, and an outside write are separate argv") {
        ZIO.scoped {
          ZIO.acquireRelease(scratch)(dirs => deleteTree(dirs.root)).flatMap(separateArgv)
        }
      } @@ TestAspect.timeout(30.seconds),
    ).when(sandboxPresent),
  )

  private val sandboxPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private def separateArgv(dirs: Dirs) =
    val script   = """printf %s 'a b"c'"""
    val joined   = "/bin/bash -lc \"" + script.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    val outside  = dirs.outside.resolve("hidden.txt")
    val write    = "printf x > " + outside.toString
    val writeLine = "/bin/bash -lc '" + write + "'"
    val bounds   = ShellBounds(dirs.workspace, Chunk(dirs.workspace), Chunk(dirs.workspace), network = false)
    val echoArgv = AcpTerminal.execArgv(joined, Chunk.empty)
    val writeArgv = AcpTerminal.execArgv(writeLine, Chunk.empty)
    val ran =
      (echoArgv, writeArgv) match
        case (Some(echo), Some(redirect)) =>
          for
            profile <- ShellSandbox.live.profile(bounds)
            version <- ShellSandbox.live.run(Chunk("git", "--version"), bounds)
            echoed  <- ShellSandbox.live.run(echo, bounds)
            denied  <- ShellSandbox.live.run(redirect, bounds)
            gone    <- missing(outside)
          yield Some((profile, version, echoed, denied, gone, echo, redirect))
        case _ => ZIO.succeed(None)
    ran.map {
      case None => assertTrue(echoArgv.isDefined, writeArgv.isDefined)
      case Some((profile, version, echoed, denied, gone, echo, redirect)) =>
        assertTrue(
          echo == Chunk("/bin/sh", "-c", script),
          !echo.exists(_.contains("/bin/bash -lc")),
          redirect == Chunk("/bin/sh", "-c", write),
          !profile.contains(script),
          !profile.contains(write),
          version.code == 0,
          version.stdout.contains("git version"),
          echoed.code == 0,
          echoed.stdout == "a b\"c",
          denied.code != 0,
          gone,
        )
    }

  private def prove(dirs: Dirs) =
    val script = """printf x > "$1""""
    val outsideFile = dirs.outside.resolve("hidden.txt")
    val argv        = Chunk("/bin/sh", "-c", script, "sh", outsideFile.toString)
    val shown       = s"/bin/sh -c ${shellQuote(script)} sh ${outsideFile.toString}"
    val readOnly    = ShellWrite.effect(shown) == ShellEffect.ReadOnly
    val body        =
      if ShellSandbox.available then confined(dirs, script, argv)
      else refused(argv, dirs.workspace, outsideFile)
    body.map(result => assertTrue(readOnly) && result)

  private def confined(dirs: Dirs, script: String, outsideArgv: Chunk[String]) =
    val workspace   = dirs.workspace
    val outsideFile = dirs.outside.resolve("hidden.txt")
    val insideFile  = workspace.resolve("landed.txt")
    val blockedFile = workspace.resolve("blocked.txt")
    val nest        = dirs.outside.resolve("nest")
    val through     = nest.resolve("hidden.txt")
    val link        = workspace.resolve("escape")
    val withWrites  = ShellBounds(workspace, Chunk(workspace), Chunk(workspace), network = false)
    val noWrites    = ShellBounds(workspace, Chunk(workspace), Chunk.empty, network = false)
    for
      _        <- write(dirs.outside.resolve("secret.txt"), "nope")
      _        <- write(workspace.resolve("note.txt"), "yes")
      _        <- ZIO.attemptBlocking(Files.createDirectories(nest)).orDie
      _        <- ZIO.attemptBlocking(Files.createSymbolicLink(link, nest)).orDie
      closed   <- ShellSandbox.live.profile(withWrites)
      opened   <- ShellSandbox.live.profile(withWrites.copy(network = true))
      outside  <- ShellSandbox.live.run(outsideArgv, withWrites)
      gone     <- missing(outsideFile)
      inside   <- ShellSandbox.live.run(Chunk("/bin/sh", "-c", script, "sh", insideFile.toString), withWrites)
      landed   <- text(insideFile)
      blocked  <- ShellSandbox.live.run(Chunk("/bin/sh", "-c", script, "sh", blockedFile.toString), noWrites)
      stayed   <- missing(blockedFile)
      secret   <- ShellSandbox.live.run(Chunk("/bin/cat", dirs.outside.resolve("secret.txt").toString), withWrites)
      note     <- ShellSandbox.live.run(Chunk("/bin/cat", workspace.resolve("note.txt").toString), withWrites)
      linked   <- ShellSandbox.live.run(
                    Chunk("/bin/sh", "-c", script, "sh", link.resolve("hidden.txt").toString),
                    withWrites,
                  )
      linkGone <- missing(through)
      lang     <- ShellSandbox.live.run(Chunk("/bin/sh", "-c", """printf %s "$LANG""""), noWrites)
      home     <- ShellSandbox.live.run(
                    Chunk("/bin/sh", "-c", """printf '%s\n%s' "$HOME" "$GIT_CONFIG_GLOBAL""""),
                    noWrites,
                  )
      token    <- ShellSandbox.live.run(Chunk("/bin/sh", "-c", """printf %s "$GH_TOKEN""""), noWrites)
      devNull  <- ShellSandbox.live.run(Chunk("/bin/sh", "-c", "printf x > /dev/null"), noWrites)
      handle   <- ShellSandbox.live.start(Chunk("/bin/sleep", "5"), noWrites, 1000)
      _        <- handle.kill
      _        <- handle.awaitExit
    yield assertTrue(
      !closed.contains(outsideFile.toString),
      !closed.split('\n').contains("(allow network*)"),
      opened.split('\n').contains("(allow network*)"),
      closed.contains("(deny default)"),
      closed.contains("(vnode-type 7)"),
      closed.contains("(allow file-read* (literal \"/\"))"),
      closed.split('\n').contains("(allow file-read-data (literal \"/var\"))"),
      !closed.split('\n').contains("(allow file-read* (subpath \"/var\"))"),
      !closed.split('\n').contains("(allow file-read* (subpath \"/private\"))"),
      !closed.split('\n').contains("(allow file-read* (subpath \"/tmp\"))"),
      !closed.split('\n').contains("(allow file-read* (subpath \"/private/tmp\"))"),
      outside.code != 0,
      gone,
      inside.code == 0,
      landed == "x",
      blocked.code != 0,
      stayed,
      !secret.stdout.contains("nope"),
      secret.code != 0,
      note.stdout.contains("yes"),
      note.code == 0,
      linked.code != 0,
      linkGone,
      lang.stdout == "C",
      lang.code == 0,
      home.stdout == "\n/dev/null",
      token.stdout.isEmpty,
      devNull.code == 0,
    )

  private def refused(argv: Chunk[String], workspace: Path, outsideFile: Path) =
    val bounds = ShellBounds(workspace, Chunk(workspace), Chunk(workspace), network = false)
    ShellSandbox.live.run(argv, bounds).either.flatMap {
      case Left(SandboxError.Unavailable) => missing(outsideFile).map(gone => assertTrue(gone))
      case other                           => ZIO.succeed(assertTrue(other == Left(SandboxError.Unavailable)))
    }

  private def ask(root: Workspace, ceiling: Ceiling, params: Json, sandbox: ShellSandbox) =
    PermissionChoice.decide(
      PermissionPolicy.Clamp(ceiling),
      ceiling,
      root,
      Chunk.empty,
      Chunk.empty,
      params,
      FileRoots.unchanged,
      sandbox = sandbox,
    )

  private def shell(command: String): Json =
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("execute"),
        "rawInput"   -> Json.Obj("command" -> Json.Str(command)),
      ),
      "options" -> options,
    )

  private def shellQuote(raw: String): String =
    "'" + raw.replace("'", "'\\''") + "'"

  private def scratch =
    ZIO
      .attemptBlocking {
        val id   = UUID.randomUUID.toString
        val root = repoRoot(Path.of("").toAbsolutePath).resolve(Path.of("target", "shell-sandbox-e2e", id))
        val workspace = root.resolve("workspace")
        val outside   = root.resolve("outside")
        Files.createDirectories(workspace)
        Files.createDirectories(outside)
        Dirs(root, workspace, outside)
      }
      .orDie

  private def repoRoot(start: Path): Path =
    if Files.exists(start.resolve("histos.scala")) then start
    else
      Option(start.getParent) match
        case Some(parent) => repoRoot(parent)
        case None         => start

  private def deleteTree(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO
      .attemptBlocking {
        def delete(path: Path): Unit =
          if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
            val kids = Files.list(path)
            try kids.forEach(child => delete(child))
            finally kids.close()
          Files.deleteIfExists(path)
        if Files.exists(root) then delete(root)
      }
      .ignore

  private def write(path: Path, body: String) =
    ZIO.attemptBlocking(Files.writeString(path, body)).orDie

  private def text(path: Path) =
    ZIO.attemptBlocking(if Files.exists(path) then Files.readString(path) else "").orDie

  private def missing(path: Path) =
    ZIO.attemptBlocking(!Files.exists(path)).orDie

  private final case class Dirs(root: Path, workspace: Path, outside: Path)
end ShellSandboxSpec
