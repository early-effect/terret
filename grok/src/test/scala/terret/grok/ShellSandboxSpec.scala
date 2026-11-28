package terret.grok

import slayer.*
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Ref, UIO, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, LinkOption, Path}
import java.util.UUID

object ShellSandboxSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  def spec = suite("shell sandbox")(
    test("a profile is deny-default, quotes paths, and opens the network only when granted") {
      val nasty = """/work/(allow network*)"""
      val off    = SeatbeltProfile.render(SeatbeltPolicy(Chunk(nasty), Chunk.empty, Chunk("/"), false))
      val on     = SeatbeltProfile.render(SeatbeltPolicy(Chunk("/work"), Chunk("/work"), Chunk("/"), true))
      val linked = SeatbeltProfile.render(
        SeatbeltPolicy(Chunk("/var/select"), Chunk.empty, Chunk("/private", "/private/var"), false, Chunk("/var")),
      )
      val quote = SeatbeltProfile.quote("/work/a\"b\\c")
      (off, on, linked) match
        case (Right(closed), Right(opened), Right(reached)) =>
          assertTrue(
            closed.startsWith("(version 1)\n(deny default)\n"),
            closed.contains("(vnode-type 7)"),
            closed.contains("(allow file-read* (literal \"/\"))"),
            closed.contains("\"/work/(allow network*)\""),
            !closed.split('\n').contains("(allow network*)"),
            !closed.contains("(allow file-write* (subpath"),
            opened.split('\n').contains("(allow network*)"),
            opened.contains("(allow file-write* (subpath \"/work\"))"),
            opened.contains("(allow network-bind (local unix-socket (subpath \"/work\")))"),
            !closed.contains("network-bind"),
            reached.split('\n').contains("(allow file-read-data (literal \"/var\"))"),
            reached.split('\n').contains("(allow file-read-metadata (literal \"/private\"))"),
            reached.split('\n').contains("(allow file-read* (subpath \"/var/select\"))"),
            !reached.split('\n').contains("(allow file-read* (subpath \"/var\"))"),
            !reached.split('\n').contains("(allow file-read* (subpath \"/private\"))"),
            SeatbeltProfile.quote("/work/\n(allow default)") == Left(SandboxError.BadPath("/work/\n(allow default)")),
            SeatbeltProfile.quote("work") == Left(SandboxError.BadPath("work")),
            quote == Right("\"/work/a\\\"b\\\\c\""),
            servicesNamed(closed),
            servicesNamed(opened),
            servicesNamed(reached),
          )
        case _ => assertTrue(false)
    },
    test("a jdk grant is the sdkman link and its real directory, and a target that covers home is dropped") {
      val link = "/Users/me/.sdkman/candidates/java/current"
      val real = "/Users/me/.sdkman/candidates/java/25.0.5.1+2-tem"
      val home = "/Users/me"
      val granted = JdkGrant.paths(link, Some(real), home)
      SeatbeltProfile.render(SeatbeltPolicy(granted, Chunk.empty, Chunk("/"), false, Chunk.empty, granted)) match
        case Right(profile) =>
          val lines = profile.split('\n')
          assertTrue(
            granted == Chunk(link, real),
            lines.contains(s"(allow file-read* (subpath \"$link\"))"),
            lines.contains(s"(allow file-read* (subpath \"$real\"))"),
            lines.contains(s"(allow file-map-executable (subpath \"$link\"))"),
            lines.contains(s"(allow file-map-executable (subpath \"$real\"))"),
            !lines.exists(_.startsWith("(allow file-write")),
            JdkGrant.paths(link, Some(link), home) == Chunk(link),
            JdkGrant.paths(link, Some(home), home).isEmpty,
            JdkGrant.paths(link, Some("/"), home).isEmpty,
            JdkGrant.paths(link, Some("/Users/me/.sdkman"), home).isEmpty,
            JdkGrant.paths("java", None, home).isEmpty,
            JdkGrant.paths("java", Some(real), home).isEmpty,
            JdkGrant.paths(link + "\n", Some("/"), home).isEmpty,
          )
        case Left(_) => assertTrue(false)
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
      test("the keychain stays closed while git and a histos test run") {
        ZIO.scoped {
          ZIO.acquireRelease(scratch)(dirs => deleteTree(dirs.root)).flatMap(keychain)
        }
      } @@ TestAspect.timeout(4.minutes) @@ TestAspect.withLiveClock,
    ).when(sandboxPresent),
  )

  private val sandboxPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private def servicesNamed(profile: String): Boolean =
    val lines = profile.split('\n')
    val ruled = lines.zipWithIndex.filter { (line, _) =>
      line.startsWith("(allow mach-") || line.startsWith("(allow ipc-") || line.startsWith("(allow system-")
    }
    !lines.contains("(allow mach*)") &&
    !lines.contains("(allow ipc*)") &&
    !lines.contains("(allow system*)") &&
    ruled.forall { (line, index) =>
      !line.contains("*") &&
      index > 0 &&
      lines(index - 1).startsWith("; ") &&
      (!line.startsWith("(allow mach-lookup") || line.contains("(global-name \""))
    }

  private def keychain(dirs: Dirs) =
    val javaHome = Option(System.getProperty("java.home")).filter(_.nonEmpty).map(Path.of(_))
    val installed = FileRoots.home.resolve(".histos").resolve("bin").resolve("histos")
    val launcher  = if executable(installed) then Some(installed) else commandOnPath("histos")
    (javaHome, launcher) match
      case (Some(home), Some(_)) =>
        ZIO.acquireRelease(shortHome)(home => reap(home) *> deleteTree(home)).flatMap(socket =>
          confineKeychain(dirs, home, socket)
        )
      case _                     =>
        ZIO.succeed(assertTrue(javaHome.isDefined, launcher.isDefined) ?? "java.home or histos is missing")

  private def confineKeychain(dirs: Dirs, javaHome: Path, linked: Path) =
    val workspace = dirs.workspace
    val project   = workspace.resolve("project")
    val tmp       = workspace.resolve("tmp")
    val xdg       = workspace.resolve("xdg")
    val realHome  = FileRoots.home.resolve(".histos")
    // macOS sun_path holds 103 bytes. The socket is HISTOS_HOME/run/<16 hex>.sock, so the home stays short.
    val socket    = linked.resolve("run").resolve("0123456789abcdef.sock").toString
    val reads     = Chunk(workspace, javaHome, realHome, linked) ++ caches(FileRoots.home)
    val bounds    = ShellBounds(
      cwd = workspace,
      reads = reads,
      writes = Chunk(workspace, linked),
      network = false,
      javaHome = Some(javaHome),
      histosHome = Some(linked),
      tmp = Some(tmp),
      home = Some(FileRoots.home),
      xdgCache = Some(xdg),
    )
    val probe   = Chunk("/usr/bin/security", "find-generic-password", "-a", "terret-probe", "-s", "terret-probe")
    val chains  = Chunk("/usr/bin/security", "list-keychains")
    val javaBin = JdkGrant.host match
      case root +: _ => Path.of(root).resolve("bin").resolve("java")
      case _         => commandOnPath("java").getOrElse(Path.of("java"))
    val version = Chunk(javaBin.toString, "-version")
    val suite      = linked.resolve("bin").resolve("histos").toString
    val javaParent = Option(javaHome.getParent).map(_.toString).filter(_ != "/")
    for
      _        <- prepareProbe(linked, realHome, project, tmp, xdg)
      profile  <- ShellSandbox.live.profile(bounds)
      outside  <- host(chains)
      listed   <- bounded(chains, bounds, 20.seconds)
      security <- bounded(probe, bounds, 20.seconds)
      git      <- bounded(Chunk("/usr/bin/git", "--version"), bounds, 20.seconds)
      booted   <- bounded(version, bounds, 20.seconds)
      tested   <-
        if booted.code == 0 then bounded(Chunk(suite, "test"), bounds.copy(cwd = project), 3.minutes)
        else ZIO.succeed(booted)
      denials <-
        if held(outside, listed, security, git, booted, tested) then ZIO.succeed("")
        else recentDenials
    yield
      val lines = profile.split('\n')
      val jdkOk = JdkGrant.host.forall { path =>
        SeatbeltProfile.quote(path) match
          case Right(quoted) =>
            lines.contains(s"(allow file-read* (subpath $quoted))") &&
            lines.contains(s"(allow file-map-executable (subpath $quoted))") &&
            !lines.exists(line => line.startsWith("(allow file-write") && line.contains(quoted))
          case Left(_) => false
      }
      assertTrue(
        jdkOk,
      javaParent.forall { parent =>
        lines.contains(s"(allow file-read-metadata (literal \"$parent\"))") &&
        !lines.contains(s"(allow file-read* (subpath \"$parent\"))")
      },
      socket.getBytes(StandardCharsets.UTF_8).length <= 103,
      !profile.split('\n').contains("(allow network*)"),
      profile.contains("(allow network-bind (local unix-socket (subpath"),
      outside.code == 0,
      outside.text.contains("keychain"),
      listed.code != 0,
      security.code != 0,
      !security.stdout.contains("password:"),
      git.code == 0,
      git.stdout.contains("git version"),
      booted.code == 0,
      tested.code == 0,
      (tested.stdout + tested.stderr).contains("passed"),
    ) ?? denials

  private def held(
      outside: HostRun,
      listed: SandboxOutput,
      security: SandboxOutput,
      git: SandboxOutput,
      booted: SandboxOutput,
      tested: SandboxOutput,
  ): Boolean =
    outside.code == 0 &&
    outside.text.contains("keychain") &&
    listed.code != 0 &&
    security.code != 0 &&
    !security.stdout.contains("password:") &&
    git.code == 0 &&
    git.stdout.contains("git version") &&
    booted.code == 0 &&
    tested.code == 0 &&
    (tested.stdout + tested.stderr).contains("passed")

  private def bounded(argv: Chunk[String], bounds: ShellBounds, limit: zio.Duration) =
    ShellSandbox.live.run(argv, bounds).timeout(limit).flatMap {
      case Some(output) => ZIO.succeed(output)
      case None         => recentDenials.map(text => SandboxOutput(124, "", s"timed out\n$text"))
    }

  private def host(argv: Chunk[String]) =
    ZIO
      .attemptBlocking {
        val process = new ProcessBuilder(argv.toArray*).redirectErrorStream(true).start()
        val text    = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        HostRun(process.waitFor(), text)
      }
      .orDie

  private def recentDenials =
    ZIO
      .attemptBlocking {
        val process = new ProcessBuilder(
          "/usr/bin/log",
          "show",
          "--last",
          "1m",
          "--style",
          "compact",
          "--info",
          "--predicate",
          "subsystem == \"com.apple.sandbox.reporting\" AND eventMessage CONTAINS \"deny(\" AND " +
            "(process == \"java\" OR process == \"histos\" OR process == \"security\" OR process == \"git\")",
        ).redirectErrorStream(true).start()
        val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
        val code = process.waitFor()
        val kept = text.linesIterator.take(40).mkString("\n")
        s"log exit $code bytes ${text.length}\n$kept"
      }
      .orElseSucceed("log unavailable")

  private def prepareProbe(linked: Path, realHome: Path, project: Path, tmp: Path, xdg: Path) =
    ZIO
      .attemptBlocking {
        Files.createDirectories(tmp)
        Files.createDirectories(xdg)
        Files.createDirectories(linked.resolve("run"))
        Files.createDirectories(project.resolve("app/src/main/scala/probe"))
        Files.createDirectories(project.resolve("app/src/test/scala/probe"))
        linkChildren(linked, realHome)
        val pin = repoRoot(Path.of("").toAbsolutePath).resolve("build.properties")
        val version =
          if Files.isRegularFile(pin) then
            Files.readString(pin).linesIterator.find(_.startsWith("histos.version="))
          else None
        version.foreach(line => Files.writeString(project.resolve("build.properties"), line + "\n"))
        Files.writeString(project.resolve("histos.scala"), probeBuild)
        Files.writeString(project.resolve("app/src/main/scala/probe/Main.scala"), probeMain)
        Files.writeString(project.resolve("app/src/test/scala/probe/MainSpec.scala"), probeSpec)
      }
      .orDie

  private def linkChildren(linked: Path, realHome: Path): Unit =
    if directory(realHome) then
      val kids = Files.list(realHome)
      try
        kids.forEach { child =>
          Option(child.getFileName).map(_.toString).foreach { name =>
            if name != "run" then
              val dest = linked.resolve(name)
              if !Files.exists(dest, LinkOption.NOFOLLOW_LINKS) then
                Files.createSymbolicLink(dest, child.toAbsolutePath)
          }
        }
      finally kids.close()

  private def caches(home: Path): Chunk[Path] =
    Chunk(
      home.resolve("Library").resolve("Caches").resolve("Coursier"),
      home.resolve(".cache").resolve("coursier"),
      home.resolve(".ivy2"),
    ).filter(directory)

  private def directory(path: Path): Boolean =
    try Files.isDirectory(path)
    catch case _: java.io.IOException | _: SecurityException => false

  private def executable(path: Path): Boolean =
    try Files.isExecutable(path)
    catch case _: java.io.IOException | _: SecurityException | _: InvalidPathException => false

  private def commandOnPath(name: String): Option[Path] =
    Option(System.getenv("PATH"))
      .getOrElse("")
      .split(":")
      .iterator
      .map(dir => Path.of(dir).resolve(name))
      .find(executable)

  private val probeBuild: String =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val app = module("app", Jvm)(scala"3.10.1")
      |      .libraryDependencies(dep"dev.zio::zio:2.1.27")
      |      .testDependencies(
      |        dep"dev.zio::zio-test:2.1.27",
      |        dep"dev.zio::zio-test-sbt:2.1.27",
      |      )
      |      .main(MainClass("probe.Main"))
      |    Projects(app)
      |""".stripMargin

  private val probeMain: String =
    """package probe
      |
      |import zio.*
      |
      |object Main extends ZIOAppDefault:
      |  val hello: String = "hello"
      |
      |  def run = Console.printLine(hello)
      |""".stripMargin

  private val probeSpec: String =
    """package probe
      |
      |import zio.test.*
      |
      |object MainSpec extends ZIOSpecDefault:
      |  def spec = suite("Main")(
      |    test("hello")(assertTrue(Main.hello == "hello"))
      |  )
      |""".stripMargin

  private final case class HostRun(code: Int, text: String)

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
      servicesNamed(closed),
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

  /** The histos server stays in this process group after the client exits. Kill whoever holds its socket. */
  private def reap(linked: Path): UIO[Unit] =
    ZIO
      .attemptBlocking {
        val run = linked.resolve("run")
        if Files.isDirectory(run) then
          val kids = Files.list(run)
          try kids.forEach(sock => end(sock))
          finally kids.close()
      }
      .ignore

  private def end(sock: Path): Unit =
    val lsof = new ProcessBuilder("/usr/sbin/lsof", "-t", sock.toString).redirectErrorStream(true).start()
    val text = new String(lsof.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    lsof.waitFor()
    val self = ProcessHandle.current().pid()
    text.linesIterator.foreach { line =>
      line.trim.toLongOption match
        case Some(pid) if pid != self =>
          ProcessHandle.of(pid).ifPresent { handle =>
            handle.destroyForcibly()
            val until = System.nanoTime() + 1000000000L
            while handle.isAlive && System.nanoTime() < until do ()
          }
        case _ => ()
    }

  private def shortHome =
    ZIO
      .attemptBlocking {
        val id   = UUID.randomUUID.toString.take(8)
        val home = repoRoot(Path.of("").toAbsolutePath).resolve(Path.of("target", "sb", id))
        Files.createDirectories(home)
        home
      }
      .orDie

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
