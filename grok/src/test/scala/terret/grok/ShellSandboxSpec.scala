package terret.grok

import slayer.*
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Ref, ZIO}

import java.nio.file.attribute.PosixFilePermissions
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
          val closed  = SeatbeltProfile.render(SeatbeltPolicy(Chunk(nasty), Chunk.empty, Chunk(root), false))
          val opened  = SeatbeltProfile.render(SeatbeltPolicy(Chunk(work), Chunk(work), Chunk(root), true))
          val reached = SeatbeltProfile.render(
            SeatbeltPolicy(Chunk(select), Chunk.empty, Chunk(priv, privVar), false, Chunk(link)),
          )
          val seen    =
            val closedLines  = closed.split('\n')
            val openedLines  = opened.split('\n')
            val reachedLines = reached.split('\n')
            val newline      = AbsPath.from("/work/\n(allow default)")
            val relative     = AbsPath.from("work")
            val quotedPath   = SeatbeltProfile.quote(quoted)
            assertTrue(
              closed.startsWith("(version 1)\n(deny default)\n"),
              closed.contains("(vnode-type 7)"),
              closed.contains("(allow file-read* (literal \"/\"))"),
              closed.contains("(allow file-map-executable)"),
            ) && assertTrue(
              closed.contains("\"/work/(allow network*)\""),
              !closedLines.contains("(allow network*)"),
              !closed.contains("(allow file-write* (subpath"),
              !closed.contains("(allow file-map-executable (subpath"),
            ) && assertTrue(
              openedLines.contains("(allow network*)"),
              opened.contains("(allow file-write* (subpath \"/work\"))"),
              reachedLines.contains("(allow file-read-data (literal \"/var\"))"),
              reachedLines.contains("(allow file-read-metadata (literal \"/private\"))"),
            ) && assertTrue(
              reachedLines.contains("(allow file-read* (subpath \"/var/select\"))"),
              !reachedLines.contains("(allow file-read* (subpath \"/var\"))"),
              !reachedLines.contains("(allow file-read* (subpath \"/private\"))"),
              newline == Left(AbsPathError.Illegal),
            ) && assertTrue(relative == Left(AbsPathError.NotAbsolute), quotedPath == "\"/work/a\\\"b\\\\c\"")
          seen
      built match
        case Right(result)                  => result
        case Left(AbsPathError.NotAbsolute) => assertTrue(false)
        case Left(AbsPathError.Illegal)     => assertTrue(false)
    },
    test("the argument vector keeps the profile and the command as separate elements") {
      val profile     = "(version 1)\n(deny default)\n"
      val command     = Chunk("/bin/sh", "-c", "echo hi > /tmp/x")
      val confined    = ShellSandbox.confined(profile, command)
      val confinedTo  = Chunk("/usr/bin/sandbox-exec", "-p", profile, "--", "/bin/sh", "-c", "echo hi > /tmp/x")
      val quotedArg   = AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "printf x > \"$1\""))
      val quotedWant  = Some(Chunk("/bin/sh", "-c", "printf x > \"$1\""))
      val spacedArg   = AcpTerminal.execArgv("/bin/bash -lc 'printf %s \"a b\"'", Chunk.empty)
      val spacedWant  = Some(Chunk("/bin/sh", "-c", "printf %s \"a b\""))
      val aposArg     = AcpTerminal.execArgv("/bin/bash -lc 'a'\\''b'", Chunk.empty)
      val aposWant    = Some(Chunk("/bin/sh", "-c", "a'b"))
      val bareEcho    = AcpTerminal.execArgv("/bin/bash -lc echo hi", Chunk.empty)
      val extraArg    = AcpTerminal.execArgv("/bin/bash -lc 'echo hi' extra", Chunk.empty)
      val nulArg      = AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "ok\u0000"))
      val manyArg     = AcpTerminal.execArgv("/bin/bash", Chunk("-lc", "echo", "hi"))
      val loginArg    = AcpTerminal.execArgv("/bin/bash", Chunk("--login", "-c", "printf x"))
      val versionArg  = AcpTerminal.execArgv("/bin/bash", Chunk("--version"))
      val versionWant = Some(Chunk("/bin/bash", "--version"))
      val dashCArg    = AcpTerminal.execArgv("/bin/bash", Chunk("-c", "printf x"))
      val dashCWant   = Some(Chunk("/bin/bash", "-c", "printf x"))
      val catArg      = AcpTerminal.execArgv("/bin/cat", Chunk("/work/note"))
      val catWant     = Some(Chunk("/bin/cat", "/work/note"))
      val writeOnce   = ShellWrite.effect(AcpTerminal.commandText("/bin/bash", Chunk("-lc", "echo hi > /tmp/x")))
      val writeJoin   = ShellWrite.effect(AcpTerminal.commandText("/bin/bash -lc 'echo hi > /tmp/x'", Chunk.empty))
      val writes      = ShellEffect.Writes(Chunk("/tmp/x"))
      val missing     = ShellSandbox.skipReason(false)
      val present     = ShellSandbox.skipReason(true)
      val create      = AcpTerminal.canon("x.ai/terminal/create")
      val kill        = AcpTerminal.canon("_x.ai/terminal/kill")
      val prompt      = AcpTerminal.canon("session/prompt")
      assertTrue(confined == confinedTo, quotedArg == quotedWant, spacedArg == spacedWant, aposArg == aposWant) &&
        assertTrue(bareEcho.isEmpty, extraArg.isEmpty, nulArg.isEmpty, manyArg.isEmpty) &&
        assertTrue(loginArg.isEmpty, versionArg == versionWant, dashCArg == dashCWant, catArg == catWant) &&
        assertTrue(
          writeOnce == writes,
          writeJoin == writes,
          missing == Some("sandbox-exec is missing"),
          present.isEmpty,
        ) &&
        assertTrue(create.contains("terminal/create"), kill.contains("terminal/kill"), prompt.isEmpty)
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
          (bounds.reads.map(_.toString), bounds.writes.map(_.toString), bounds.network, bounds.writable)
        }
        assertTrue(
          texts.lift(0) == Some((Chunk("/work"), Chunk.empty, false, false)),
          texts.lift(1) == Some((Chunk("/work"), Chunk.empty, true, false)),
          texts.lift(2) == Some((Chunk("/work"), Chunk("/work"), false, true)),
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
    test("policyOf reads caches without WriteWorkspace and grants only the temp directory") {
      val built =
        for
          work    <- AbsPath.from("/terret-policy/work")
          jdkRoot <- AbsPath.from("/terret-policy/home/.sdkman/candidates/java/25")
          jdkLink <- AbsPath.from("/terret-policy/home/.sdkman/candidates/java/current")
          sbtRoot <- AbsPath.from("/terret-policy/home/sdks/sbt-v")
          sbtLink <- AbsPath.from("/terret-policy/home/.sdkman/candidates/sbt/current")
          maven   <- AbsPath.from("/terret-policy/home/.m2")
          ivy     <- AbsPath.from("/terret-policy/home/.ivy2")
          temp    <- AbsPath.from("/terret-policy/tmp/terret-1")
          sdks    <- AbsPath.from("/terret-policy/home/sdks")
        yield
          val caches = Chunk(CacheGrant(CacheDir.Maven, maven), CacheGrant(CacheDir.Ivy, ivy))
          val off    = renderPolicy(
            work,
            caches,
            jdkRoot,
            jdkLink,
            sbtRoot,
            sbtLink,
            temp,
            writable = false,
            network = false,
          )
          val on     = renderPolicy(
            work,
            caches,
            jdkRoot,
            jdkLink,
            sbtRoot,
            sbtLink,
            temp,
            writable = true,
            network = true,
          )
          val quiet  = off.split('\n')
          val open   = on.split('\n')
          val seen   =
            val jdkRead       = readSub(jdkRoot.text)
            val jdkMeta       = meta(jdkLink.text)
            val jdkData       = data(jdkLink.text)
            val sbtRead       = readSub(sbtRoot.text)
            val sbtMeta       = meta(sbtLink.text)
            val sbtData       = data(sbtLink.text)
            val sdksRead      = readSub(sdks.text)
            val mavenRead     = readSub(maven.text)
            val mavenWrite    = writeSub(maven.text)
            val jdkWrite      = writeSub(jdkRoot.text)
            val sbtWrite      = writeSub(sbtRoot.text)
            val tempRead      = readSub(temp.text)
            val tempWrite     = writeSub(temp.text)
            val parentRead    = readSub("/terret-policy/tmp")
            val parentWrite   = writeSub("/terret-policy/tmp")
            val settingsRead  = denyRead(maven.text + "/settings.xml")
            val guardRead     = denyRead(maven.text + "/settings-security.xml")
            val credsRead     = denyRead(ivy.text + "/.credentials")
            val settingsWrite = denyWrite(maven.text + "/settings.xml")
            val guardWrite    = denyWrite(maven.text + "/settings-security.xml")
            val credsWrite    = denyWrite(ivy.text + "/.credentials")
            val readOrdered   = follows(quiet, mavenRead, settingsRead)
            val writeOrdered  = follows(open, mavenWrite, settingsWrite)
            val noMapSubpath  = !quiet.exists(_.startsWith("(allow file-map-executable (subpath"))
            assertTrue(
              quiet.contains(jdkRead),
              quiet.contains(jdkMeta),
              quiet.contains(jdkData),
              quiet.contains(sbtRead),
            ) && assertTrue(
              quiet.contains(sbtMeta),
              quiet.contains(sbtData),
              !quiet.contains(sdksRead),
              quiet.contains(mavenRead),
            ) && assertTrue(
              !quiet.contains(mavenWrite),
              !quiet.contains(jdkWrite),
              !quiet.contains(sbtWrite),
              open.contains(mavenWrite),
            ) && assertTrue(
              quiet.contains(tempRead),
              quiet.contains(tempWrite),
              !quiet.contains(parentRead),
              !quiet.contains(parentWrite),
            ) && assertTrue(
              quiet.contains(settingsRead),
              quiet.contains(guardRead),
              quiet.contains(credsRead),
              quiet.contains(settingsWrite),
            ) && assertTrue(
              quiet.contains(guardWrite),
              quiet.contains(credsWrite),
              open.contains(settingsWrite),
              readOrdered,
            ) && assertTrue(
              writeOrdered,
              !quiet.contains("(allow network*)"),
              open.contains("(allow network*)"),
              noMapSubpath,
            )
          seen
      built match
        case Right(result)                  => result
        case Left(AbsPathError.NotAbsolute) => assertTrue(false)
        case Left(AbsPathError.Illegal)     => assertTrue(false)
    },
    test("a session temp directory is private and removed without following links") {
      ZIO.scoped {
        ZIO.acquireRelease(TestHome.scratch("session-temp"))(TestHome.delete).flatMap { parent =>
          for
            dir    <- SessionTemp.open(parent)
            perms  <- ZIO.attemptBlocking(Files.getPosixFilePermissions(dir)).orDie
            keep   <- ZIO.attemptBlocking {
              val file = parent.resolve("keep")
              Files.writeString(file, "stay")
              Files.createSymbolicLink(dir.resolve("out"), file)
              file
            }.orDie
            _      <- SessionTemp.remove(dir)
            gone   <- ZIO.attemptBlocking(!Files.exists(dir, LinkOption.NOFOLLOW_LINKS)).orDie
            stayed <- ZIO.attemptBlocking(Files.readString(keep) == "stay").orDie
          yield assertTrue(perms == PosixFilePermissions.fromString("rwx------"), gone, stayed)
        }
      }
    },
    test("a hidden write outside the workspace does not land") {
      ZIO.scoped {
        for
          dirs    <- ZIO.acquireRelease(scratch)(dirs => deleteTree(dirs.root))
          sandbox <- ShellSandbox.make(FileRoots.home)
          result  <- prove(dirs, sandbox)
        yield result
      }
    } @@ TestAspect.timeout(30.seconds),
    suite("macos sandbox")(
      test("git --version, a quoted echo, and an outside write are separate argv") {
        ZIO.scoped {
          for
            dirs    <- ZIO.acquireRelease(scratch)(dirs => deleteTree(dirs.root))
            sandbox <- ShellSandbox.make(FileRoots.home)
            result  <- separateArgv(dirs, sandbox)
          yield result
        }
      } @@ TestAspect.timeout(30.seconds),
    ).when(sandboxPresent),
  )

  private val sandboxPresent: Boolean =
    val present = ShellSandbox.available
    ShellSandbox.skipReason(present).foreach(reason => System.err.println(s"SKIP macos sandbox: $reason"))
    present

  private def separateArgv(dirs: Dirs, sandbox: ShellSandbox) =
    val script    = """printf %s 'a b"c'"""
    val joined    = "/bin/bash -lc \"" + script.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    val outside   = dirs.outside.resolve("hidden.txt")
    val write     = "printf x > " + outside.toString
    val writeLine = "/bin/bash -lc '" + write + "'"
    val bounds    = ShellBounds(
      dirs.workspace,
      Chunk(dirs.workspace),
      Chunk(dirs.workspace),
      network = false,
      writable = true,
    )
    val echoArgv  = AcpTerminal.execArgv(joined, Chunk.empty)
    val writeArgv = AcpTerminal.execArgv(writeLine, Chunk.empty)
    val ran       =
      (echoArgv, writeArgv) match
        case (Some(echo), Some(redirect)) =>
          for
            profile <- sandbox.profile(bounds)
            version <- sandbox.run(Chunk("git", "--version"), bounds)
            echoed  <- sandbox.run(echo, bounds)
            denied  <- sandbox.run(redirect, bounds)
            gone    <- missing(outside)
          yield Some((profile, version, echoed, denied, gone, echo, redirect))
        case _ => ZIO.succeed(None)
    ran.map {
      case None => assertTrue(echoArgv.isDefined, writeArgv.isDefined)
      case Some((profile, version, echoed, denied, gone, echo, redirect)) =>
        val echoSh     = echo == Chunk("/bin/sh", "-c", script)
        val echoBash   = echo.exists(_.contains("/bin/bash -lc"))
        val redirectSh = redirect == Chunk("/bin/sh", "-c", write)
        assertTrue(echoSh, !echoBash, redirectSh, !profile.contains(script)) &&
          assertTrue(
            !profile.contains(write),
            version.code == 0,
            version.stdout.contains("git version"),
            echoed.code == 0,
          ) &&
          assertTrue(echoed.stdout == "a b\"c", denied.code != 0, gone)
    }

  private def prove(dirs: Dirs, sandbox: ShellSandbox) =
    val script      = """printf x > "$1""""
    val outsideFile = dirs.outside.resolve("hidden.txt")
    val argv        = Chunk("/bin/sh", "-c", script, "sh", outsideFile.toString)
    val shown       = s"/bin/sh -c ${shellQuote(script)} sh ${outsideFile.toString}"
    val readOnly    = ShellWrite.effect(shown) == ShellEffect.ReadOnly
    val body        =
      if ShellSandbox.available then confined(dirs, sandbox, script, argv)
      else refused(sandbox, argv, dirs.workspace, outsideFile)
    body.map(result => assertTrue(readOnly) && result)

  private def confined(dirs: Dirs, sandbox: ShellSandbox, script: String, outsideArgv: Chunk[String]) =
    val workspace   = dirs.workspace
    val outsideFile = dirs.outside.resolve("hidden.txt")
    val insideFile  = workspace.resolve("landed.txt")
    val blockedFile = workspace.resolve("blocked.txt")
    val nest        = dirs.outside.resolve("nest")
    val through     = nest.resolve("hidden.txt")
    val link        = workspace.resolve("escape")
    val withWrites  =
      ShellBounds(workspace, Chunk(workspace), Chunk(workspace), network = false, writable = true)
    val noWrites    =
      ShellBounds(workspace, Chunk(workspace), Chunk.empty, network = false, writable = false)
    for
      _        <- write(dirs.outside.resolve("secret.txt"), "nope")
      _        <- write(workspace.resolve("note.txt"), "yes")
      _        <- ZIO.attemptBlocking(Files.createDirectories(nest)).orDie
      _        <- ZIO.attemptBlocking(Files.createSymbolicLink(link, nest)).orDie
      closed   <- sandbox.profile(withWrites)
      opened   <- sandbox.profile(withWrites.copy(network = true))
      outside  <- sandbox.run(outsideArgv, withWrites)
      gone     <- missing(outsideFile)
      inside   <- sandbox.run(Chunk("/bin/sh", "-c", script, "sh", insideFile.toString), withWrites)
      landed   <- text(insideFile)
      blocked  <- sandbox.run(Chunk("/bin/sh", "-c", script, "sh", blockedFile.toString), noWrites)
      stayed   <- missing(blockedFile)
      secret   <- sandbox.run(Chunk("/bin/cat", dirs.outside.resolve("secret.txt").toString), withWrites)
      note     <- sandbox.run(Chunk("/bin/cat", workspace.resolve("note.txt").toString), withWrites)
      linked   <- sandbox.run(
                    Chunk("/bin/sh", "-c", script, "sh", link.resolve("hidden.txt").toString),
                    withWrites,
                  )
      linkGone <- missing(through)
      lang     <- sandbox.run(Chunk("/bin/sh", "-c", """printf %s "$LANG""""), noWrites)
      home     <- sandbox.run(
                    Chunk("/bin/sh", "-c", """printf '%s\n%s' "$HOME" "$GIT_CONFIG_GLOBAL""""),
                    noWrites,
                  )
      token    <- sandbox.run(Chunk("/bin/sh", "-c", """printf %s "$GH_TOKEN""""), noWrites)
      devNull  <- sandbox.run(Chunk("/bin/sh", "-c", "printf x > /dev/null"), noWrites)
      tempDir  <- sandbox.run(Chunk("/bin/sh", "-c", """printf %s "$TMPDIR""""), noWrites)
      toolOpt  <- sandbox.run(Chunk("/bin/sh", "-c", """printf %s "$JAVA_TOOL_OPTIONS""""), noWrites)
      touched  <- sandbox.run(Chunk("/bin/sh", "-c", """touch "$TMPDIR/ok""""), noWrites)
      dir      = Path.of(tempDir.stdout)
      parent   = Option(dir.getParent).getOrElse(dir)
      sibling  = parent.resolve("terret-sibling-denied")
      sibTouch <- sandbox.run(Chunk("/usr/bin/touch", sibling.toString), noWrites)
      okFile   <- ZIO.attemptBlocking(Files.exists(dir.resolve("ok"))).orDie
      sibFile  <- ZIO.attemptBlocking(Files.exists(sibling)).orDie
      quiet    <- sandbox.profile(noWrites)
      handle   <- sandbox.start(Chunk("/bin/sleep", "5"), noWrites, 1000)
      _        <- handle.kill
      _        <- handle.awaitExit
    yield
      val closedLn = closed.split('\n')
      val openedLn = opened.split('\n')
      val quietLn  = quiet.split('\n')
      val tmpRead  = "(allow file-read* (subpath \"" + tempDir.stdout + "\"))"
      val tmpWrite = "(allow file-write* (subpath \"" + tempDir.stdout + "\"))"
      val parRead  = "(allow file-read* (subpath \"" + parent.toString + "\"))"
      val parWrite = "(allow file-write* (subpath \"" + parent.toString + "\"))"
      val jvmTmp   = s"""-Djava.io.tmpdir="${tempDir.stdout}""""
      assertTrue(
        !closed.contains(outsideFile.toString),
        !closedLn.contains("(allow network*)"),
        openedLn.contains("(allow network*)"),
        closed.contains("(deny default)"),
      ) && assertTrue(
        closed.contains("(vnode-type 7)"),
        closed.contains("(allow file-read* (literal \"/\"))"),
        closedLn.contains("(allow file-read-data (literal \"/var\"))"),
        !closedLn.contains("(allow file-read* (subpath \"/var\"))"),
      ) && assertTrue(
        !closedLn.contains("(allow file-read* (subpath \"/private\"))"),
        !closedLn.contains("(allow file-read* (subpath \"/tmp\"))"),
        !closedLn.contains("(allow file-read* (subpath \"/private/tmp\"))"),
        !closedLn.contains("(allow file-read* (subpath \"/var/folders\"))"),
      ) && assertTrue(
        !closedLn.contains("(allow file-write* (subpath \"/var/folders\"))"),
        tempDir.code == 0,
        tempDir.stdout.nonEmpty,
        quiet.contains(tmpRead),
      ) && assertTrue(
        quiet.contains(tmpWrite),
        !quietLn.contains(parRead),
        !quietLn.contains(parWrite),
        toolOpt.stdout == jvmTmp,
      ) && assertTrue(
        touched.code == 0,
        okFile,
        sibTouch.code != 0,
        !sibFile,
      ) && assertTrue(
        outside.code != 0,
        gone,
        inside.code == 0,
        landed == "x",
      ) && assertTrue(
        blocked.code != 0,
        stayed,
        !secret.stdout.contains("nope"),
        secret.code != 0,
      ) && assertTrue(
        note.stdout.contains("yes"),
        note.code == 0,
        linked.code != 0,
        linkGone,
      ) && assertTrue(
        lang.stdout == "C",
        lang.code == 0,
        home.stdout == "\n/dev/null",
        token.stdout.isEmpty,
      ) && assertTrue(devNull.code == 0)

  private def refused(sandbox: ShellSandbox, argv: Chunk[String], workspace: Path, outsideFile: Path) =
    val bounds = ShellBounds(workspace, Chunk(workspace), Chunk(workspace), network = false, writable = true)
    sandbox.run(argv, bounds).either.flatMap {
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

  private def renderPolicy(
      work: AbsPath,
      caches: Chunk[CacheGrant],
      jdkRoot: AbsPath,
      jdkLink: AbsPath,
      sbtRoot: AbsPath,
      sbtLink: AbsPath,
      temp: AbsPath,
      writable: Boolean,
      network: Boolean,
  ): String =
    val writes = if writable then Chunk(work) else Chunk.empty
    SeatbeltProfile.render(
      ShellSandbox.policyOf(
        Chunk(work),
        writes,
        work,
        network,
        writable,
        Some(Granted(jdkRoot, jdkLink)),
        Some(Granted(sbtRoot, sbtLink)),
        caches,
        Some(temp),
        _ => None,
      )
    )

  private def readSub(path: String): String =
    s"(allow file-read* (subpath \"$path\"))"

  private def writeSub(path: String): String =
    s"(allow file-write* (subpath \"$path\"))"

  private def meta(path: String): String =
    s"(allow file-read-metadata (literal \"$path\"))"

  private def data(path: String): String =
    s"(allow file-read-data (literal \"$path\"))"

  private def denyRead(path: String): String =
    s"(deny file-read* (literal \"$path\"))"

  private def denyWrite(path: String): String =
    s"(deny file-write* (literal \"$path\"))"

  private def follows(lines: Array[String], earlier: String, later: String): Boolean =
    val left  = lines.indexOf(earlier)
    val right = lines.indexOf(later)
    left >= 0 && right > left

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
