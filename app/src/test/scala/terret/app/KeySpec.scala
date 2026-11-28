package terret.app

import slayer.*
import terret.mcp.EventsKey
import zio.test.*
import zio.{Chunk, Exit, Scope, ZIO}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import scala.jdk.CollectionConverters.*

object KeySpec extends ZIOSpecDefault:
  private val Secret = "file-mode-secret"

  def spec = suite("one key")(
    test("a private file loads and a looser mode, link, or missing file is refused") {
      ZIO.scoped {
        for
          dir      <- scratch
          tight    <- write(dir, "tight.env", "rw-------", s"TERRET_KEY=$Secret\n")
          readable <- write(dir, "read.env", "r--------", s"export TERRET_KEY=$Secret\n")
          world    <- write(dir, "world.env", "rw-r--r--", s"TERRET_KEY=$Secret\n")
          group    <- write(dir, "group.env", "rw-r-----", s"TERRET_KEY=$Secret\n")
          execute  <- write(dir, "exec.env", "rwx------", s"TERRET_KEY=$Secret\n")
          empty    <- write(dir, "empty.env", "rw-------", "TERRET_KEY=\n")
          link     <- ZIO.attempt(Files.createSymbolicLink(dir.resolve("link.env"), tight))
          loaded   <- KeyFile.load(tight)
          owner    <- KeyFile.load(readable)
          exposed  <- KeyFile.load(world).exit
          grouped  <- KeyFile.load(group).exit
          marked   <- KeyFile.load(execute).exit
          blank    <- KeyFile.load(empty).exit
          linked   <- KeyFile.load(link).exit
          folder   <- KeyFile.load(dir).exit
          missing  <- KeyFile.load(dir.resolve("absent.env")).exit
        yield assertTrue(
          accepts(loaded),
          accepts(owner),
          exposed == Exit.fail(KeyError.Exposed),
          grouped == Exit.fail(KeyError.Exposed),
          marked == Exit.fail(KeyError.Exposed),
          blank == Exit.fail(KeyError.Empty),
          linked == Exit.fail(KeyError.Unreadable),
          folder == Exit.fail(KeyError.Unreadable),
          missing == Exit.fail(KeyError.Unreadable),
          !KeyError.Exposed.render.contains(Secret),
          !KeyError.Unreadable.render.contains(Secret),
          !KeyError.Empty.render.contains(Secret),
        )
      }
    },
    test("the env file sets TERRET_KEY once and does not expand it") {
      val secret = "parser-secret"
      assertTrue(
        KeyFile.parse(s"# note\n\nOTHER=1\nexport TERRET_KEY=$secret\n") == Right(secret),
        KeyFile.parse("TERRET_KEY=\"a$b\"\n") == Right("a$b"),
        KeyFile.parse("TERRET_KEY='a$b'\n") == Right("a$b"),
        KeyFile.parse("TERRET_KEY=\"\"\n") == Left(KeyError.Empty),
        KeyFile.parse("TERRET_KEY=\n") == Left(KeyError.Empty),
        KeyFile.parse("TERRET_KEY=\"unterminated\n") == Left(KeyError.Malformed),
        KeyFile.parse("NOT AN ASSIGNMENT\n") == Left(KeyError.Malformed),
        KeyFile.parse("") == Left(KeyError.Malformed),
        KeyFile.parse(s"TERRET_KEY=$secret\nTERRET_KEY=$secret\n") == Left(KeyError.Malformed),
        !KeyError.Malformed.render.contains(secret),
        !KeyError.Empty.render.contains("a$b"),
      )
    },
    test("the keychain command keeps the account as one argument and drops a failed read") {
      val secret = "not-in-the-error"
      val args   = Keychain.argv("terret", "a; rm")
      assertTrue(
        args == Chunk("security", "find-generic-password", "-s", "terret", "-a", "a; rm", "-w"),
        !args.exists(_.contains(secret)),
        Keychain.interpret(44, secret + "\n") == Left(KeyError.Missing),
        Keychain.interpret(1, secret) == Left(KeyError.Missing),
        Keychain.interpret(0, secret + "\n") == Right(secret),
        Keychain.interpret(0, secret + "\n\n") == Right(secret + "\n"),
        Keychain.interpret(0, "a$b\n") == Right("a$b"),
        Keychain.interpret(0, "\n") == Left(KeyError.Empty),
        Keychain.interpret(0, "") == Left(KeyError.Empty),
        !KeyError.Missing.render.contains(secret),
        !KeyError.Empty.render.contains(secret),
      )
    },
    test("a stubbed keychain item is the same key and a miss stays typed") {
      val secret = "keychain-secret"
      val load =
        for
          _ <- stub[Keychain](_.password(any[String], any[String]))((service: String, account: String) =>
            if service == "terret" && account == "mcp" then ZIO.succeed(secret)
            else ZIO.fail(KeyError.Missing)
          )
          found  <- SecretStore.load(KeySource.Keychain("terret", "mcp"))
          missed <- SecretStore.load(KeySource.Keychain("terret", "other")).exit
        yield assertTrue(
          found.accepts(secret),
          found.toString == "EventsKey",
          !found.toString.contains(secret),
          missed == Exit.fail(KeyError.Missing),
          !KeyError.Missing.render.contains(secret),
        )
      load.provide(stubbed[Keychain])
    },
    test("a file source does not ask the keychain") {
      ZIO.scoped {
        for
          dir  <- scratch
          path <- write(dir, "tight.env", "rw-------", s"TERRET_KEY=$Secret\n")
          found <- (
            for
              _ <- stub[Keychain](_.password(any[String], any[String]))((_: String, _: String) =>
                ZIO.fail(KeyError.Unavailable)
              )
              key <- SecretStore.load(KeySource.File(path))
            yield key
          ).provide(stubbed[Keychain])
        yield assertTrue(found.accepts(Secret), found.toString == "EventsKey")
      }
    },
    test("stdio may omit the key and HTTP may not") {
      val file = Some(KeySource.File(Path.of("/var/terret/key.env")))
      assertTrue(
        KeySource.parse(Some("/var/terret/key.env"), None, None) == Right(file),
        KeySource.parse(Some("key.env"), None, None) == Left(ProcessError.KeyPath),
        KeySource.parse(None, Some("terret"), Some("mcp")) ==
          Right(Some(KeySource.Keychain("terret", "mcp"))),
        KeySource.parse(Some("/var/terret/key.env"), Some("terret"), Some("mcp")) == Left(ProcessError.KeyBoth),
        KeySource.parse(None, Some("terret"), None) == Left(ProcessError.KeyPair),
        KeySource.parse(None, None, Some("mcp")) == Left(ProcessError.KeyPair),
        KeySource.parse(Some("  "), Some(""), None) == Right(None),
        KeyGate.ready(Listen.Stdio, None) == Right(None),
        KeyGate.ready(Listen.Stdio, file) == Right(file),
        KeyGate.ready(Listen.Http("127.0.0.1", 8080), None) == Left(ProcessError.KeyMissing),
        KeyGate.ready(Listen.Http("127.0.0.1", 8080), file) == Right(file),
        ProcessError.KeyMissing.render.contains("terret.key.file"),
        !ProcessError.KeyMissing.render.contains("TERRET_EVENTS_KEY"),
        !ProcessError.KeyMissing.render.contains(Secret),
        !ProcessError.KeyBoth.render.contains(Secret),
        !ProcessError.KeyPair.render.contains(Secret),
        !ProcessError.KeyPath.render.contains(Secret),
      )
    },
  )

  private def accepts(raw: String): Boolean =
    EventsKey.from(raw).exists(_.accepts(Secret))

  private def scratch: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt {
        val root = Path.of("target")
        Files.createDirectories(root)
        Files.createTempDirectory(root, "key-spec")
      }
    )(dir => ZIO.attempt(deleteTree(dir)).ignore)

  private def write(dir: Path, name: String, mode: String, text: String): ZIO[Any, Throwable, Path] =
    ZIO.attempt {
      val path = dir.resolve(name)
      Files.writeString(path, text)
      Files.setPosixFilePermissions(path, PosixFilePermissions.fromString(mode))
      path
    }

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walked = Files.walk(root)
      try
        val paths = walked.iterator.asScala.toVector.sortBy(_.getNameCount)(using Ordering[Int].reverse)
        paths.foreach(Files.deleteIfExists(_))
      finally walked.close()
end KeySpec
