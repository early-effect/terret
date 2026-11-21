package terret.grok

import zio.test.*
import zio.ZIO

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.{Comparator, UUID}

object WireLogSpec extends ZIOSpecDefault:
  def spec = suite("wire log")(
    test("two captures keep their own bytes and cursors") {
      ZIO.scoped {
        for
          root   <- directory
          first  <- WireLog.make(root)
          second <- WireLog.make(root)
          mark   <- first.size
          _      <- write(second, "session/request_permission\n")
          _      <- write(first, "other\n")
          own    <- first.askedSince(mark)
          other  <- second.askedSince(0L)
          left   <- text(first)
          right  <- text(second)
        yield assertTrue(
          first.file != second.file,
          !own,
          other,
          left == "other\n",
          !left.contains("session/request_permission"),
          right.contains("session/request_permission"),
          !right.contains("other"),
        )
      }
    }
  )

  private def directory =
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        val dir = Path.of("target", "wire-log", UUID.randomUUID.toString)
        Files.createDirectories(dir)
        dir
      }.orDie
    )(delete)

  private def write(wire: WireLog, body: String) =
    ZIO.scoped {
      wire.open.flatMap { out =>
        ZIO.attemptBlocking {
          out.write(body.getBytes(StandardCharsets.UTF_8))
          out.flush()
        }.orDie
      }
    }

  private def text(wire: WireLog) =
    ZIO.attemptBlocking(Files.readString(wire.file)).orDie

  private def delete(dir: Path) =
    ZIO.attemptBlocking {
      if Files.exists(dir) then
        val walk = Files.walk(dir)
        try
          val paths = walk.sorted(Comparator.reverseOrder).iterator()
          while paths.hasNext do Files.deleteIfExists(paths.next())
        finally walk.close()
    }.ignore
end WireLogSpec
