package terret.grok

import zio.{Scope, UIO, ZIO}

import java.io.{FileOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.UUID

private[grok] trait WireLog:
  def file: Path
  def size: UIO[Long]
  def askedSince(offset: Long): UIO[Boolean]
  def open: ZIO[Scope, Nothing, OutputStream]

private[grok] object WireLog:
  def make(root: Path): ZIO[Scope, Nothing, WireLog] =
    ZIO
      .acquireRelease(
        ZIO.attemptBlocking {
          Files.createDirectories(root)
          val path = root.resolve(s"wire-${UUID.randomUUID}.log")
          Files.write(path, Array.emptyByteArray)
          path
        }.orDie
      )(path => ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore)
      .map(from)

  private def from(path: Path): WireLog = new:
    def file: Path = path

    def size: UIO[Long] =
      ZIO.attemptBlocking(if Files.exists(path) then Files.size(path) else 0L).orDie

    def askedSince(offset: Long): UIO[Boolean] =
      ZIO.attemptBlocking {
        if !Files.exists(path) then false
        else
          val bytes = Files.readAllBytes(path)
          val start = math.max(0L, math.min(offset, bytes.length.toLong)).toInt
          new String(bytes, start, bytes.length - start, StandardCharsets.UTF_8)
            .contains("session/request_permission")
      }.orDie

    def open: ZIO[Scope, Nothing, OutputStream] =
      ZIO.acquireRelease(ZIO.attemptBlocking(new FileOutputStream(path.toFile, true)).orDie) { out =>
        ZIO.attemptBlocking(out.close()).ignore
      }
end WireLog
