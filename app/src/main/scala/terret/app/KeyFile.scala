package terret.app

import zio.{IO, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path}
import java.nio.file.attribute.{PosixFileAttributes, PosixFilePermission}
import scala.jdk.CollectionConverters.*

/** A mode 600 env file. `TERRET_KEY` is read from the file, never from the process environment. */
object KeyFile:
  private val Name = "TERRET_KEY"

  def load(path: Path): IO[KeyError, String] =
    read(path).flatMap(text => ZIO.fromEither(parse(text)))

  /** Group, other, and owner-execute are refused. Owner read, with or without owner write, is kept. */
  private def read(path: Path): IO[KeyError, String] =
    ZIO
      .attemptBlockingInterrupt {
        val attrs = Files.readAttributes(path, classOf[PosixFileAttributes], LinkOption.NOFOLLOW_LINKS)
        if attrs.isSymbolicLink || !attrs.isRegularFile then Left(KeyError.Unreadable)
        else if loose(attrs.permissions) then Left(KeyError.Exposed)
        else Right(Files.readString(path, StandardCharsets.UTF_8))
      }
      .mapError(_ => KeyError.Unreadable)
      .flatMap(ZIO.fromEither)

  private def loose(permissions: java.util.Set[PosixFilePermission]): Boolean =
    permissions.asScala.exists {
      case PosixFilePermission.OWNER_READ | PosixFilePermission.OWNER_WRITE => false
      case _                                                                => true
    }

  /** One `TERRET_KEY`. A matching quote layer is removed. `$` stays literal. Other names are ignored. */
  def parse(text: String): Either[KeyError, String] =
    text.linesIterator
      .foldLeft[Either[KeyError, Option[String]]](Right(None)) { (acc, line) =>
        acc.flatMap(found => step(found, line))
      }
      .flatMap {
        case Some(secret) => Right(secret)
        case None         => Left(KeyError.Malformed)
      }

  private def step(found: Option[String], line: String): Either[KeyError, Option[String]] =
    val trimmed = line.stripSuffix("\r").trim
    if trimmed.isEmpty || trimmed.startsWith("#") then Right(found)
    else
      assignment(trimmed) match
        case None                 => Left(KeyError.Malformed)
        case Some((name, raw)) =>
          if name != Name then Right(found)
          else if found.nonEmpty then Left(KeyError.Malformed)
          else quoted(raw).map(Some(_))

  private def assignment(line: String): Option[(String, String)] =
    val body =
      if line.startsWith("export ") then line.stripPrefix("export ").trim
      else line
    val split = body.indexOf('=')
    if split <= 0 then None
    else Some((body.take(split).trim, body.drop(split + 1)))

  private def quoted(value: String): Either[KeyError, String] =
    if value.isEmpty then Left(KeyError.Empty)
    else
      val mark = value.charAt(0)
      if mark == '"' || mark == '\'' then
        if value.length >= 2 && value.charAt(value.length - 1) == mark then
          val inner = value.substring(1, value.length - 1)
          if inner.isEmpty then Left(KeyError.Empty) else Right(inner)
        else Left(KeyError.Malformed)
      else Right(value)
end KeyFile
