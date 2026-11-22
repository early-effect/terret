package terret.mcp

import heddle.http.header.Headers
import heddle.mcp.events.Principal
import zio.{UIO, ZIO}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Optional `terret.events.key`. The secret stays out of `toString` and `hashCode`. */
final class EventsKey private (private val raw: String):
  override def equals(other: Any): Boolean =
    other match
      case key: EventsKey => EventsKey.same(raw, key.raw)
      case _              => false

  /** A hash of the secret would publish a low-entropy key. */
  override def hashCode: Int = 0

  override def toString: String = "EventsKey"
end EventsKey

object EventsKey:
  val Header: String = "x-terret-key"

  private val Hex = "0123456789abcdef"

  def from(raw: String): Option[EventsKey] =
    if raw.isEmpty then None else Some(new EventsKey(raw))

  def principal(configured: Option[EventsKey])(headers: Headers): UIO[Option[Principal]] =
    ZIO.succeed(identify(configured, headers.get(Header)))

  private def identify(configured: Option[EventsKey], presented: Option[String]): Option[Principal] =
    presented.filter(_.nonEmpty) match
      case None      => None
      case Some(got) =>
        val named = id(got)
        if configured.forall(key => same(key.raw, got)) then Some(named) else None

  private def id(material: String): Principal =
    Principal(s"key:${hexPrefix(sha256(material))}")

  /** Different lengths return before the bytes. */
  private def same(left: String, right: String): Boolean =
    MessageDigest.isEqual(utf8(left), utf8(right))

  private def sha256(text: String): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(utf8(text))

  private def utf8(text: String): Array[Byte] =
    text.getBytes(StandardCharsets.UTF_8)

  private def hexPrefix(bytes: Array[Byte]): String =
    bytes
      .take(4)
      .map { byte =>
        val value = byte & 0xff
        s"${Hex.charAt(value >>> 4)}${Hex.charAt(value & 0x0f)}"
      }
      .mkString
end EventsKey
