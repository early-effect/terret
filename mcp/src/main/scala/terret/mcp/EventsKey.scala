package terret.mcp

import heddle.http.header.Headers
import heddle.mcp.events.Principal
import zio.{UIO, ZIO}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** The one configured key. `toString` and `hashCode` do not reveal it. Stdio has no headers, so HTTP is what checks
  * it.
  */
final class EventsKey private (private val raw: String):
  def accepts(presented: String): Boolean =
    presented.nonEmpty && EventsKey.same(raw, presented)

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
        configured match
          case Some(key) if key.accepts(got) => Some(id(got))
          case _                             => None

  private def id(material: String): Principal =
    Principal(s"key:${hexPrefix(sha256(material))}")

  /** Both sides are hashed first so a shorter header does not return early. */
  private def same(left: String, right: String): Boolean =
    MessageDigest.isEqual(sha256(left), sha256(right))

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
