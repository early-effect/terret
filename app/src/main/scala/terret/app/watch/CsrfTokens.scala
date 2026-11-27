package terret.app.watch

import zio.{Ref, UIO, ULayer, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64

/** One synchronizer token per viewer. A second tab keeps the token the first page minted. */
trait CsrfTokens:
  def issue(viewer: String): UIO[String]
  def accept(viewer: String, presented: Option[String]): UIO[Boolean]

object CsrfTokens:
  def make: UIO[CsrfTokens] =
    Ref.make(Map.empty[String, String]).map { rows =>
      new:
        def issue(viewer: String): UIO[String] =
          rows.get.flatMap { current =>
            current.get(viewer) match
              case Some(token) => ZIO.succeed(token)
              case None        =>
                mint.flatMap { token =>
                  rows.modify { latest =>
                    latest.get(viewer) match
                      case Some(existing) => (existing, latest)
                      case None           => (token, latest.updated(viewer, token))
                  }
                }
          }

        def accept(viewer: String, presented: Option[String]): UIO[Boolean] =
          rows.get.map { current =>
            (current.get(viewer), presented.filter(_.nonEmpty)) match
              case (Some(stored), Some(shown)) => same(stored, shown)
              case _                           => false
          }
    }

  val layer: ULayer[CsrfTokens] = ZLayer.fromZIO(make)

  /** `SecureRandom` is the token source. A failure here is fatal: a guessed token would make the form check useless.
    */
  private def mint: UIO[String] =
    ZIO.attempt {
      val bytes = new Array[Byte](32)
      random.nextBytes(bytes)
      Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
    }.orDie

  private def same(left: String, right: String): Boolean =
    MessageDigest.isEqual(
      left.getBytes(StandardCharsets.UTF_8),
      right.getBytes(StandardCharsets.UTF_8),
    )

  private val random = new java.security.SecureRandom()
end CsrfTokens
