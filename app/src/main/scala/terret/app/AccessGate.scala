package terret.app

import heddle.client.Client
import heddle.http.{Response, Status}
import heddle.http.header.Headers
import heddle.route.Middleware
import zio.{Chunk, Clock, IO, Ref, ULayer, ZIO, ZLayer}

import java.time.Instant

/** Team hostname, such as `example.cloudflareaccess.com`. No scheme and no path. */
opaque type TeamHost = String

object TeamHost:
  def from(raw: String): Either[AccessConfigError, TeamHost] =
    val host = raw.trim
    if host.isEmpty || host.contains("://") || host.contains('/') || host.exists(_.isWhitespace) || !host.contains('.')
    then Left(AccessConfigError.Team)
    else Right(host)

  extension (host: TeamHost) def value: String = host
end TeamHost

/** Cloudflare Access application audience tag. */
opaque type Audience = String

object Audience:
  def from(raw: String): Either[AccessConfigError, Audience] =
    val tag = raw.trim
    if tag.isEmpty || tag.exists(_.isWhitespace) then Left(AccessConfigError.Audience) else Right(tag)

  extension (audience: Audience) def value: String = audience
end Audience

final case class AccessConfig(team: TeamHost, audience: Audience)

enum AccessConfigError:
  case Incomplete
  case Team
  case Audience

/** Loads the team JWKS. The live load is the only network call, and tests supply the keys. */
trait AccessCerts:
  def keys: IO[AccessError, Chunk[AccessJwt.RsaKey]]

object AccessCerts:
  def fixed(loaded: Chunk[AccessJwt.RsaKey]): AccessCerts = new:
    def keys: IO[AccessError, Chunk[AccessJwt.RsaKey]] = ZIO.succeed(loaded)

  def remote(team: TeamHost): AccessCerts = new:
    def keys: IO[AccessError, Chunk[AccessJwt.RsaKey]] =
      Client.get(s"https://${team.value}/cdn-cgi/access/certs").mapError(_ => AccessError.Certs).flatMap { response =>
        response.body.utf8.mapError(_ => AccessError.Certs).flatMap { body =>
          if response.status.isSuccess then ZIO.fromEither(AccessJwt.keys(body))
          else ZIO.fail(AccessError.Certs)
        }
      }
end AccessCerts

/** Allows a request, or rejects it. Unconfigured, every request is allowed. */
trait AccessGate:
  def allow(headers: Headers): IO[AccessError, Unit]

object AccessGate:
  private val Assertion = "cf-access-jwt-assertion"
  private val Proxied   = List("cf-connecting-ip", "cf-ray", "cf-access-authenticated-user-email")

  val open: ULayer[AccessGate] =
    ZLayer.succeed(new:
      def allow(headers: Headers): IO[AccessError, Unit] = ZIO.unit)

  def layer(audience: Audience): ZLayer[AccessCerts, Nothing, AccessGate] =
    ZLayer.scoped {
      for
        certs <- ZIO.service[AccessCerts]
        cache <- Ref.make(Option.empty[(Chunk[AccessJwt.RsaKey], Instant)])
      yield Live(certs, audience, cache)
    }

  /** HTTP routes only. A direct client with no Cloudflare headers is allowed. A presented token is always checked. */
  def http: Middleware[AccessGate] =
    Middleware.interceptZIO { request =>
      ZIO.serviceWithZIO[AccessGate](_.allow(request.headers)).mapError(_ => denied).as(request)
    }

  private val denied: Response =
    Response.text("access denied", Status.Unauthorized)

  private final class Live(
      certs: AccessCerts,
      audience: Audience,
      cache: Ref[Option[(Chunk[AccessJwt.RsaKey], Instant)]],
  ) extends AccessGate:
    def allow(headers: Headers): IO[AccessError, Unit] =
      token(headers) match
        case None =>
          if proxied(headers) then ZIO.fail(AccessError.Rejected) else ZIO.unit
        case Some(jwt) =>
          Clock.instant.flatMap { now =>
            load(force = false).flatMap { keys =>
              check(jwt, keys, now).catchSome { case AccessError.UnknownKid =>
                load(force = true).flatMap(fresh => check(jwt, fresh, now))
              }
            }
          }

    private def check(jwt: String, keys: Chunk[AccessJwt.RsaKey], now: Instant): IO[AccessError, Unit] =
      ZIO.fromEither(AccessJwt.verify(jwt, keys, audience.value, now))

    /** An unknown kid refreshes once. A cache younger than 30 seconds stays, so a bad kid cannot stampede the certs. */
    private def load(force: Boolean): IO[AccessError, Chunk[AccessJwt.RsaKey]] =
      Clock.instant.flatMap { now =>
        cache.get.flatMap {
          case Some((keys, at)) if !force || !now.isAfter(at.plusSeconds(30)) => ZIO.succeed(keys)
          case _                                                              =>
            certs.keys.tap(found => cache.set(Some(found -> now)))
        }
      }
  end Live

  private def token(headers: Headers): Option[String] =
    headers.get(Assertion).filter(_.nonEmpty)

  private def proxied(headers: Headers): Boolean =
    Proxied.exists(name => headers.get(name).exists(_.nonEmpty))
end AccessGate
