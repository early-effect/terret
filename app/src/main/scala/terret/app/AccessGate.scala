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

/** `port` is the tunnel listener. It is not the local MCP port. */
final case class AccessConfig(team: TeamHost, audience: Audience, port: Int)

object AccessConfig:
  val TunnelHost: String = "127.0.0.1"
  val TunnelPort: Int    = 8081

enum AccessConfigError:
  case Incomplete
  case Team
  case Audience
  case Port(value: Int)
  case SamePort(value: Int)

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

  val open: ULayer[AccessGate] =
    ZLayer.succeed(new:
      def allow(headers: Headers): IO[AccessError, Unit] = ZIO.unit)

  def layer(team: TeamHost, audience: Audience): ZLayer[AccessCerts, Nothing, AccessGate] =
    ZLayer.scoped {
      for
        certs <- ZIO.service[AccessCerts]
        cache <- Ref.make(Option.empty[(Chunk[AccessJwt.RsaKey], Instant)])
      yield Live(certs, audience, AccessJwt.issuer(team), cache)
    }

  /** The tunnel listener, on every path except `/mcp`. A missing token is rejected. There is no localhost exemption:
    * cloudflared connects from 127.0.0.1, so an exemption would let every tunnel request through. The local listener
    * does not install this, and `/mcp` keeps the events-key check.
    */
  def http: Middleware[AccessGate & AuthLog] =
    Middleware.interceptZIO { request =>
      ZIO
        .serviceWithZIO[AccessGate](_.allow(request.headers))
        .tapError { error =>
          ZIO.serviceWithZIO[AuthLog](
            _.record(AuthReason.from(error), request.path.render, AccessActor.of(request.headers))
          )
        }
        .mapError(_ => denied)
        .as(request)
    }

  private val denied: Response =
    Response.text("access denied", Status.Unauthorized)

  private final class Live(
      certs: AccessCerts,
      audience: Audience,
      issuer: String,
      cache: Ref[Option[(Chunk[AccessJwt.RsaKey], Instant)]],
  ) extends AccessGate:
    def allow(headers: Headers): IO[AccessError, Unit] =
      token(headers) match
        case None      => ZIO.fail(AccessError.Rejected)
        case Some(jwt) =>
          Clock.instant.flatMap { now =>
            load(force = false).flatMap { keys =>
              check(jwt, keys, now).catchSome { case AccessError.UnknownKid =>
                load(force = true).flatMap(fresh => check(jwt, fresh, now))
              }
            }
          }

    private def check(jwt: String, keys: Chunk[AccessJwt.RsaKey], now: Instant): IO[AccessError, Unit] =
      ZIO.fromEither(AccessJwt.verify(jwt, keys, audience.value, issuer, now))

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
end AccessGate
