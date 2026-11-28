package terret.app

import zio.json.*
import zio.json.ast.Json
import zio.Chunk

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.spec.RSAPublicKeySpec
import java.security.{KeyFactory, PublicKey, Signature}
import java.time.Instant
import java.util.{Base64, Locale}
import scala.util.Try

/** Checks a Cloudflare Access JWT: RS256 against the team keys, then issuer, audience, and expiry. Optional `nbf` and
  * `iat` are then bounded by `ClockSkewSeconds`. The owner email is matched after that, ignoring case. A missing or
  * different email is rejected. A service token, which carries `common_name` and no email, is rejected. `common_name`
  * is not read.
  */
object AccessJwt:
  /** Seconds `nbf` and `iat` may sit ahead of now. Further ahead is rejected. A missing claim is allowed. */
  val ClockSkewSeconds: Long = 60

  final case class RsaKey(kid: Option[String], publicKey: PublicKey)

  /** Issuer for a team hostname, such as `https://example.cloudflareaccess.com`. */
  def issuer(team: TeamHost): String = s"https://${team.value}"

  /** A map key for the stream cap. A `sub` claim is hashed, and so is the token when `sub` is absent. Neither value is
    * kept.
    */
  def viewer(token: String): String =
    val material = subject(token).filter(_.nonEmpty).getOrElse(token)
    digest(material)

  def verify(
      token: String,
      keys: Chunk[RsaKey],
      audience: String,
      issuer: String,
      owner: OwnerEmail,
      now: Instant,
  ): Either[AccessError, Unit] =
    val parts = token.split("\\.", -1)
    if parts.length != 3 || parts.exists(_.isEmpty) then Left(AccessError.Malformed)
    else
      for
        header  <- obj(parts(0))
        payload <- obj(parts(1))
        sig     <- b64(parts(2))
        alg     <- text(header, "alg")
        _       <- if alg == "RS256" then Right(()) else Left(AccessError.Rejected)
        key     <- select(keys, optional(header, "kid"))
        _       <- if signed(parts(0), parts(1), sig, key.publicKey) then Right(()) else Left(AccessError.Rejected)
        iss     <- text(payload, "iss")
        _       <- if iss == issuer then Right(()) else Left(AccessError.Rejected)
        _       <- audienceOf(payload, audience)
        exp     <- epoch(payload, "exp")
        _       <- if now.isBefore(Instant.ofEpochSecond(exp)) then Right(()) else Left(AccessError.Rejected)
        _       <- futureBound(payload, "nbf", now)
        _       <- futureBound(payload, "iat", now)
        _       <- ownerOf(payload, owner)
      yield ()

  def keys(json: String): Either[AccessError, Chunk[RsaKey]] =
    json.fromJson[Jwks] match
      case Left(_)     => Left(AccessError.Malformed)
      case Right(jwks) =>
        jwks.keys.foldLeft[Either[AccessError, Chunk[RsaKey]]](Right(Chunk.empty)) {
          case (Left(error), _)  => Left(error)
          case (Right(acc), jwk) =>
            if jwk.kty != "RSA" || jwk.alg.exists(_ != "RS256") then Right(acc)
            else keyOf(jwk).map(acc :+ _)
        }

  private def keyOf(jwk: Jwk): Either[AccessError, RsaKey] =
    for
      modulus  <- b64(jwk.n)
      exponent <- b64(jwk.e)
      public   <- Try {
        val spec = new RSAPublicKeySpec(new BigInteger(1, modulus), new BigInteger(1, exponent))
        KeyFactory.getInstance("RSA").generatePublic(spec)
      }.toEither.left.map(_ => AccessError.Malformed)
    yield RsaKey(jwk.kid, public)

  /** `Signature.verify` throws on a key the provider rejects. That failure is a rejected token, not a crash. */
  private def signed(header: String, payload: String, sig: Array[Byte], key: PublicKey): Boolean =
    Try {
      val signature = Signature.getInstance("SHA256withRSA")
      signature.initVerify(key)
      signature.update(s"$header.$payload".getBytes(StandardCharsets.UTF_8))
      signature.verify(sig)
    }.getOrElse(false)

  private def select(keys: Chunk[RsaKey], kid: Option[String]): Either[AccessError, RsaKey] =
    kid match
      case Some(id) =>
        keys.find(_.kid.contains(id)) match
          case Some(key) => Right(key)
          case None      => Left(AccessError.UnknownKid)
      case None =>
        keys.toList match
          case key :: Nil => Right(key)
          case _          => Left(AccessError.UnknownKid)

  private def ownerOf(payload: Json.Obj, owner: OwnerEmail): Either[AccessError, Unit] =
    optional(payload, "email") match
      case Some(found) if mailbox(found) == mailbox(owner.value) => Right(())
      case _                                                     => Left(AccessError.Rejected)

  /** `Locale.ROOT` so case folding does not follow the JVM default locale. The whole address is folded. */
  private def mailbox(raw: String): String =
    raw.toLowerCase(Locale.ROOT)

  /** A missing claim is allowed. A present claim must be an epoch second no further ahead than the skew. */
  private def futureBound(payload: Json.Obj, name: String, now: Instant): Either[AccessError, Unit] =
    payload.get(name) match
      case None    => Right(())
      case Some(_) =>
        epoch(payload, name).flatMap { seconds =>
          if seconds <= now.getEpochSecond + ClockSkewSeconds then Right(())
          else Left(AccessError.Rejected)
        }

  private def audienceOf(payload: Json.Obj, expected: String): Either[AccessError, Unit] =
    payload.get("aud") match
      case Some(Json.Str(value)) if value == expected                                    => Right(())
      case Some(Json.Arr(values)) if values.exists(named => named == Json.Str(expected)) => Right(())
      case _                                                                             => Left(AccessError.Rejected)

  private def epoch(obj: Json.Obj, name: String): Either[AccessError, Long] =
    obj.get(name) match
      case Some(Json.Num(number)) =>
        val whole = number.stripTrailingZeros
        if whole.scale > 0 then Left(AccessError.Malformed)
        else
          val integer = whole.toBigInteger
          if integer.signum >= 0 && integer.bitLength < 63 then Right(integer.longValue)
          else Left(AccessError.Malformed)
      case _ => Left(AccessError.Malformed)

  private def obj(part: String): Either[AccessError, Json.Obj] =
    b64(part).flatMap { bytes =>
      new String(bytes, StandardCharsets.UTF_8).fromJson[Json] match
        case Right(found: Json.Obj) => Right(found)
        case _                      => Left(AccessError.Malformed)
    }

  private def text(obj: Json.Obj, name: String): Either[AccessError, String] =
    optional(obj, name) match
      case Some(value) => Right(value)
      case None        => Left(AccessError.Malformed)

  private def optional(obj: Json.Obj, name: String): Option[String] =
    obj.get(name).collect { case Json.Str(value) if value.nonEmpty => value }

  private def subject(token: String): Option[String] =
    val parts = token.split("\\.", -1)
    if parts.length != 3 then None
    else obj(parts(1)).toOption.flatMap(payload => optional(payload, "sub"))

  private def digest(raw: String): String =
    Try {
      val md = java.security.MessageDigest.getInstance("SHA-256")
      md.digest(raw.getBytes(StandardCharsets.UTF_8))
        .map(byte => f"${byte & 0xff}%02x")
        .take(8)
        .mkString
    }.getOrElse("anonymous")

  private def b64(raw: String): Either[AccessError, Array[Byte]] =
    val normalized = raw.replace('-', '+').replace('_', '/')
    val padded     = normalized + ("=" * ((4 - normalized.length % 4) % 4))
    Try(Base64.getDecoder.decode(padded)).toEither.left.map(_ => AccessError.Malformed)

  private final case class Jwk(
      kty: String,
      kid: Option[String],
      n: String,
      e: String,
      alg: Option[String] = None,
  ) derives JsonDecoder

  private final case class Jwks(keys: Chunk[Jwk]) derives JsonDecoder
end AccessJwt

enum AccessError:
  case Rejected
  case Certs
  case Malformed
  case UnknownKid
