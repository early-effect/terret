package terret.app

import heddle.http.{Method, Request, Response, Status}
import heddle.http.header.Headers
import heddle.route.{Handler, PathDsl, Routes}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Ref, ZIO, ZLayer}
import zio.durationInt

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.interfaces.RSAPublicKey
import java.security.{KeyPairGenerator, PrivateKey, Signature}
import java.time.Instant
import java.util.Base64
import scala.util.Try

object AccessGateSpec extends ZIOSpecDefault:
  private val when  = Instant.parse("2026-11-21T15:04:05Z")
  private val Aud   = "aud-tag"
  private val Kid   = "k1"
  private val Iss   = "https://example.cloudflareaccess.com"
  private val Owner = "owner@example.com"

  def spec = suite("cloudflare access")(
    test("a token is RS256 for this audience and still unexpired") {
      material.map { keys =>
        val exp   = when.plusSeconds(60).getEpochSecond
        val token = jwt(keys.signing, Some(Kid), Json.Str(Aud), exp)
        val array = jwt(
          keys.signing,
          Some(Kid),
          Json.Arr(Json.Str("other"), Json.Str(Aud)),
          exp,
        )
        val late    = Instant.ofEpochSecond(exp)
        val wrong   = jwt(keys.signing, Some("missing"), Json.Str(Aud), exp)
        val foreign = jwt(
          keys.signing,
          Some(Kid),
          Json.Str(Aud),
          exp,
          "https://evil.example",
        )
        val round = AccessJwt.keys(keys.jwks).flatMap { parsed =>
          check(token, parsed, Aud, when)
        }
        val tampered = tamper(token)
        assertTrue(
          token.isRight,
          check(token, keys.loaded, Aud, when) == Right(()),
          check(array, keys.loaded, Aud, when) == Right(()),
          check(token, keys.loaded, "other", when) == Left(AccessError.Rejected),
        ) &&
        assertTrue(
          check(token, keys.loaded, Aud, late) == Left(AccessError.Rejected),
          check(foreign, keys.loaded, Aud, when) == Left(AccessError.Rejected),
          check(tampered, keys.loaded, Aud, when) == Left(AccessError.Rejected),
          check(wrong, keys.loaded, Aud, when) == Left(AccessError.UnknownKid),
        ) &&
        assertTrue(
          verified("a.b.c", keys.loaded, Aud, when) == Left(
            AccessError.Malformed
          ),
          round == Right(()),
        )
      }
    },
    test("nbf beyond the skew is rejected, and a past or in-skew nbf is accepted") {
      material.map { keys =>
        val exp   = when.plusSeconds(3600).getEpochSecond
        val now   = when.getEpochSecond
        val skew  = AccessJwt.ClockSkewSeconds
        val ahead = skew + 1
        assertTrue(
          timed(keys, exp, nbf = Some(seconds(now + ahead))) == Left(AccessError.Rejected),
          timed(keys, exp, nbf = Some(seconds(now + skew))) == Right(()),
          timed(keys, exp, nbf = Some(seconds(now - 30))) == Right(()),
          timed(keys, exp, nbf = Some(seconds(now + 1))) == Right(()),
        )
      }
    },
    test("iat beyond the skew is rejected, and a past or in-skew iat is accepted") {
      material.map { keys =>
        val exp   = when.plusSeconds(3600).getEpochSecond
        val now   = when.getEpochSecond
        val skew  = AccessJwt.ClockSkewSeconds
        val ahead = skew + 1
        assertTrue(
          timed(keys, exp, iat = Some(seconds(now + ahead))) == Left(AccessError.Rejected),
          timed(keys, exp, iat = Some(seconds(now + skew))) == Right(()),
          timed(keys, exp, iat = Some(seconds(now - 30))) == Right(()),
          timed(keys, exp, iat = Some(seconds(now + 1))) == Right(()),
        )
      }
    },
    test("a non-integer nbf or iat is malformed") {
      material.map { keys =>
        val exp = when.plusSeconds(3600).getEpochSecond
        assertTrue(
          timed(keys, exp, nbf = Some(Json.Num(BigDecimal("1.5")))) == Left(AccessError.Malformed),
          timed(keys, exp, iat = Some(Json.Str("1"))) == Left(AccessError.Malformed),
        )
      }
    },
    test("the owner email matches when only the case differs") {
      material.map { keys =>
        val exp        = when.plusSeconds(60).getEpochSecond
        val local      = addressed(keys, exp, "Owner@example.com")
        val domain     = addressed(keys, exp, "owner@Example.com")
        val both       = addressed(keys, exp, "Owner@Example.COM")
        val configured = addressedTo(keys, exp, "owner@example.com", "Owner@Example.COM")
        assertTrue(
          local == Right(()),
          domain == Right(()),
          both == Right(()),
          configured == Right(()),
        )
      }
    },
    test("expiry and a bad issuer still fail when nbf, iat, and the email are acceptable") {
      material.map { keys =>
        val exp     = when.plusSeconds(3600).getEpochSecond
        val stamp   = Some(seconds(when.getEpochSecond))
        val late    = Instant.ofEpochSecond(exp)
        val foreign = jwt(
          keys.signing,
          Some(Kid),
          Json.Str(Aud),
          exp,
          "https://evil.example",
          nbf = stamp,
          iat = stamp,
        )
        val expired = jwt(keys.signing, Some(Kid), Json.Str(Aud), exp, nbf = stamp, iat = stamp)
        assertTrue(
          check(foreign, keys.loaded, Aud, when) == Left(AccessError.Rejected),
          check(expired, keys.loaded, Aud, late) == Left(AccessError.Rejected),
          check(tamper(expired), keys.loaded, Aud, when) == Left(AccessError.Rejected),
        )
      }
    },
    test("a missing token is rejected, with or without a cloudflare header") {
      for
        _      <- TestClock.setTime(when)
        keys   <- material
        hits   <- Ref.make(0)
        gate   <- gate(keys, hits)
        bare   <- gate.allow(Headers.empty).either
        denied <- gate
          .allow(Headers.empty.add("CF-Connecting-IP", "203.0.113.4"))
          .either
        exp = when.plusSeconds(60).getEpochSecond
        raw     <- ZIO.fromEither(jwt(keys.signing, Some(Kid), Json.Str(Aud), exp))
        allowed <- gate.allow(Headers.empty.add("Cf-Access-Jwt-Assertion", raw))
        unknown <- ZIO.fromEither(
          jwt(keys.signing, Some("missing"), Json.Str(Aud), exp)
        )
        first <- gate
          .allow(Headers.empty.add("Cf-Access-Jwt-Assertion", unknown))
          .either
        once   <- hits.get
        _      <- TestClock.adjust(31.seconds)
        second <- gate
          .allow(Headers.empty.add("Cf-Access-Jwt-Assertion", unknown))
          .either
        twice  <- hits.get
        open   <- ZIO.scoped(AccessGate.open.build.map(_.get[AccessGate]))
        passed <- open.allow(Headers.empty.add("CF-Ray", "ray"))
      yield assertTrue(
        bare == Left(AccessError.Rejected),
        denied == Left(AccessError.Rejected),
        allowed == (),
        first == Left(AccessError.UnknownKid),
      ) &&
        assertTrue(
          second == Left(AccessError.UnknownKid),
          once == 1,
          twice == 2,
          passed == (),
        )
    },
    test("a service token, a wrong email, and a missing email are rejected") {
      import PathDsl.*
      val other = "other@example.com"
      ZIO.scoped {
        for
          _    <- TestClock.setTime(when)
          keys <- material
          hits <- Ref.make(0)
          gate <- gate(keys, hits)
          env  <- (ZLayer.succeed(gate) ++ AuthLog.live).build
          route = Routes(
            Method.GET / "watch" -> Handler.succeed(Response.text("ok"))
          ) @@ AccessGate.http
          exp = when.plusSeconds(60).getEpochSecond
          service <- ZIO.fromEither(
            jwt(
              keys.signing,
              Some(Kid),
              Json.Str(Aud),
              exp,
              email = None,
              commonName = Some("ci-bot"),
            )
          )
          wrong <- ZIO.fromEither(
            jwt(
              keys.signing,
              Some(Kid),
              Json.Str(Aud),
              exp,
              email = Some(other),
            )
          )
          missing <- ZIO.fromEither(
            jwt(keys.signing, Some(Kid), Json.Str(Aud), exp, email = None)
          )
          serviceDenied <- route(
            Request.get("/watch").withHeader("cf-access-jwt-assertion", service)
          )
            .provideEnvironment(env)
          wrongDenied <- route(
            Request.get("/watch").withHeader("cf-access-jwt-assertion", wrong)
          )
            .provideEnvironment(env)
          missingDenied <- route(
            Request.get("/watch").withHeader("cf-access-jwt-assertion", missing)
          )
            .provideEnvironment(env)
          recorded <- env.get[AuthLog].recent
        yield assertTrue(
          serviceDenied.status == Status.Unauthorized,
          wrongDenied.status == Status.Unauthorized,
          missingDenied.status == Status.Unauthorized,
          recorded.length == 3,
        ) &&
          assertTrue(
            recorded.forall(_.reason == "Rejected"),
            recorded.forall(row => !row.toString.contains(service)),
            recorded.forall(row => !row.toString.contains(wrong)),
            recorded.forall(row => !row.toString.contains(other) && !row.toString.contains("ci-bot")),
          )
      }
    },
    test("a refused token is logged as the reason and the path") {
      import PathDsl.*
      val garbage = "not-a-jwt"
      ZIO.scoped {
        for
          _    <- TestClock.setTime(when)
          keys <- material
          hits <- Ref.make(0)
          gate <- gate(keys, hits)
          env  <- (ZLayer.succeed(gate) ++ AuthLog.live).build
          route = Routes(
            Method.GET / "watch" -> Handler.succeed(Response.text("ok"))
          ) @@ AccessGate.http
          denied <- route(Request.get("/watch")).provideEnvironment(env)
          forged <- route(
            Request
              .get("/watch/forged")
              .withHeader("cf-access-jwt-assertion", garbage)
          )
            .provideEnvironment(env)
          recorded <- env.get[AuthLog].recent
        yield assertTrue(
          denied.status == Status.Unauthorized,
          denied.body.text.contains("access denied"),
          forged.status == Status.Unauthorized,
          forged.body.text.contains("access denied"),
        ) &&
          assertTrue(
            recorded.length == 2,
            recorded.exists(row =>
              row.reason == "Rejected" && row.path == "/watch" && row.at == when && row.actor.isEmpty
            ),
            recorded.exists(row =>
              row.reason == "Malformed" &&
                row.path == "/watch/forged" &&
                row.at == when &&
                row.actor.contains(AccessJwt.viewer(garbage))
            ),
          )
      }
    },
  )

  private final case class Material(
      loaded: Chunk[AccessJwt.RsaKey],
      signing: PrivateKey,
      jwks: String,
  )

  private def material: ZIO[Any, String, Material] =
    ZIO
      .attempt {
        val generator = KeyPairGenerator.getInstance("RSA")
        generator.initialize(2048)
        val pair = generator.generateKeyPair()
        pair.getPublic match
          case rsa: RSAPublicKey =>
            Right(
              Material(
                Chunk(AccessJwt.RsaKey(Some(Kid), rsa)),
                pair.getPrivate,
                document(rsa),
              )
            )
          case _ => Left("the generated key was not RSA")
      }
      .mapError(error => Option(error.getMessage).getOrElse(error.toString))
      .flatMap(ZIO.fromEither)

  private def gate(
      found: Material,
      hits: Ref[Int],
  ): ZIO[Any, String, AccessGate] =
    (
      TeamHost.from("example.cloudflareaccess.com"),
      Audience.from(Aud),
      OwnerEmail.from(Owner),
    ) match
      case (Right(team), Right(tag), Right(owner)) =>
        val certs = new AccessCerts:
          def keys: zio.IO[AccessError, Chunk[AccessJwt.RsaKey]] =
            hits.update(_ + 1).as(found.loaded)
        ZIO.scoped(
          (ZLayer.succeed(certs) >>> AccessGate.layer(team, tag, owner)).build
            .map(_.get[AccessGate])
        )
      case _ => ZIO.fail("access")

  private def check(
      built: Either[String, String],
      keys: Chunk[AccessJwt.RsaKey],
      audience: String,
      now: Instant,
  ): Either[AccessError, Unit] =
    built match
      case Left(_)    => Left(AccessError.Malformed)
      case Right(raw) => verified(raw, keys, audience, now)

  private def verified(
      raw: String,
      keys: Chunk[AccessJwt.RsaKey],
      audience: String,
      now: Instant,
  ): Either[AccessError, Unit] =
    OwnerEmail.from(Owner) match
      case Left(_)      => Left(AccessError.Malformed)
      case Right(owner) =>
        AccessJwt.verify(raw, keys, audience, Iss, owner, now)

  private def timed(
      keys: Material,
      exp: Long,
      nbf: Option[Json] = None,
      iat: Option[Json] = None,
  ): Either[AccessError, Unit] =
    check(
      jwt(keys.signing, Some(Kid), Json.Str(Aud), exp, nbf = nbf, iat = iat),
      keys.loaded,
      Aud,
      when,
    )

  private def addressed(keys: Material, exp: Long, email: String): Either[AccessError, Unit] =
    addressedTo(keys, exp, email, Owner)

  private def addressedTo(
      keys: Material,
      exp: Long,
      email: String,
      owner: String,
  ): Either[AccessError, Unit] =
    (jwt(keys.signing, Some(Kid), Json.Str(Aud), exp, email = Some(email)), OwnerEmail.from(owner)) match
      case (Right(raw), Right(who)) => AccessJwt.verify(raw, keys.loaded, Aud, Iss, who, when)
      case _                        => Left(AccessError.Malformed)

  private def seconds(value: Long): Json = Json.Num(BigDecimal(value))

  /** Flips one signature character. The owner email is unchanged, and the signature check still fails.
    */
  private def tamper(token: Either[String, String]): Either[String, String] =
    token.map { raw =>
      val at = raw.lastIndexOf('.')
      if at < 0 || at >= raw.length - 1 then s"${raw}A"
      else
        val end     = raw.charAt(raw.length - 1)
        val flipped = if end == 'A' then 'B' else 'A'
        s"${raw.dropRight(1)}$flipped"
    }

  private def document(rsa: RSAPublicKey): String =
    Json
      .Obj(
        "keys" -> Json.Arr(
          Json.Obj(
            "kty" -> Json.Str("RSA"),
            "kid" -> Json.Str(Kid),
            "alg" -> Json.Str("RS256"),
            "n"   -> Json.Str(unsigned(rsa.getModulus)),
            "e"   -> Json.Str(unsigned(rsa.getPublicExponent)),
          )
        )
      )
      .toJson

  private def jwt(
      key: PrivateKey,
      kid: Option[String],
      aud: Json,
      exp: Long,
      iss: String = Iss,
      email: Option[String] = Some(Owner),
      commonName: Option[String] = None,
      nbf: Option[Json] = None,
      iat: Option[Json] = None,
  ): Either[String, String] =
    Try {
      val header = kid match
        case Some(id) =>
          Json.Obj("alg" -> Json.Str("RS256"), "kid" -> Json.Str(id))
        case None => Json.Obj("alg" -> Json.Str("RS256"))
      val payload = claims(aud, exp, iss, email, commonName, nbf, iat)
      val encoded = s"${url(header.toJson)}.${url(payload.toJson)}"
      val signer  = Signature.getInstance("SHA256withRSA")
      signer.initSign(key)
      signer.update(encoded.getBytes(StandardCharsets.UTF_8))
      s"$encoded.${url(signer.sign())}"
    }.toEither.left.map(error => Option(error.getMessage).getOrElse(error.toString))

  private def claims(
      aud: Json,
      exp: Long,
      iss: String,
      email: Option[String],
      commonName: Option[String],
      nbf: Option[Json],
      iat: Option[Json],
  ): Json.Obj =
    val base = Chunk[(String, Json)](
      "aud" -> aud,
      "exp" -> Json.Num(BigDecimal(exp)),
      "iss" -> Json.Str(iss),
    )
    val withEmail =
      email.fold(base)(value => base :+ ("email" -> Json.Str(value)))
    val withNbf = nbf.fold(withEmail)(value => withEmail :+ ("nbf" -> value))
    val withIat = iat.fold(withNbf)(value => withNbf :+ ("iat" -> value))
    val fields  = commonName.fold(withIat)(value => withIat :+ ("common_name" -> Json.Str(value)))
    Json.Obj(fields*)

  private def unsigned(value: BigInteger): String =
    val bytes = value.toByteArray
    val bare  = if bytes.nonEmpty && bytes(0) == 0 then bytes.drop(1) else bytes
    Base64.getUrlEncoder.withoutPadding.encodeToString(bare)

  private def url(raw: String): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(
      raw.getBytes(StandardCharsets.UTF_8)
    )

  private def url(raw: Array[Byte]): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(raw)
end AccessGateSpec
