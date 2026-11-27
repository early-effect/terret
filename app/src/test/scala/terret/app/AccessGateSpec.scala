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
  private val when = Instant.parse("2026-11-21T15:04:05Z")
  private val Aud  = "aud-tag"
  private val Kid  = "k1"
  private val Iss  = "https://example.cloudflareaccess.com"

  def spec = suite("cloudflare access")(
    test("a token is RS256 for this audience and still unexpired") {
      material.map { keys =>
        val exp     = when.plusSeconds(60).getEpochSecond
        val token   = jwt(keys.signing, Some(Kid), Json.Str(Aud), exp)
        val array   = jwt(keys.signing, Some(Kid), Json.Arr(Json.Str("other"), Json.Str(Aud)), exp)
        val late    = Instant.ofEpochSecond(exp)
        val wrong   = jwt(keys.signing, Some("missing"), Json.Str(Aud), exp)
        val foreign = jwt(keys.signing, Some(Kid), Json.Str(Aud), exp, "https://evil.example")
        val round   = AccessJwt.keys(keys.jwks).flatMap { parsed =>
          check(token, parsed, Aud, when)
        }
        assertTrue(
          token.isRight,
          check(token, keys.loaded, Aud, when) == Right(()),
          check(array, keys.loaded, Aud, when) == Right(()),
          check(token, keys.loaded, "other", when) == Left(AccessError.Rejected),
          check(token, keys.loaded, Aud, late) == Left(AccessError.Rejected),
          check(foreign, keys.loaded, Aud, when) == Left(AccessError.Rejected),
          check(wrong, keys.loaded, Aud, when) == Left(AccessError.UnknownKid),
          AccessJwt.verify("a.b.c", keys.loaded, Aud, Iss, when) == Left(AccessError.Malformed),
          round == Right(()),
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
        denied <- gate.allow(Headers.empty.add("CF-Connecting-IP", "203.0.113.4")).either
        exp = when.plusSeconds(60).getEpochSecond
        raw     <- ZIO.fromEither(jwt(keys.signing, Some(Kid), Json.Str(Aud), exp))
        allowed <- gate.allow(Headers.empty.add("Cf-Access-Jwt-Assertion", raw))
        unknown <- ZIO.fromEither(jwt(keys.signing, Some("missing"), Json.Str(Aud), exp))
        first   <- gate.allow(Headers.empty.add("Cf-Access-Jwt-Assertion", unknown)).either
        once    <- hits.get
        _       <- TestClock.adjust(31.seconds)
        second  <- gate.allow(Headers.empty.add("Cf-Access-Jwt-Assertion", unknown)).either
        twice   <- hits.get
        open    <- ZIO.scoped(AccessGate.open.build.map(_.get[AccessGate]))
        passed  <- open.allow(Headers.empty.add("CF-Ray", "ray"))
      yield assertTrue(
        bare == Left(AccessError.Rejected),
        denied == Left(AccessError.Rejected),
        allowed == (),
        first == Left(AccessError.UnknownKid),
        second == Left(AccessError.UnknownKid),
        once == 1,
        twice == 2,
        passed == (),
      )
    },
    test("a refused token is logged as the reason and the path") {
      import PathDsl.*
      ZIO.scoped {
        for
          _    <- TestClock.setTime(when)
          keys <- material
          hits <- Ref.make(0)
          gate <- gate(keys, hits)
          env  <- (ZLayer.succeed(gate) ++ AuthLog.live).build
          route = Routes(Method.GET / "watch" -> Handler.succeed(Response.text("ok"))) @@ AccessGate.http
          denied   <- route(Request.get("/watch")).provideEnvironment(env)
          recorded <- env.get[AuthLog].recent
        yield assertTrue(
          denied.status == Status.Unauthorized,
          denied.body.text.contains("access denied"),
          recorded.length == 1,
          recorded.exists(row => row.reason == "Rejected" && row.path == "/watch" && row.at == when),
        )
      }
    },
  )

  private final case class Material(loaded: Chunk[AccessJwt.RsaKey], signing: PrivateKey, jwks: String)

  private def material: ZIO[Any, String, Material] =
    ZIO
      .attempt {
        val generator = KeyPairGenerator.getInstance("RSA")
        generator.initialize(2048)
        val pair = generator.generateKeyPair()
        pair.getPublic match
          case rsa: RSAPublicKey =>
            Right(Material(Chunk(AccessJwt.RsaKey(Some(Kid), rsa)), pair.getPrivate, document(rsa)))
          case _ => Left("the generated key was not RSA")
      }
      .mapError(error => Option(error.getMessage).getOrElse(error.toString))
      .flatMap(ZIO.fromEither)

  private def gate(found: Material, hits: Ref[Int]): ZIO[Any, String, AccessGate] =
    (TeamHost.from("example.cloudflareaccess.com"), Audience.from(Aud)) match
      case (Right(team), Right(tag)) =>
        val certs = new AccessCerts:
          def keys: zio.IO[AccessError, Chunk[AccessJwt.RsaKey]] =
            hits.update(_ + 1).as(found.loaded)
        ZIO.scoped((ZLayer.succeed(certs) >>> AccessGate.layer(team, tag)).build.map(_.get[AccessGate]))
      case _ => ZIO.fail("access")

  private def check(
      built: Either[String, String],
      keys: Chunk[AccessJwt.RsaKey],
      audience: String,
      now: Instant,
  ): Either[AccessError, Unit] =
    built match
      case Left(_)    => Left(AccessError.Malformed)
      case Right(raw) => AccessJwt.verify(raw, keys, audience, Iss, now)

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
  ): Either[String, String] =
    Try {
      val header = kid match
        case Some(id) => Json.Obj("alg" -> Json.Str("RS256"), "kid" -> Json.Str(id))
        case None     => Json.Obj("alg" -> Json.Str("RS256"))
      val payload = Json.Obj("aud" -> aud, "exp" -> Json.Num(BigDecimal(exp)), "iss" -> Json.Str(iss))
      val encoded = s"${url(header.toJson)}.${url(payload.toJson)}"
      val signer  = Signature.getInstance("SHA256withRSA")
      signer.initSign(key)
      signer.update(encoded.getBytes(StandardCharsets.UTF_8))
      s"$encoded.${url(signer.sign())}"
    }.toEither.left.map(error => Option(error.getMessage).getOrElse(error.toString))

  private def unsigned(value: BigInteger): String =
    val bytes = value.toByteArray
    val bare  = if bytes.nonEmpty && bytes(0) == 0 then bytes.drop(1) else bytes
    Base64.getUrlEncoder.withoutPadding.encodeToString(bare)

  private def url(raw: String): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(raw.getBytes(StandardCharsets.UTF_8))

  private def url(raw: Array[Byte]): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(raw)
end AccessGateSpec
