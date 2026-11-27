package terret.mcp

import heddle.http.header.Headers
import heddle.mcp.events.{Principal, WebhookTransport}
import heddle.mcp.protocol.{ProtocolVersion, RequestMeta, RpcError}
import terret.Terret
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.Milestones
import zio.json.ast.Json
import zio.test.*
import zio.ZIO

import java.time.Instant
import java.util.Base64

object EventsKeySpec extends ZIOSpecDefault:
  private val Fips       = "abc"
  private val FipsId     = "key:ba7816bf"
  private val Secret     = "terret-events-key"
  private val Wrong      = "terret-events-kez"
  private val when       = Instant.parse("2026-11-21T15:04:05Z")
  private val refreshAt  = when.plusMillis(EventPolicy.terret.default.toMillis).toString
  private val Url        = "https://hooks.example/events"
  private val WebhookKey = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(3.toByte))

  def spec = suite("events key")(
    test("a configured key accepts only an exact header and the id is a hash prefix") {
      val configured = EventsKey.from(Fips)
      val headers    = Headers.empty
      for
        matched <- EventsKey.principal(configured)(headers.add(EventsKey.Header, Fips))
        cased   <- EventsKey.principal(configured)(headers.add("X-Terret-Key", Fips))
        missed  <- EventsKey.principal(configured)(headers.add(EventsKey.Header, "abd"))
        longer  <- EventsKey.principal(configured)(headers.add(EventsKey.Header, "abcd"))
        blank   <- EventsKey.principal(configured)(headers.add(EventsKey.Header, ""))
        absent  <- EventsKey.principal(configured)(headers)
        hidden  <- EventsKey.principal(EventsKey.from(Secret))(headers.add(EventsKey.Header, Secret))
      yield assertTrue(
        configured.isDefined,
        matched.contains(Principal(FipsId)),
        cased.contains(Principal(FipsId)),
        missed.isEmpty,
        longer.isEmpty,
        blank.isEmpty,
        absent.isEmpty,
        hidden.exists(principal => hashId(principal.value) && !principal.value.contains(Secret)),
        configured.forall(_.toString == "EventsKey"),
        EventsKey.from(Secret).forall(_.toString == "EventsKey"),
        EventsKey.from("").isEmpty,
        EventsKey.from(Fips) == configured,
        EventsKey.from("abd") != configured,
      )
    },
    test("an unset key accepts nobody, including a non-empty header") {
      val headers = Headers.empty
      for
        named  <- EventsKey.principal(None)(headers.add(EventsKey.Header, Fips))
        other  <- EventsKey.principal(None)(headers.add(EventsKey.Header, Secret))
        blank  <- EventsKey.principal(None)(headers.add(EventsKey.Header, ""))
        absent <- EventsKey.principal(None)(headers)
      yield assertTrue(
        named.isEmpty,
        other.isEmpty,
        blank.isEmpty,
        absent.isEmpty,
      )
    },
    test("subscribe with the header is granted and a missing or wrong header is forbidden") {
      EventsKey.from(Secret) match
        case None      => ZIO.succeed(assertTrue(false))
        case Some(key) =>
          ZIO.scoped {
            for
              memory <- InMemory.make(Script.echo)
              terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
              mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test", key = Some(key)))
              result <- granted(mcp, key)
            yield result
          }
    },
  )

  private def granted(mcp: heddle.mcp.Mcp[Milestones & heddle.mcp.events.Events.Env], key: EventsKey) =
    val body =
      for
        _       <- TestClock.setTime(when)
        missing <- mcp.handle(subscribe, Headers.empty)
        wrong   <- mcp.handle(subscribe, Headers.empty.add(EventsKey.Header, Wrong))
        ok      <- mcp.handle(subscribe, Headers.empty.add(EventsKey.Header, Secret))
      yield assertTrue(
        forbidden(missing),
        forbidden(wrong),
        refresh(ok).contains(refreshAt),
        ok.exists(json => !mentions(json, Secret)),
        key.toString == "EventsKey",
      )
    body.provide(mcp.eventsLayer ++ WebhookTransport.unavailable ++ Milestones.live)

  private def hashId(value: String): Boolean =
    value.startsWith("key:") && value.length == 12 && value.drop(4).forall(EventsKeyHex)

  private def forbidden(message: Option[Json]): Boolean =
    val expected = java.math.BigDecimal.valueOf(RpcError.Forbidden(None).code.toLong)
    message match
      case Some(obj: Json.Obj) =>
        obj.get("error") match
          case Some(err: Json.Obj) =>
            err.get("code") match
              case Some(Json.Num(found)) => found.compareTo(expected) == 0
              case _                     => false
          case _ => false
      case _ => false

  private def mentions(json: Json, needle: String): Boolean =
    json match
      case Json.Str(value)  => value.contains(needle)
      case Json.Arr(values) => values.exists(mentions(_, needle))
      case obj: Json.Obj    => obj.fields.exists((name, value) => name.contains(needle) || mentions(value, needle))
      case Json.Bool(_) | Json.Null => false
      case Json.Num(_)              => false

  private def refresh(message: Option[Json]): Option[String] =
    message match
      case Some(obj: Json.Obj) =>
        obj.get("result") match
          case Some(result: Json.Obj) =>
            result.get("refreshBefore") match
              case Some(Json.Str(value)) => Some(value)
              case _                     => None
          case _ => None
      case _ => None

  private val subscribe: Json.Obj =
    val meta = Json.Obj(
      RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> Json.Obj(),
    )
    val params = Json.Obj(
      "name"      -> Json.Str("session.milestone"),
      "arguments" -> Json.Obj("session" -> Json.Str("s"), "kinds" -> Json.Arr()),
      "delivery"  -> Json.Obj(
        "mode"   -> Json.Str("webhook"),
        "url"    -> Json.Str(Url),
        "secret" -> Json.Str(WebhookKey),
      ),
      "_meta" -> meta,
    )
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> Json.Num(1),
      "method"  -> Json.Str("events/subscribe"),
      "params"  -> params,
    )

  private val EventsKeyHex: Char => Boolean =
    char => (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f')
end EventsKeySpec
