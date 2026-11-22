package terret.mcp

import heddle.mcp.events.{DeliveryReply, Events, Principal, VerificationPolicy, WebhookCall, WebhookTransport}
import heddle.mcp.protocol.{ClientRequest, DeliveryHeaders, EventName, RpcError, TtlRequest}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.Milestones
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Clock, Ref, Scope, ZIO}

import java.util.Base64

object EventPolicySpec extends ZIOSpecDefault:
  private val Url    = "https://hooks.example/events"
  private val Secret = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(3.toByte))

  private val granted = EventPolicy(VerificationPolicy.Skip, 2.minutes, 10.minutes, 45.minutes)

  def spec = suite("event policy")(
    test("skip verification grants floor, default, and cap, then the grant expires") {
      drive(Some(granted)) {
        for
          calls  <- Ref.make(Chunk.empty[WebhookCall])
          _      <- record(calls)
          at     <- Clock.instant
          raised <- subscribe("floor", TtlRequest.Millis(1))
          plain  <- subscribe("default", TtlRequest.Absent)
          capped <- subscribe("cap", TtlRequest.Unbounded)
          seen   <- calls.get
          _      <- TestClock.setTime(at.plusMillis(granted.cap.toMillis + 1000L))
          _      <- Events.maintain
          gone   <- unsubscribe("cap")
        yield assertTrue(
          refresh(raised).contains(at.plusMillis(granted.floor.toMillis).toString),
          refresh(plain).contains(at.plusMillis(granted.default.toMillis).toString),
          refresh(capped).contains(at.plusMillis(granted.cap.toMillis).toString),
          seen.isEmpty,
          gone == Left(RpcError.NotFound(Some("subscription"))),
        )
      }
    },
    test("the terret preset skips verification and grants its default ttl") {
      drive(None) {
        for
          calls <- Ref.make(Chunk.empty[WebhookCall])
          _     <- record(calls)
          at    <- Clock.instant
          plain <- subscribe("preset", TtlRequest.Absent)
          seen  <- calls.get
        yield assertTrue(
          refresh(plain).contains(at.plusMillis(EventPolicy.terret.default.toMillis).toString),
          seen.isEmpty,
        )
      }
    },
    test("required verification posts a challenge and does not commit a foreign body") {
      val policy = EventPolicy(VerificationPolicy.Required, 1.minute, 2.hours, 24.hours)
      drive(Some(policy)) {
        for
          calls  <- Ref.make(Chunk.empty[WebhookCall])
          _      <- record(calls)
          opened <- subscribe("required", TtlRequest.Absent)
          seen   <- calls.get
        yield assertTrue(opened.isLeft, seen.length == 1, seen.forall(verification))
      }
    },
  )

  private def drive(policy: Option[EventPolicy])(
      use: ZIO[Events.Env & Stubbed[WebhookTransport], ProbeFailure, TestResult]
  ): ZIO[Any, ProbeFailure, TestResult] =
    ZIO.scoped {
      open(policy).flatMap { mcp =>
        use.provide(mcp.eventsLayer ++ stubbed[WebhookTransport])
      }
    }

  private def open(
      policy: Option[EventPolicy]
  ): ZIO[Scope, ProbeFailure, heddle.mcp.Mcp[Milestones & Events.Env]] =
    for
      memory <- InMemory.make(Script.echo)
      terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
      built  <- policy match
        case Some(chosen) => ZIO.fromEither(TerretMcp.mcp(terret, "test", chosen)).mapError(ProbeFailure.Build(_))
        case None         => ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(ProbeFailure.Build(_))
    yield built

  private def record(calls: Ref[Chunk[WebhookCall]]): ZIO[Stubbed[WebhookTransport], Nothing, Unit] =
    stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
      calls.update(_ :+ call).as(DeliveryReply(200, "not-the-nonce"))
    }

  private def subscribe(session: String, ttl: TtlRequest): ZIO[Events.Env, ProbeFailure, Either[RpcError, Json.Obj]] =
    for
      args   <- ZIO.fromEither(arguments(session)).mapError(ProbeFailure.Arguments(_))
      result <- ZIO.serviceWithZIO[Events](
        _.subscribe(Some(Principal("rex")), request(args, ttl))
      )
    yield result

  private def unsubscribe(session: String): ZIO[Events.Env, ProbeFailure, Either[RpcError, Json.Obj]] =
    for
      args   <- ZIO.fromEither(arguments(session)).mapError(ProbeFailure.Arguments(_))
      result <- ZIO.serviceWithZIO[Events](
        _.unsubscribe(
          Some(Principal("rex")),
          ClientRequest.UnsubscribeEvents(EventName("session.milestone"), args, Url),
        )
      )
    yield result

  private def request(args: Json.Obj, ttl: TtlRequest): ClientRequest.SubscribeEvents =
    ClientRequest.SubscribeEvents(
      EventName("session.milestone"),
      args,
      Url,
      Secret,
      None,
      None,
      ttl,
      DeliveryHeaders.empty,
    )

  private def arguments(session: String): Either[String, Json.Obj] =
    MilestoneFilter(Some(session), Chunk.empty).toJsonAST match
      case Right(obj: Json.Obj) => Right(obj)
      case Right(other)         => Left(other.toJson)
      case Left(message)        => Left(message)

  private def refresh(result: Either[RpcError, Json.Obj]): Option[String] =
    result.toOption.flatMap(_.get("refreshBefore")).collect { case Json.Str(value) => value }

  private def verification(call: WebhookCall): Boolean =
    call.body.fromJson[Json] match
      case Right(obj: Json.Obj) => obj.get("type").contains(Json.Str("verification"))
      case _                    => false

  private enum ProbeFailure:
    case Build(error: TerretMcpError)
    case Arguments(message: String)
end EventPolicySpec
