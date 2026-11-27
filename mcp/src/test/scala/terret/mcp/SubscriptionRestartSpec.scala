package terret.mcp

import heddle.mcp.events.{
  DeliveryReply,
  EventId,
  EventIdError,
  EventSnapshot,
  Events,
  Principal,
  RingHead,
  StoreError,
  SubscriptionStore,
  VerificationPolicy,
  WebhookCall,
  WebhookTransport,
}
import heddle.mcp.events.EmitError
import heddle.mcp.protocol.{ClientRequest, DeliveryHeaders, EventName, RpcError, TtlRequest}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.milestone.MilestoneKind
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Clock, IO, Ref, Scope, ZIO}

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.Base64
import scala.jdk.CollectionConverters.*

object SubscriptionRestartSpec extends ZIOSpecDefault:
  private val Start   = Instant.parse("2026-11-26T12:00:00Z")
  private val Url     = "https://hooks.example/events"
  private val Failing = "https://hooks.example/failing"
  private val Secret  = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(3.toByte))
  private val policy  = EventPolicy(VerificationPolicy.Skip, 1.minute, 10.minutes, 30.minutes)

  def spec = suite("subscription restart")(
    test("pace is half the shortest grant") {
      assertTrue(
        EventKeep.pace(1.minute) == 30.seconds,
        EventKeep.pace(1.millis) == 1.millis,
        EventKeep.pace(zio.Duration.Zero) == 1.millis,
        EventKeep.pace(zio.Duration.fromMillis(-5L)) == 1.millis,
      )
    },
    test("a restart keeps the id and cursor, and the next event is one past the head") {
      ZIO.scoped {
        for
          path   <- scratch
          mcp    <- open
          _      <- TestClock.setTime(Start)
          opened <- run(mcp, path) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- accept(calls)(_ => 200)
              sub   <- subscribe(Url)
              _     <- emit("m1")
            yield sub
          }
          saved <- read(path)
          next  <- run(mcp, path) {
            for
              calls  <- Ref.make(Chunk.empty[WebhookCall])
              _      <- accept(calls)(_ => 200)
              before <- poll
              _      <- emit("m2")
              seen   <- calls.get
              after  <- poll
            yield (before, seen, after)
          }
        yield
          val (before, seen, after) = next
          val id                    = opened.toOption.flatMap(obj => text(obj, "id"))
          assertTrue(
            id.exists(_.nonEmpty),
            saved.subscriptions.headOption.map(_.id) == id,
            saved.heads == Vector(RingHead("session.milestone", 1L)),
            text(before, "cursor").contains("1"),
            events(before).exists(_.isEmpty),
            seen.length == 1,
            seen.headOption.exists { call =>
              call.header("X-MCP-Subscription-Id") == id &&
              call.body.contains("\"eventId\":\"m2\"") &&
              call.body.contains("\"cursor\":\"1\"")
            },
            text(after, "cursor").contains("2"),
          )
      }
    },
    test("the renew schedule extends a live target and lets a failing url expire") {
      ZIO.scoped {
        for
          path <- scratch
          mcp  <- open
          _    <- TestClock.setTime(Start)
          left <- run(mcp, path) {
            for
              calls <- Ref.make(Chunk.empty[WebhookCall])
              _     <- accept(calls)(call => if call.url == Failing then 500 else 200)
              live  <- subscribe(Url)
              dead  <- subscribe(Failing)
              _     <- emit("m1")
              _     <- EventKeep.renewed(policy.default, EventKeep.pace(policy.floor))
              _     <- TestClock.adjust(EventKeep.pace(policy.floor))
              _     <- TestClock.adjust(policy.default)
              snap  <- read(path)
            yield (live, dead, snap)
          }
        yield
          val (live, dead, snap) = left
          val liveId             = live.toOption.flatMap(obj => text(obj, "id"))
          val deadId             = dead.toOption.flatMap(obj => text(obj, "id"))
          val kept               = liveId.flatMap(id => snap.subscriptions.find(_.id == id))
          assertTrue(
            liveId.exists(_.nonEmpty),
            deadId.exists(id => !snap.subscriptions.exists(_.id == id)),
            kept.exists(_.refreshBefore.isAfter(Start.plus(policy.default))),
            kept.exists(_.lastDeliveryAt.isDefined),
          )
      }
    },
  ) @@ TestAspect.timeout(15.seconds)

  private def open: ZIO[Scope, Probe, heddle.mcp.Mcp[terret.milestone.Milestones & Events.Env]] =
    for
      memory <- InMemory.make(Script.echo)
      terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
      mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test", policy)).mapError(Probe.Build(_))
    yield mcp

  private def run[A](mcp: heddle.mcp.Mcp[terret.milestone.Milestones & Events.Env], path: Path)(
      program: ZIO[Events.Env & Stubbed[WebhookTransport] & Scope, Probe, A]
  ): IO[Probe, A] =
    ZIO.scoped {
      val layer =
        mcp.eventsLayer(SubscriptionStore.file(path.toString)).mapError(Probe.Store(_)) ++
          stubbed[WebhookTransport]
      for
        env   <- layer.build
        scope <- ZIO.environment[Scope]
        value <- program.provideEnvironment(env ++ scope)
      yield value
    }

  private def accept(calls: Ref[Chunk[WebhookCall]])(
      status: WebhookCall => Int
  ): ZIO[Stubbed[WebhookTransport], Nothing, Unit] =
    stub[WebhookTransport](_.post(any[WebhookCall])) { (call: WebhookCall) =>
      calls.update(_ :+ call).as(DeliveryReply(status(call), ""))
    }

  private def subscribe(url: String): ZIO[Events.Env, Probe, Either[RpcError, Json.Obj]] =
    for
      args   <- arguments
      result <- ZIO.serviceWithZIO[Events](
        _.subscribe(
          Some(Principal("rex")),
          ClientRequest.SubscribeEvents(
            EventName("session.milestone"),
            args,
            url,
            Secret,
            None,
            None,
            TtlRequest.Absent,
            DeliveryHeaders.empty,
          ),
        )
      )
    yield result

  private def poll: ZIO[Events.Env, Probe, Either[RpcError, Json.Obj]] =
    arguments.flatMap { args =>
      ZIO.serviceWithZIO[Events](
        _.poll(None, ClientRequest.PollEvents(EventName("session.milestone"), args, None, None, None))
      )
    }

  private def emit(eventId: String): ZIO[Events.Env, Probe, Unit] =
    for
      id <- ZIO.fromEither(EventId.from(eventId)).mapError(Probe.Id(_))
      at <- Clock.instant
      _  <- Events
        .publish(
          EventName("session.milestone"),
          id,
          MilestoneData("s", MilestoneKind.Commit, eventId, Some(0L), at.toString),
          at,
        )
        .mapError(Probe.Publish(_))
    yield ()

  private def arguments: IO[Probe, Json.Obj] =
    ZIO.fromEither(MilestoneFilter(None, Chunk.empty).toJsonAST match
      case Right(obj: Json.Obj) => Right(obj)
      case _                    => Left(Probe.Arguments))

  private def read(path: Path): IO[Probe, EventSnapshot] =
    ZIO
      .attemptBlocking(Files.readString(path))
      .mapError(_ => Probe.Io)
      .flatMap(raw => ZIO.fromEither(EventSnapshot.decode(raw)).mapError(Probe.Store(_)))

  private def text(body: Either[RpcError, Json.Obj], key: String): Option[String] =
    body.toOption.flatMap(obj => text(obj, key))

  private def text(obj: Json.Obj, key: String): Option[String] =
    obj.get(key).collect { case Json.Str(value) => value }

  private def events(body: Either[RpcError, Json.Obj]): Option[Chunk[Json]] =
    body.toOption.flatMap(_.get("events")).collect { case Json.Arr(values) => values }

  private def scratch: ZIO[Scope, Probe, Path] =
    ZIO
      .acquireRelease(
        ZIO
          .attempt {
            val root = Path.of("target")
            Files.createDirectories(root)
            Files.createTempDirectory(root, "subscriptions")
          }
          .mapError(_ => Probe.Io)
      )(root => ZIO.attempt(deleteTree(root)).ignore)
      .map(_.resolve("subscriptions.json"))

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walked = Files.walk(root)
      try
        val paths = walked.iterator.asScala.toVector.sortBy(_.getNameCount)(using Ordering[Int].reverse)
        paths.foreach(Files.deleteIfExists(_))
      finally walked.close()

  private enum Probe:
    case Build(error: TerretMcpError)
    case Arguments
    case Id(error: EventIdError)
    case Publish(error: EmitError)
    case Store(error: StoreError)
    case Io
end SubscriptionRestartSpec
