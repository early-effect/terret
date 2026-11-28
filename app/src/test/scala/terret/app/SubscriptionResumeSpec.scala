package terret.app

import heddle.mcp.events.{EventSnapshot, RingHead, StoreError, SubscriptionStore}
import slayer.*
import terret.Terret
import terret.config.TerretConfig
import terret.hooks.Hooks
import terret.mcp.{MilestoneEvents, MilestoneFilter, TerretMcp}
import terret.memory.{InMemory, Script}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.durationInt
import zio.{Chunk, Exit, Ref, Scope, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.Base64
import scala.jdk.CollectionConverters.*

object SubscriptionResumeSpec extends ZIOSpecDefault:
  private val Start       = Instant.parse("2026-11-26T12:00:00Z")
  private val Url         = "https://hooks.example/events"
  private val Secret      = "whsec_" + Base64.getEncoder.encodeToString(Array.fill(32)(3.toByte))
  private val LiveSession = "live-session"
  private val Head        = RingHead(MilestoneEvents.name.value, 4L)

  def spec = suite("subscription resume")(
    test("a stale or unrestorable subscription is skipped and the rest restore") {
      ZIO.scoped {
        for
          stored <- Ref.make(Option(snapshot))
          ran    <- passed(stored)
          (first, restored, second) = ran
          kept  = decoded(first.written)
          later = decoded(second.written)
        yield assertTrue(
          first.skipped.map(_.render) == Chunk(
            "skipped event subscription sub_session: session gone is not open",
            "skipped event subscription sub_bad: arguments",
            "skipped event subscription sub_unknown: unknown event retired.event",
            "skipped event subscription sub_old: expired",
          ),
          kept.map(_.subscriptions.map(_.id)) == Some(Vector("sub_live", "sub_open")),
          kept.map(_.heads) == Some(Vector(Head)),
          restored.isEmpty,
          second.skipped.map(_.render) == Chunk("skipped event subscription sub_live: expired"),
          later.map(_.subscriptions.map(_.id)) == Some(Vector("sub_open")),
          later.map(_.heads) == Some(Vector(Head)),
        )
      }
    },
    test("a file that cannot be decoded fails and is not rewritten") {
      for
        wrote <- Ref.make(false)
        failed <- undecodable(wrote)
        called <- wrote.get
      yield assertTrue(failed == Exit.fail(StoreError.Undecodable), !called)
    },
  ) @@ TestAspect.timeout(15.seconds)

  private def passed(stored: Ref[Option[String]]) =
    (for
      _ <- TestClock.setTime(Start)
      _ <- stubs(stored)
      first <- current(stored)
      restored <- reopen(first.written)
      _ <- TestClock.adjust(31.minutes)
      second <- current(stored)
    yield (first, restored, second))
      .provideSome[Scope](SubscriptionResume.layer, stubbed[OpenSessions], stubbed[SubscriptionJournal])

  private def current(stored: Ref[Option[String]]) =
    for
      skipped <- SubscriptionResume.prepare
      written <- stored.get
    yield Step(skipped, written)

  private def stubs(stored: Ref[Option[String]]) =
    stub[SubscriptionJournal](_.load)(stored.get) *>
      stub[SubscriptionJournal](_.save(any[String]))((text: String) => stored.set(Some(text))) *>
      stub[OpenSessions](_.ids)(ZIO.succeed(Set(LiveSession)))

  private def undecodable(wrote: Ref[Boolean]) =
    (for
      _ <- stub[SubscriptionJournal](_.load)(ZIO.succeed(Some("{")))
      _ <- stub[SubscriptionJournal](_.save(any[String]))((_: String) => wrote.set(true))
      failed <- SubscriptionResume.prepare.exit
    yield failed).provide(SubscriptionResume.layer, stubbed[OpenSessions], stubbed[SubscriptionJournal])

  private def reopen(written: Option[String]): ZIO[Scope, Nothing, String] =
    written match
      case None       => ZIO.succeed("nothing saved")
      case Some(text) =>
        ZIO.scoped {
          val opened =
            for
              path   <- scratch.mapError(_.toString)
              _      <- ZIO.attempt(Files.writeString(path, text, StandardCharsets.UTF_8)).mapError(_.toString)
              memory <- InMemory.make(Script.echo)
              terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
              mcp    <- ZIO.fromEither(TerretMcp.mcp(terret, "test")).mapError(_.toString)
              built  <- mcp.eventsLayer(SubscriptionStore.file(path.toString)).build.either
            yield built.fold(_.toString, _ => "")
          opened.foldCause(cause => cause.prettyPrint, identity)
        }

  private def decoded(written: Option[String]): Option[EventSnapshot] =
    written.flatMap(text => EventSnapshot.decode(text).toOption)

  private def snapshot: String =
    val live    = Start.plusSeconds(30 * 60)
    val holding = Start.plusSeconds(2 * 60 * 60)
    val later   = Start.plusSeconds(60 * 60)
    val rows: Chunk[Json] = Chunk(
      row("sub_live", MilestoneEvents.name.value, arguments(MilestoneFilter(None, Chunk.empty)), live),
      row("sub_open", MilestoneEvents.name.value, arguments(MilestoneFilter(Some(LiveSession), Chunk.empty)), holding),
      row("sub_session", MilestoneEvents.name.value, arguments(MilestoneFilter(Some("gone"), Chunk.empty)), later),
      row("sub_bad", MilestoneEvents.name.value, Json.Obj("session" -> Json.Num(BigDecimal(1))), later),
      row("sub_unknown", "retired.event", arguments(MilestoneFilter(None, Chunk.empty)), later),
      row("sub_old", MilestoneEvents.name.value, arguments(MilestoneFilter(None, Chunk.empty)), Start.minusSeconds(1)),
    )
    Json
      .Obj(
        "subscriptions" -> Json.Arr(rows),
        "heads"         -> Json.Arr(Json.Obj("name" -> Json.Str(Head.name), "head" -> Json.Num(BigDecimal(Head.head)))),
      )
      .toJson

  private def row(id: String, name: String, arguments: Json, refresh: Instant): Json.Obj =
    Json.Obj(
      "id"            -> Json.Str(id),
      "principal"     -> Json.Str("rex"),
      "url"           -> Json.Str(Url),
      "name"          -> Json.Str(name),
      "arguments"     -> arguments,
      "refreshBefore" -> Json.Str(refresh.toString),
      "secret"        -> Json.Str(Secret),
    )

  private def arguments(filter: MilestoneFilter): Json =
    filter.toJsonAST match
      case Right(obj: Json.Obj) => obj
      case _                    => Json.Obj()

  private def scratch: ZIO[Scope, Throwable, Path] =
    ZIO
      .acquireRelease(
        ZIO.attempt {
          val root = Path.of("target")
          Files.createDirectories(root)
          Files.createTempDirectory(root, "subscription-resume")
        }
      )(root => ZIO.attempt(deleteTree(root)).ignore)
      .map(_.resolve("subscriptions.json"))

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walked = Files.walk(root)
      try
        val paths = walked.iterator.asScala.toVector.sortBy(_.getNameCount)(using Ordering[Int].reverse)
        paths.foreach(Files.deleteIfExists(_))
      finally walked.close()

  private final case class Step(skipped: Chunk[SkippedSubscription], written: Option[String])
end SubscriptionResumeSpec
