package terret.app

import heddle.mcp.events.{EventSnapshot, StoreError, StoredSubscription, WebhookUrl}
import terret.Terret
import terret.mcp.{ActivityEvents, ActivityFilter, MilestoneEvents, MilestoneFilter}
import zio.json.JsonDecoder
import zio.{Chunk, Clock, IO, UIO, ULayer, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.time.Instant

/** Sessions this process still holds. */
trait OpenSessions:
  def ids: UIO[Set[String]]

object OpenSessions:
  def layer(terret: Terret): ULayer[OpenSessions] =
    ZLayer.succeed(new OpenSessions:
      def ids = terret.list.map(rows => rows.map(_.id.value).toSet))

/** The subscriptions file. A missing file is empty, not a failure. */
trait SubscriptionJournal:
  def load: IO[StoreError, Option[String]]
  def save(text: String): IO[StoreError, Unit]

object SubscriptionJournal:
  def file(path: Path): ULayer[SubscriptionJournal] =
    ZLayer.succeed(FileJournal(path))

/** Drops saved subscriptions that would stop startup, then rewrites the file.
  *
  * A row heddle cannot materialize is unrestorable. A row that names a session this process does not hold is stale.
  * An expired row is dropped. Every other row keeps its id. Ring heads stay, so the next event is still one past the
  * saved cursor.
  */
trait SubscriptionResume:
  def prepare: ZIO[OpenSessions & SubscriptionJournal, StoreError, Chunk[SkippedSubscription]]

object SubscriptionResume:
  val layer: ULayer[SubscriptionResume] =
    ZLayer.succeed(Resume())

  def prepare
      : ZIO[SubscriptionResume & OpenSessions & SubscriptionJournal, StoreError, Chunk[SkippedSubscription]] =
    ZIO.serviceWithZIO[SubscriptionResume](_.prepare)

final case class SkippedSubscription(id: String, reason: SkipReason):
  def render: String = s"skipped event subscription $id: ${reason.render}"

enum SkipReason:
  case Expired
  case Secret
  case UnknownEvent(name: String)
  case Arguments
  case Url
  case Session(id: String)

  def render: String = this match
    case Expired              => "expired"
    case Secret               => "secret"
    case UnknownEvent(name)   => s"unknown event $name"
    case Arguments            => "arguments"
    case Url                  => "url"
    case Session(id)          => s"session $id is not open"
end SkipReason

private final class Resume extends SubscriptionResume:
  def prepare =
    ZIO.serviceWithZIO[SubscriptionJournal](_.load).flatMap {
      case None       => ZIO.succeed(Chunk.empty)
      case Some(text) =>
        for
          snapshot <- ZIO.fromEither(EventSnapshot.decode(text))
          open     <- ZIO.serviceWithZIO[OpenSessions](_.ids)
          now      <- Clock.instant
          (kept, skipped) = partition(snapshot, open, now)
          _ <-
            if skipped.isEmpty then ZIO.unit
            else ZIO.serviceWithZIO[SubscriptionJournal](_.save(EventSnapshot.encode(kept)))
        yield skipped
    }

  private def partition(
      snapshot: EventSnapshot,
      open: Set[String],
      now: Instant,
  ): (EventSnapshot, Chunk[SkippedSubscription]) =
    val (skipped, kept) =
      snapshot.subscriptions.foldLeft((Chunk.empty[SkippedSubscription], Vector.empty[StoredSubscription])) {
        (acc, sub) =>
          val (bad, good) = acc
          reason(sub, open, now) match
            case Some(why) => (bad :+ SkippedSubscription(sub.id, why), good)
            case None      => (bad, good :+ sub)
      }
    (EventSnapshot(kept, snapshot.heads), skipped)

  private def reason(sub: StoredSubscription, open: Set[String], now: Instant): Option[SkipReason] =
    // Restore rejects a secret outside these bounds. A file row is already inside them; this keeps a bad one from
    // failing the layer.
    if sub.secret.length < 24 || sub.secret.length > 64 then Some(SkipReason.Secret)
    else if WebhookUrl.from(sub.url).isLeft then Some(SkipReason.Url)
    else if sub.name != MilestoneEvents.name.value && sub.name != ActivityEvents.name.value then
      Some(SkipReason.UnknownEvent(sub.name))
    else
      sessionOf(sub) match
        case None                                       => Some(SkipReason.Arguments)
        case Some(_) if !sub.refreshBefore.isAfter(now) => Some(SkipReason.Expired)
        case Some(Some(id)) if !open.contains(id)        => Some(SkipReason.Session(id))
        case Some(_)                                     => None

  private def sessionOf(sub: StoredSubscription): Option[Option[String]] =
    val decoded =
      if sub.name == MilestoneEvents.name.value then
        JsonDecoder[MilestoneFilter].fromJsonAST(sub.arguments).map(_.session)
      else JsonDecoder[ActivityFilter].fromJsonAST(sub.arguments).map(_.session)
    decoded.toOption
end Resume

private final class FileJournal(path: Path) extends SubscriptionJournal:
  def load =
    ZIO
      .attemptBlocking {
        if Files.notExists(path) then None
        else Some(Files.readString(path, StandardCharsets.UTF_8))
      }
      .mapError(error => StoreError.Unreadable(error.getClass.getSimpleName))

  def save(text: String) =
    ZIO
      .attemptBlocking(write(text))
      .mapError(error => StoreError.Unwritable(error.getClass.getSimpleName))

  private def write(text: String): Unit =
    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
    val leaf = Option(path.getFileName).fold("subscriptions")(_.toString)
    val tmp  = path.resolveSibling(leaf + ".tmp")
    Files.writeString(tmp, text, StandardCharsets.UTF_8)
    try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)
    ()
end FileJournal
