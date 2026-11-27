package terret.transcript

import terret.event.Event
import terret.milestone.MilestoneSink
import terret.session.SessionId
import zio.test.*
import zio.{Chunk, Queue, ZIO}
import zio.durationInt

object JournalSpec extends ZIOSpecDefault:
  private val id = SessionId("session")

  def spec = suite("Journal")(
    test("subscribe, then the snapshot: nothing is lost and nothing is repeated") {
      for
        journal <- Journal.make
        _       <- journal.open(id)
        _       <- ZIO.foreachDiscard(0 until 5)(n => journal.append(id, say(n)))
        seen    <- Queue.unbounded[Entry]
        pulling <- journal.follow(id, None).foreach(seen.offer).fork
        _       <- awaitCount(seen, 5)
        _       <- ZIO.foreachDiscard(5 until 40)(n => journal.append(id, say(n)))
        _       <- awaitCount(seen, 40)
        _       <- pulling.interrupt
        got     <- seen.takeAll
      yield assertTrue(
        got.map(_.index) == Chunk.fromIterable(0 until 40),
        got.map(_.event) == Chunk.fromIterable((0 until 40).map(say)),
      )
    },
    test("two followers see the same log, and interrupting one leaves the writer running") {
      for
        journal <- Journal.make
        _       <- journal.open(id)
        left    <- journal.follow(id, None).take(2).runCollect.fork
        right   <- journal.follow(id, None).runDrain.fork
        _       <- journal.append(id, say(0))
        _       <- journal.append(id, say(1))
        first   <- left.join
        _       <- right.interrupt
        _       <- journal.append(id, say(2))
        page    <- journal.page(id, None)
        again   <- journal.follow(id, Some(1)).take(1).runCollect
      yield assertTrue(
        first.map(_.index) == Chunk(0L, 1L),
        page.map(_.events.map(_.event)) == Some(Chunk(say(0), say(1), say(2))),
        page.flatMap(_.cursor) == Some(2L),
        again.map(fact) == Chunk((2L, 0L, say(2))),
      )
    },
    test("a page after the cursor is the tail, and the next page is empty with the same cursor") {
      for
        journal <- Journal.make
        _       <- journal.open(id)
        empty   <- journal.page(id, None)
        _       <- ZIO.foreachDiscard(0 until 3)(n => journal.append(id, say(n)))
        all     <- journal.page(id, None)
        tail    <- journal.page(id, Some(0))
        rest    <- journal.page(id, Some(2))
        ahead   <- journal.page(id, Some(9))
        whole   <- journal.page(id, Some(-1))
        missing <- journal.page(SessionId("missing"), None)
        quiet   <- journal.follow(SessionId("missing"), None).runCollect
      yield assertTrue(
        empty == Some(Page(Chunk.empty, None)),
        all.map(page => page.events.map(fact) -> page.cursor) == Some(
          Chunk((0L, 0L, say(0)), (1L, 0L, say(1)), (2L, 0L, say(2))) -> Some(2L)
        ),
        tail.map(page => page.events.map(fact) -> page.cursor) == Some(
          Chunk((1L, 0L, say(1)), (2L, 0L, say(2))) -> Some(2L)
        ),
        rest == Some(Page(Chunk.empty, Some(2))),
        ahead == Some(Page(Chunk.empty, Some(2))),
        whole == all,
        missing.isEmpty,
        quiet.isEmpty,
      )
    },
    test("past the bound the oldest entries drop and the indexes that remain do not move") {
      JournalBound.from(2) match
        case Left(error)  => ZIO.succeed(assertTrue(false) ?? error.toString)
        case Right(bound) =>
          for
            journal <- Journal.make(MilestoneSink.discard, bound)
            _       <- journal.open(id)
            _       <- ZIO.foreachDiscard(0 until 4)(n => journal.append(id, say(n)))
            page    <- journal.page(id, None)
            tail    <- journal.page(id, Some(1))
            dropped <- journal.page(id, Some(0))
          yield assertTrue(
            JournalBound.default.limit == 4096,
            JournalBound.from(0) == Left(JournalBoundError.NonPositive(0)),
            page.map(found => found.events.map(fact) -> found.cursor) == Some(
              Chunk((2L, 0L, say(2)), (3L, 0L, say(3))) -> Some(3L)
            ),
            tail.map(found => found.events.map(fact) -> found.cursor) == Some(
              Chunk((2L, 0L, say(2)), (3L, 0L, say(3))) -> Some(3L)
            ),
            dropped == tail,
          )
    },
    test("follow after an index replays only the tail") {
      for
        journal <- Journal.make
        _       <- journal.open(id)
        _       <- ZIO.foreachDiscard(0 until 3)(n => journal.append(id, say(n)))
        tail    <- journal.follow(id, Some(0)).take(2).runCollect
      yield assertTrue(tail.map(fact) == Chunk((1L, 0L, say(1)), (2L, 0L, say(2))))
    },
    test("an entry records the marked turn and the time it was appended") {
      for
        journal <- Journal.make
        _       <- journal.open(id)
        _       <- journal.mark(id, 4L)
        before  <- zio.Clock.instant
        _       <- journal.append(id, say(0))
        after   <- zio.Clock.instant
        page    <- journal.page(id, None)
      yield assertTrue(
        page.exists { found =>
          found.events.exists { entry =>
            entry.index == 0L && entry.turn == 4L && !entry.at.isBefore(before) && !entry.at.isAfter(after)
          }
        }
      )
    },
  ) @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock

  private def fact(entry: Entry): (Long, Long, Event) =
    (entry.index, entry.turn, entry.event)

  private def say(n: Int): Event = Event.Assistant(n.toString)

  private def awaitCount(seen: Queue[Entry], n: Int): ZIO[Any, Nothing, Unit] =
    seen.size.flatMap {
      case got if got >= n => ZIO.unit
      case _               => ZIO.yieldNow *> awaitCount(seen, n)
    }
end JournalSpec
