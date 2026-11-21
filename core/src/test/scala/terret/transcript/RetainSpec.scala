package terret.transcript

import terret.event.{CallId, Event, ToolCall, ToolName}
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.durationInt
import zio.{Chunk, Promise, Ref, ZIO}

object RetainSpec extends ZIOSpecDefault:
  private val call = ToolCall(CallId("c1"), ToolName("grep"), Json.Obj())

  def spec = suite("Retain")(
    test("adjacent assistant and thought runs join, and other events stay separate") {
      val assistants = Retain.coalesce(
        ZStream(
          Event.Assistant("a"),
          Event.Assistant(" b"),
          Event.ToolStarted(call),
          Event.Assistant("c"),
        )
      )(ignore)
      val thoughts = Retain.coalesce(
        ZStream(
          Event.Thought("a"),
          Event.Thought(" b"),
          Event.ToolUpdated(call),
          Event.ToolStarted(call),
        )
      )(ignore)
      for
        joined <- assistants.runCollect
        kept   <- thoughts.runCollect
      yield assertTrue(
        joined == Chunk(
          Event.Assistant("a b"),
          Event.ToolStarted(call),
          Event.Assistant("c"),
        ),
        kept == Chunk(
          Event.Thought("a b"),
          Event.ToolUpdated(call),
          Event.ToolStarted(call),
        ),
      )
    },
    test("cancelling the upstream emits the open text run") {
      for
        gate  <- Promise.make[Nothing, Unit]
        seen  <- Promise.make[Nothing, Unit]
        fiber <- Retain
          .coalesce(
            (ZStream(Event.Assistant("stay")) ++ ZStream.fromZIO(seen.succeed(()) *> gate.await).drain)
              .interruptWhen(gate)
          )(ignore)
          .runCollect
          .fork
        _   <- seen.await
        _   <- gate.succeed(())
        got <- fiber.join
      yield assertTrue(got == Chunk(Event.Assistant("stay")))
    },
    test("an open text run is emitted when the stream fails") {
      val stream = ZStream(Event.Assistant("a"), Event.Assistant(" b")) ++ ZStream.fail("boom")
      Retain.coalesce(stream)(ignore).either.runCollect.map { events =>
        assertTrue(events == Chunk(Right(Event.Assistant("a b")), Left("boom")))
      }
    },
    test("a parked text run is visible before cancel emits it") {
      for
        parked <- Promise.make[Nothing, Unit]
        gate   <- Promise.make[Nothing, Unit]
        fiber  <- Retain.parked.locally(_ => parked.succeed(()).unit) {
          Retain
            .coalesce(
              (ZStream(Event.Thought("hmm")) ++ ZStream.fromZIO(gate.await).drain).interruptWhen(gate)
            )(ignore)
            .runCollect
            .fork
        }
        _   <- parked.await
        _   <- gate.succeed(())
        got <- fiber.join
      yield assertTrue(got == Chunk(Event.Thought("hmm")))
    } @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock,
    test("interrupting the consumer delivers the open text run") {
      for
        seen    <- Promise.make[Nothing, Unit]
        flushed <- Ref.make(Chunk.empty[Event])
        extra   <- Ref.make(Chunk.empty[Event])
        fiber   <- Retain.deliver.locally(event => extra.update(_ :+ event)) {
          Retain
            .coalesce(
              ZStream(Event.Assistant("stay")) ++ ZStream.fromZIO(seen.succeed(()) *> ZIO.never).drain
            )(event => flushed.update(_ :+ event))
            .runCollect
            .fork
        }
        _         <- seen.await
        _         <- fiber.interrupt
        fromFlush <- flushed.get
        fromRef   <- extra.get
      yield assertTrue(
        fromFlush == Chunk(Event.Assistant("stay")),
        fromRef == Chunk(Event.Assistant("stay")),
      )
    },
  )

  private val ignore: Event => ZIO[Any, Nothing, Unit] = _ => ZIO.unit
end RetainSpec
