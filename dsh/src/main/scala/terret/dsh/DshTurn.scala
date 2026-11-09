package terret.dsh

import heddle.LinePipe
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, IO, Ref, ZIO}

object DshTurn:
  def events(
      stdout: LinePipe,
      exit: IO[HarnessFailure, Int],
      expected: Option[String],
      adopted: Ref[Option[String]],
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      Ref.make(State.empty).map { state =>
        ZStream.repeatZIOChunkOption {
          state.get.flatMap { now =>
            if now.closed then ZIO.fail(None)
            else
              stdout.readLine.mapError(_ => Some(HarnessFailure.Protocol(ProtocolFault.Incomplete))).flatMap {
                case None => finish(state, exit)
                case Some(line) =>
                  ZIO.fromEither(DshLine.parse(line)).mapError(fault => Some(HarnessFailure.Protocol(fault))).flatMap {
                    notice => step(state, adopted, expected, notice).mapError(fault => Some(HarnessFailure.Protocol(fault)))
                  }
              }
          }
        }
      }
    }

  private def step(
      state: Ref[State],
      adopted: Ref[Option[String]],
      expected: Option[String],
      notice: DshNotice,
  ): IO[ProtocolFault, Chunk[Event]] =
    notice match
      case DshNotice.Skip                    => ZIO.succeed(Chunk.empty)
      case DshNotice.Session(id)             => adopt(adopted, expected, id).as(Chunk.empty)
      case DshNotice.Thought(text)           => ZIO.succeed(Chunk(Event.Thought(text)))
      case DshNotice.Assistant(text)         => ZIO.succeed(Chunk(Event.Assistant(text)))
      case DshNotice.ToolStarted(call)       => learn(state, call)
      case DshNotice.ToolResult(id, status, result) => recall(state, id, status, result)
      case DshNotice.Turn(outcome)           => state.update(_.copy(outcome = Some(outcome))).as(Chunk.empty)
      case DshNotice.Final(_)                => closeIfDecided(state)
      case DshNotice.Failed                  => fail(state)

  private def adopt(adopted: Ref[Option[String]], expected: Option[String], id: String): IO[ProtocolFault, Unit] =
    adopted.get.flatMap { have =>
      val mismatch = have.exists(_ != id) || expected.exists(_ != id)
      if mismatch then ZIO.fail(ProtocolFault.Malformed)
      else adopted.set(Some(id))
    }

  private def learn(state: Ref[State], call: ToolCall): IO[ProtocolFault, Chunk[Event]] =
    state.update(now => now.copy(names = now.names.updated(call.id.value, call.name))).as(Chunk(Event.ToolStarted(call)))

  private def recall(state: Ref[State], callId: String, status: String, result: String): IO[ProtocolFault, Chunk[Event]] =
    state.get.map { now =>
      (now.names.get(callId), CallId.from(callId)) match
        case (Some(name), Right(id)) =>
          val payload = Json.Obj("status" -> Json.Str(status), "result" -> Json.Str(result))
          Chunk(Event.ToolUpdated(ToolCall(id, name, payload)))
        case _ => Chunk.empty
    }

  private def closeIfDecided(state: Ref[State]): IO[ProtocolFault, Chunk[Event]] =
    state.modify { now =>
      now.outcome match
        case Some(done) if !now.closed => (Chunk(Event.TurnFinished(done)), now.copy(closed = true))
        case _                         => (Chunk.empty, now)
    }

  private def fail(state: Ref[State]): IO[ProtocolFault, Chunk[Event]] =
    val outcome = TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Incomplete))
    state.update(_.copy(outcome = Some(outcome), closed = true)).as(Chunk(Event.TurnFinished(outcome)))

  private def finish(state: Ref[State], exit: IO[HarnessFailure, Int]): ZIO[Any, Option[HarnessFailure], Chunk[Event]] =
    state.get.flatMap { now =>
      now.outcome match
        case Some(done) =>
          state.set(now.copy(closed = true)).as(Chunk(Event.TurnFinished(done)))
        case None =>
          exit.mapError(Some(_)).flatMap { code =>
            if code == 0 then ZIO.fail(Some(HarnessFailure.Protocol(ProtocolFault.Incomplete)))
            else ZIO.fail(Some(HarnessFailure.Exited(code)))
          }
    }

  private final case class State(names: Map[String, ToolName], outcome: Option[TurnOutcome], closed: Boolean)

  private object State:
    val empty: State = State(Map.empty, None, false)
end DshTurn
