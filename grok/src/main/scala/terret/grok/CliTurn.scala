package terret.grok

import heddle.LinePipe
import terret.event.{Event, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.stream.ZStream
import zio.{Chunk, Ref, ZIO}

object CliTurn:
  def events(
      stdout: LinePipe,
      exit: ZIO[Any, HarnessFailure, Int],
      hostCancel: Ref[Boolean],
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      for
        done    <- Ref.make(false)
        names   <- Ref.make(Map.empty[String, ToolName])
        printed <- Ref.make(Map.empty[String, String])
      yield ZStream.repeatZIOChunkOption {
        done.get.flatMap {
          case true  => ZIO.fail(None)
          case false =>
            stdout.readLine.mapError(_ => Some(HarnessFailure.Protocol(ProtocolFault.Incomplete))).flatMap {
              case None       => finish(exit)
              case Some(line) =>
                for
                  seen      <- names.get
                  prior     <- printed.get
                  cancelled <- hostCancel.get
                  parsed    <- ZIO
                    .fromEither(CliLine.parse(line, seen))
                    .mapError(fault => Some(HarnessFailure.Protocol(fault)))
                  (next, events)  = parsed
                  adjusted        = if cancelled then events.map(hosted) else events
                  (kept, trimmed) = ToolOutput.trim(prior, adjusted)
                  terminal        = trimmed.exists {
                    case Event.TurnFinished(_) => true
                    case _                     => false
                  }
                  result <- names.set(next) *> printed.set(kept) *> done.set(terminal).as(trimmed)
                yield result
            }
        }
      }
    }

  private def hosted(event: Event): Event =
    event match
      case Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.PermissionDenied))) =>
        Event.TurnFinished(TurnOutcome.Cancelled)
      case other => other

  private def finish(exit: ZIO[Any, HarnessFailure, Int]): ZIO[Any, Option[HarnessFailure], Chunk[Event]] =
    exit.mapError(Some(_)).flatMap { code =>
      if code == 0 then ZIO.fail(Some(HarnessFailure.Protocol(ProtocolFault.Incomplete)))
      else ZIO.fail(Some(HarnessFailure.Exited(code)))
    }
end CliTurn
