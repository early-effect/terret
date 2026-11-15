package terret.grok

import heddle.LinePipe
import terret.event.{Event, ToolName}
import terret.harness.{HarnessFailure, ProtocolFault}
import zio.stream.ZStream
import zio.{Chunk, Ref, ZIO}

object CliTurn:
  def events(stdout: LinePipe, exit: ZIO[Any, HarnessFailure, Int]): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      for
        done  <- Ref.make(false)
        names <- Ref.make(Map.empty[String, ToolName])
      yield ZStream.repeatZIOChunkOption {
        done.get.flatMap {
          case true  => ZIO.fail(None)
          case false =>
            stdout.readLine.mapError(_ => Some(HarnessFailure.Protocol(ProtocolFault.Incomplete))).flatMap {
              case None       => finish(exit)
              case Some(line) =>
                names.get.flatMap { seen =>
                  ZIO
                    .fromEither(CliLine.parse(line, seen))
                    .mapError(fault => Some(HarnessFailure.Protocol(fault)))
                    .flatMap { (next, events) =>
                      val terminal = events.exists {
                        case Event.TurnFinished(_) => true
                        case _                     => false
                      }
                      names.set(next) *> done.set(terminal).as(events)
                    }
                }
            }
        }
      }
    }

  private def finish(exit: ZIO[Any, HarnessFailure, Int]): ZIO[Any, Option[HarnessFailure], Chunk[Event]] =
    exit.mapError(Some(_)).flatMap { code =>
      if code == 0 then ZIO.fail(Some(HarnessFailure.Protocol(ProtocolFault.Incomplete)))
      else ZIO.fail(Some(HarnessFailure.Exited(code)))
    }
end CliTurn
