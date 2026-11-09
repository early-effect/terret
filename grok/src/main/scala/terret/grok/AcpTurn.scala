package terret.grok

import terret.event.{Event, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, IO, Ref, ZIO}

object AcpTurn:
  def await(pipe: RpcPipe, id: Int): IO[HarnessFailure, Json] =
    val expect = Json.Num(BigDecimal(id))
    pipe.read.flatMap {
      case None       => ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))
      case Some(json) =>
        ZIO.fromEither(Wire.parse(json)).mapError(HarnessFailure.Protocol(_)).flatMap {
          case Wire.Ok(got, result) if Wire.sameId(got, expect) => ZIO.succeed(result)
          case Wire.Err(got, code) if Wire.sameId(got, expect)  =>
            ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Rejected(code)))
          case Wire.Call(requestId, _, _) =>
            pipe.write(Wire.fail(requestId, Wire.methodNotFound, "Method not found")) *> await(pipe, id)
          case _ => await(pipe, id)
        }
    }

  def prompt(pipe: RpcPipe, expect: Json, policy: PermissionPolicy, ceiling: Ceiling): ZStream[Any, HarnessFailure, Event] =
    collect(
      pipe,
      expect,
      (requestId, method, params) => answer(pipe, requestId, method, params, policy, ceiling),
      result => AcpEvents.stop(result).map(outcome => Chunk(Event.TurnFinished(outcome))),
      code => Right(Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Rejected(code)))))),
    )

  def load(pipe: RpcPipe, expect: Json, policy: PermissionPolicy, ceiling: Ceiling): ZStream[Any, HarnessFailure, Event] =
    collect(
      pipe,
      expect,
      (requestId, method, params) => answer(pipe, requestId, method, params, policy, ceiling),
      _ => Right(Chunk.empty),
      code => Left(HarnessFailure.Protocol(ProtocolFault.Rejected(code))),
    )

  private def answer(
      pipe: RpcPipe,
      requestId: Json,
      method: String,
      params: Json,
      policy: PermissionPolicy,
      ceiling: Ceiling,
  ): IO[HarnessFailure, Chunk[Event]] =
    if method == "session/request_permission" then
      val decision = PermissionChoice.decide(policy, ceiling, params)
      pipe.write(Wire.ok(requestId, PermissionChoice.body(decision))).as(Chunk.empty)
    else pipe.write(Wire.fail(requestId, Wire.methodNotFound, "Method not found")).as(Chunk.empty)

  private def collect(
      pipe: RpcPipe,
      expect: Json,
      onCall: (Json, String, Json) => IO[HarnessFailure, Chunk[Event]],
      onOk: Json => Either[ProtocolFault, Chunk[Event]],
      onErr: Int => Either[HarnessFailure, Chunk[Event]],
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      Ref.make(false).map { done =>
        ZStream.repeatZIOChunkOption {
          done.get.flatMap {
            case true  => ZIO.fail(None)
            case false =>
              step(pipe, expect, onCall, onOk, onErr).mapError(Some(_)).flatMap {
                case Step.Emit(events) => ZIO.succeed(events)
                case Step.Done(events) => done.set(true).as(events)
              }
          }
        }
      }
    }

  private def step(
      pipe: RpcPipe,
      expect: Json,
      onCall: (Json, String, Json) => IO[HarnessFailure, Chunk[Event]],
      onOk: Json => Either[ProtocolFault, Chunk[Event]],
      onErr: Int => Either[HarnessFailure, Chunk[Event]],
  ): IO[HarnessFailure, Step] =
    pipe.read.flatMap {
      case None       => ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))
      case Some(json) =>
        ZIO.fromEither(Wire.parse(json)).mapError(HarnessFailure.Protocol(_)).flatMap {
          case Wire.Note(method, params) if Wire.isUpdate(method) =>
            ZIO.fromEither(AcpEvents.fromParams(params)).mapBoth(HarnessFailure.Protocol(_), Step.Emit(_))
          case Wire.Note(_, _)                                    => ZIO.succeed(Step.Emit(Chunk.empty))
          case Wire.Call(requestId, method, params)               => onCall(requestId, method, params).map(Step.Emit(_))
          case Wire.Ok(got, result) if Wire.sameId(got, expect)   =>
            ZIO.fromEither(onOk(result)).mapBoth(HarnessFailure.Protocol(_), Step.Done(_))
          case Wire.Err(got, code) if Wire.sameId(got, expect)    =>
            ZIO.fromEither(onErr(code)).map(Step.Done(_))
          case Wire.Ok(_, _)                                      => ZIO.succeed(Step.Emit(Chunk.empty))
          case Wire.Err(_, _)                                     => ZIO.succeed(Step.Emit(Chunk.empty))
        }
    }

  private enum Step:
    case Emit(events: Chunk[Event])
    case Done(events: Chunk[Event])
end AcpTurn
