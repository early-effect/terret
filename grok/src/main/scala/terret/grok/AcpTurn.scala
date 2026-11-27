package terret.grok

import terret.event.{Event, ToolName, TurnOutcome}
import terret.harness.{HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, IO, Ref, ZIO}

object AcpTurn:
  final case class Scope(
      policy: PermissionPolicy,
      ceiling: Ceiling,
      workspace: Workspace,
      readRoots: Chunk[Workspace],
      writeRoots: Chunk[Workspace],
      files: FileRoots,
      names: Ref[Map[String, ToolName]],
      refusals: Ref[Map[String, String]],
      printed: Ref[Map[String, String]],
      hostCancel: Ref[Boolean],
      mcp: McpAccess,
      network: NetworkPolicy,
      sandbox: ShellSandbox,
      terminals: ShellTerminals,
  )

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

  def prompt(pipe: RpcPipe, expect: Json, scope: Scope): ZStream[Any, HarnessFailure, Event] =
    collect(
      pipe,
      expect,
      scope,
      (requestId, method, params) => answer(pipe, requestId, method, params, scope),
      (result, cancelled) => AcpEvents.stop(result, cancelled).map(outcome => Chunk(Event.TurnFinished(outcome))),
      code =>
        Right(Chunk(Event.TurnFinished(TurnOutcome.Failed(HarnessFailure.Protocol(ProtocolFault.Rejected(code)))))),
    )

  def load(pipe: RpcPipe, expect: Json, scope: Scope): ZStream[Any, HarnessFailure, Event] =
    collect(
      pipe,
      expect,
      scope,
      (requestId, method, params) => answer(pipe, requestId, method, params, scope),
      (_, _) => Right(Chunk.empty),
      code => Left(HarnessFailure.Protocol(ProtocolFault.Rejected(code))),
    )

  private def answer(
      pipe: RpcPipe,
      requestId: Json,
      method: String,
      params: Json,
      scope: Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    if method == "session/request_permission" then
      PermissionChoice
        .decide(
          scope.policy,
          scope.ceiling,
          scope.workspace,
          scope.readRoots,
          scope.writeRoots,
          params,
          scope.files,
          scope.mcp,
          scope.network,
          sandbox = scope.sandbox,
        )
        .flatMap { decision =>
          remember(scope, decision) *>
            pipe.write(Wire.ok(requestId, PermissionChoice.body(decision))).as(refused(params, decision))
        }
    else if AcpTerminal.isTerminal(method) then AcpTerminal.answer(pipe, requestId, method, params, scope)
    else pipe.write(Wire.fail(requestId, Wire.methodNotFound, "Method not found")).as(Chunk.empty)

  private def refused(params: Json, decision: PermissionAnswer): Chunk[Event] =
    decision match
      case PermissionAnswer.Refused(_, callId, reason) =>
        Chunk(Event.Refused(callId, PermissionChoice.statedName(params), reason))
      case _ => Chunk.empty

  private def remember(scope: Scope, decision: PermissionAnswer): IO[HarnessFailure, Unit] =
    decision match
      case PermissionAnswer.Refused(_, callId, reason) if callId.nonEmpty =>
        scope.refusals.update(_.updated(callId, reason))
      case _ => ZIO.unit

  private def collect(
      pipe: RpcPipe,
      expect: Json,
      scope: Scope,
      onCall: (Json, String, Json) => IO[HarnessFailure, Chunk[Event]],
      onOk: (Json, Boolean) => Either[ProtocolFault, Chunk[Event]],
      onErr: Int => Either[HarnessFailure, Chunk[Event]],
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      Ref.make(false).map { done =>
        ZStream.repeatZIOChunkOption {
          done.get.flatMap {
            case true  => ZIO.fail(None)
            case false =>
              step(pipe, expect, scope, onCall, onOk, onErr).mapError(Some(_)).flatMap {
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
      scope: Scope,
      onCall: (Json, String, Json) => IO[HarnessFailure, Chunk[Event]],
      onOk: (Json, Boolean) => Either[ProtocolFault, Chunk[Event]],
      onErr: Int => Either[HarnessFailure, Chunk[Event]],
  ): IO[HarnessFailure, Step] =
    pipe.read.flatMap {
      case None       => ZIO.fail(HarnessFailure.Protocol(ProtocolFault.Incomplete))
      case Some(json) =>
        ZIO.fromEither(Wire.parse(json)).mapError(HarnessFailure.Protocol(_)).flatMap {
          case Wire.Note(method, params) if Wire.isUpdate(method) =>
            scope.names.get.zip(scope.refusals.get).flatMap { (seen, refusals) =>
              ZIO
                .fromEither(AcpEvents.fromParams(params, seen, refusals))
                .mapError(HarnessFailure.Protocol(_))
                .flatMap { (next, events) =>
                  scope.printed.get.flatMap { printed =>
                    val (kept, trimmed) = ToolOutput.trim(printed, events)
                    scope.names.set(next) *> scope.printed.set(kept).as(Step.Emit(trimmed))
                  }
                }
            }
          case Wire.Note(_, _)                                  => ZIO.succeed(Step.Emit(Chunk.empty))
          case Wire.Call(requestId, method, params)             => onCall(requestId, method, params).map(Step.Emit(_))
          case Wire.Ok(got, result) if Wire.sameId(got, expect) =>
            scope.hostCancel.get.flatMap { cancelled =>
              ZIO.fromEither(onOk(result, cancelled)).mapBoth(HarnessFailure.Protocol(_), Step.Done(_))
            }
          case Wire.Err(got, code) if Wire.sameId(got, expect) =>
            ZIO.fromEither(onErr(code)).map(Step.Done(_))
          case Wire.Ok(_, _)  => ZIO.succeed(Step.Emit(Chunk.empty))
          case Wire.Err(_, _) => ZIO.succeed(Step.Emit(Chunk.empty))
        }
    }

  private enum Step:
    case Emit(events: Chunk[Event])
    case Done(events: Chunk[Event])
end AcpTurn
