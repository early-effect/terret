package terret.grok

import heddle.LinePipe
import terret.event.Event
import terret.harness.{Harness, HarnessFailure, ProtocolFault}
import terret.permission.{Ceiling, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.Transcript
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.{Chunk, IO, Promise, Random, Ref, Scope, UIO, ZIO, ZLayer}

final class GrokHarness private (
    config: GrokConfig,
    spawner: Spawner,
    acp: AcpOpen,
    scope: Scope,
    sessions: Ref[Map[SessionId, GrokHarness.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
) extends Harness:

  def open(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    config.transport match
      case GrokTransport.Cli => openCli(workspace, ceiling, policy)
      case _                 => openAcp(workspace, ceiling, policy)

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        Promise.make[HarnessFailure, Unit].flatMap { gate =>
          gates.update(_.updated(id, gate)).as {
            turn(id, slot, prompt).interruptWhen(gate).ensuring(gates.update(_.removed(id))).tap(append(id, _))
          }
        }
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        slot.pipe match
          case Some(pipe) if slot.events.isEmpty => reload(id, slot, pipe).map(_.tap(append(id, _)))
          case _                                 => ZIO.succeed(ZStream.fromChunk(slot.events))
      }
    }

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    require(id).map(slot => Transcript(id, slot.events))

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    require(id).flatMap { slot =>
      gates.get.flatMap(open => ZIO.foreachDiscard(open.get(id))(_.succeed(()))) *>
        ZIO.foreachDiscard(slot.pipe) { pipe =>
          pipe.write(Wire.notify("session/cancel", Json.Obj("sessionId" -> Json.Str(id.value)))).ignore
        }
    }

  private def openCli(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id   <- ZIO.fromEither(SessionId.from(uuid.toString)).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
      _ <- sessions.update(_.updated(id, GrokHarness.Slot.empty(workspace, ceiling, policy)))
    yield id

  private def openAcp(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): IO[HarnessFailure, SessionId] =
    scope.extend {
      for
        pipe   <- acp.open(workspace, policy, ceiling)
        result <- handshake(pipe, workspace, ceiling).tapError(_ => pipe.close)
        id     <- ZIO.fromEither(AcpEvents.sessionId(result)).mapError(HarnessFailure.Protocol(_))
        _      <- sessions.update(_.updated(id, GrokHarness.Slot.empty(workspace, ceiling, policy).copy(pipe = Some(pipe), nextId = 3)))
      yield id
    }

  private def handshake(pipe: RpcPipe, workspace: Workspace, ceiling: Ceiling): IO[HarnessFailure, Json] =
    for
      _      <- pipe.write(Wire.request(1, "initialize", AcpOpen.initialize(ceiling)))
      _      <- AcpTurn.await(pipe, 1)
      _      <- pipe.write(Wire.request(2, "session/new", AcpOpen.sessionNew(config, workspace)))
      result <- AcpTurn.await(pipe, 2)
    yield result

  private def turn(id: SessionId, slot: GrokHarness.Slot, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    slot.pipe match
      case None       => cliTurn(id, slot, prompt)
      case Some(pipe) => acpTurn(id, prompt, pipe, slot.policy, slot.ceiling)

  private def cliTurn(id: SessionId, slot: GrokHarness.Slot, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrapScoped {
      val argv = Launch.cli(config, id, prompt, slot.prompted, slot.policy, slot.ceiling)
      for
        spawned <- ZIO.acquireRelease(spawner.spawn(argv.command, argv.args, slot.workspace))(_.destroy)
        _       <- markPrompted(id)
        _       <- Spawner.drain(spawned.stderr).forkScoped
      yield CliTurn.events(LinePipe.streams(spawned.stdout, spawned.stdin), spawned.exit)
    }

  private def acpTurn(
      id: SessionId,
      prompt: Prompt,
      pipe: RpcPipe,
      policy: PermissionPolicy,
      ceiling: Ceiling,
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      nextId(id).map { number =>
        val expect = Json.Num(BigDecimal(number))
        ZStream.fromZIO(pipe.write(Wire.request(number, "session/prompt", promptBody(id, prompt)))) *>
          AcpTurn.prompt(pipe, expect, policy, ceiling)
      }
    }

  private def reload(id: SessionId, slot: GrokHarness.Slot, pipe: RpcPipe): ZIO[Any, HarnessFailure, ZStream[Any, HarnessFailure, Event]] =
    nextId(id).map { number =>
      val expect = Json.Num(BigDecimal(number))
      val body   = Json.Obj(
        "sessionId"  -> Json.Str(id.value),
        "cwd"        -> Json.Str(slot.workspace.path.toString),
        "mcpServers" -> Json.Arr(),
      )
      ZStream.fromZIO(pipe.write(Wire.request(number, "session/load", body))) *>
        AcpTurn.load(pipe, expect, slot.policy, slot.ceiling)
    }

  private def promptBody(id: SessionId, prompt: Prompt): Json =
    Json.Obj(
      "sessionId" -> Json.Str(id.value),
      "prompt"    -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(prompt.text))),
    )

  private def nextId(id: SessionId): IO[HarnessFailure, Int] =
    sessions.modify { rows =>
      rows.get(id) match
        case None       => (Left(HarnessFailure.UnknownSession(id)), rows)
        case Some(slot) => (Right(slot.nextId), rows.updated(id, slot.copy(nextId = slot.nextId + 1)))
    }.flatMap(ZIO.fromEither)

  private def markPrompted(id: SessionId): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None       => rows
        case Some(slot) => rows.updated(id, slot.copy(prompted = true))
    }

  private def append(id: SessionId, event: Event): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None       => rows
        case Some(slot) => rows.updated(id, slot.copy(events = slot.events :+ event))
    }

  private def require(id: SessionId): IO[HarnessFailure, GrokHarness.Slot] =
    sessions.get.map(_.get(id)).flatMap {
      case Some(slot) => ZIO.succeed(slot)
      case None       => ZIO.fail(HarnessFailure.UnknownSession(id))
    }
end GrokHarness

object GrokHarness:
  private[grok] final case class Slot(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      events: Chunk[Event],
      prompted: Boolean,
      pipe: Option[RpcPipe],
      nextId: Int,
  )

  private[grok] object Slot:
    def empty(workspace: Workspace, ceiling: Ceiling, policy: PermissionPolicy): Slot =
      Slot(workspace, ceiling, policy, Chunk.empty, prompted = false, pipe = None, nextId = 1)
  end Slot

  def make(config: GrokConfig, spawner: Spawner, acp: AcpOpen): ZIO[Scope, Nothing, GrokHarness] =
    for
      scope    <- ZIO.scope
      sessions <- Ref.make(Map.empty[SessionId, Slot])
      gates    <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
    yield GrokHarness(config, spawner, acp, scope, sessions, gates)

  def layer(config: GrokConfig): ZLayer[Scope, Nothing, Harness] =
    ZLayer.scoped {
      make(config, Spawner.jdk, AcpOpen.live(config, Spawner.jdk)).map(harness => harness: Harness)
    }
end GrokHarness
