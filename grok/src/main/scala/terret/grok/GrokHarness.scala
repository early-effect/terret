package terret.grok

import heddle.LinePipe
import terret.event.{Event, ToolName, TurnOutcome}
import terret.harness.{Harness, HarnessFailure, ProtocolFault}
import terret.milestone.MilestoneSink
import terret.transcript.JournalBound
import terret.model.ModelKey
import terret.permission.{Ceiling, PermissionPolicy, SessionTools}
import terret.prompt.Prompt
import terret.session.SessionId
import terret.transcript.{Entry, Journal, Page, Retain, Transcript}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.{Take, ZStream}
import zio.{Chunk, IO, Promise, Queue, Random, Ref, Scope, UIO, ZIO, ZLayer}

final class GrokHarness private (
    config: GrokConfig,
    spawner: Spawner,
    acp: AcpOpen,
    scope: Scope,
    sessions: Ref[Map[SessionId, GrokHarness.Slot]],
    gates: Ref[Map[SessionId, Promise[HarnessFailure, Unit]]],
    journal: Journal,
    writeRoots: Chunk[Workspace],
    mcp: McpPolicy,
    catalog: McpCatalog,
    hosts: HostAllowlist,
    readRoots: Chunk[Workspace],
    files: FileRoots,
    sandbox: ShellSandbox,
    terminals: ShellTerminals,
) extends Harness:

  def open(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      model: Option[ModelKey] = None,
      tools: SessionTools = SessionTools.Unset,
  ): IO[HarnessFailure, SessionId] =
    val chosen = grantsOf(tools)
    config.transport match
      case GrokTransport.Cli => openCli(workspace, ceiling, policy, model, chosen)
      case _                 => openAcp(workspace, ceiling, policy, model, chosen)

  def prompt(id: SessionId, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        for
          gate     <- Promise.make[HarnessFailure, Unit]
          finished <- Ref.make(false)
          _        <- gates.update(_.updated(id, gate))
        yield watched(id, slot, prompt, gate, finished).ensuring(gates.update(_.removed(id)))
      }
    }

  def resume(id: SessionId): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      require(id).flatMap { slot =>
        journal.isEmpty(id).flatMap { empty =>
          slot.pipe match
            case Some(pipe) if empty =>
              reload(id, slot, pipe).map(stream => Retain.coalesce(stream)(append(id, _)).tap(append(id, _)))
            case _ => journal.events(id).map(ZStream.fromChunk(_))
        }
      }
    }

  def transcript(id: SessionId): IO[HarnessFailure, Transcript] =
    require(id) *> journal.events(id).map(Transcript(id, _))

  def markTurn(id: SessionId, turn: Long): UIO[Unit] =
    journal.mark(id, turn)

  def follow(id: SessionId, after: Option[Long]): ZStream[Any, HarnessFailure, Entry] =
    ZStream.unwrap(require(id).as(journal.follow(id, after)))

  def page(id: SessionId, after: Option[Long]): IO[HarnessFailure, Page] =
    require(id) *> journal.page(id, after).flatMap {
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(HarnessFailure.UnknownSession(id))
    }

  def note(id: SessionId, event: Event): UIO[Unit] =
    journal.append(id, event)

  def cancel(id: SessionId): IO[HarnessFailure, Unit] =
    require(id).flatMap { slot =>
      slot.hostCancel.set(true) *>
        gates.get.flatMap(open => ZIO.foreachDiscard(open.get(id))(_.succeed(()))) *>
        ZIO.foreachDiscard(slot.pipe) { pipe =>
          pipe.write(Wire.notify("session/cancel", Json.Obj("sessionId" -> Json.Str(id.value)))).ignore
        }
    }

  def close: UIO[Unit] =
    terminals.shutdown *> sessions.get.zip(gates.get).flatMap { (rows, open) =>
      ZIO.foreachDiscard(rows) { (id, slot) =>
        slot.hostCancel.set(true) *>
          ZIO.foreachDiscard(open.get(id))(_.succeed(())) *>
          ZIO.foreachDiscard(slot.pipe) { pipe =>
            pipe.write(Wire.notify("session/cancel", Json.Obj("sessionId" -> Json.Str(id.value)))).ignore *>
              pipe.close
          }
      }
    }

  private def openCli(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      model: Option[ModelKey],
      grants: McpPolicy,
  ): IO[HarnessFailure, SessionId] =
    for
      uuid <- Random.nextUUID
      id   <- ZIO
        .fromEither(SessionId.from(uuid.toString))
        .mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
      network    <- hosts.load
      names      <- Ref.make(Map.empty[String, ToolName])
      refusals   <- Ref.make(Map.empty[String, String])
      printed    <- Ref.make(Map.empty[String, String])
      hostCancel <- Ref.make(false)
      _          <- journal.open(id)
      _          <- sessions.update(
        _.updated(
          id,
          GrokHarness.Slot
            .empty(workspace, ceiling, policy, names, refusals, printed, hostCancel)
            .copy(network = network, model = model, grants = grants),
        )
      )
    yield id

  private def openAcp(
      workspace: Workspace,
      ceiling: Ceiling,
      policy: PermissionPolicy,
      model: Option[ModelKey],
      grants: McpPolicy,
  ): IO[HarnessFailure, SessionId] =
    scope.extend {
      for
        loaded <- catalog.load(workspace).mapError(_ => HarnessFailure.Protocol(ProtocolFault.Malformed))
        passed = grants.granted(loaded)
        network    <- hosts.load
        pipe       <- acp.open(workspace, policy, ceiling, grants, network, model)
        result     <- handshake(pipe, workspace, passed).tapError(_ => pipe.close)
        id         <- ZIO.fromEither(AcpEvents.sessionId(result)).mapError(HarnessFailure.Protocol(_))
        names      <- Ref.make(Map.empty[String, ToolName])
        refusals   <- Ref.make(Map.empty[String, String])
        printed    <- Ref.make(Map.empty[String, String])
        hostCancel <- Ref.make(false)
        _          <- journal.open(id)
        _          <- sessions.update(
          _.updated(
            id,
            GrokHarness.Slot
              .empty(workspace, ceiling, policy, names, refusals, printed, hostCancel)
              .copy(
                pipe = Some(pipe),
                nextId = 3,
                mcpServers = passed,
                network = network,
                model = model,
                grants = grants,
              ),
          )
        )
      yield id
    }

  private def handshake(pipe: RpcPipe, workspace: Workspace, servers: Chunk[McpServer]): IO[HarnessFailure, Json] =
    for
      _      <- pipe.write(Wire.request(1, "initialize", AcpOpen.initialize))
      _      <- AcpTurn.await(pipe, 1)
      _      <- pipe.write(Wire.request(2, "session/new", AcpOpen.sessionNew(config, workspace, servers)))
      result <- AcpTurn.await(pipe, 2)
    yield result

  // The producer is what offers the end marker. Joining it from the consumer that marker
  // resumes deadlocks, so the scope only signals interruption and does not wait.
  private def watched(
      id: SessionId,
      slot: GrokHarness.Slot,
      prompt: Prompt,
      gate: Promise[HarnessFailure, Unit],
      finished: Ref[Boolean],
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrapScoped {
      for
        queue <- Queue.unbounded[Take[HarnessFailure, Event]]
        fiber <- turnEvents(id, slot, prompt, gate)
          .runForeach { event =>
            ZIO.whenCase(event) { case Event.TurnFinished(_) => finished.set(true) } *>
              queue.offer(Take.single(event)).unit
          }
          .catchAllCause { cause =>
            slot.hostCancel.get.flatMap {
              case true  => ZIO.unit
              case false => queue.offer(Take.failCause(cause)).unit
            }
          }
          .ensuring {
            finished.get.zip(slot.hostCancel.get).flatMap { (done, cancelled) =>
              if !done && cancelled then
                val event = Event.TurnFinished(TurnOutcome.Cancelled)
                append(id, event) *> queue.offer(Take.single(event)).unit
              else ZIO.unit
            } *> queue.offer(Take.end)
          }
          .forkDaemon
        _ <- ZIO.addFinalizer(fiber.interruptFork)
      yield ZStream.fromQueue(queue).flattenTake
    }

  private def turnEvents(
      id: SessionId,
      slot: GrokHarness.Slot,
      prompt: Prompt,
      gate: Promise[HarnessFailure, Unit],
  ): ZStream[Any, HarnessFailure, Event] =
    Retain
      .coalesce(turn(id, slot, prompt).interruptWhen(gate))(append(id, _))
      .tap(append(id, _))

  private def turn(id: SessionId, slot: GrokHarness.Slot, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    slot.pipe match
      case None       => cliTurn(id, slot, prompt)
      case Some(pipe) => acpTurn(id, prompt, pipe, slot)

  private def cliTurn(id: SessionId, slot: GrokHarness.Slot, prompt: Prompt): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrapScoped {
      val argv =
        Launch.cli(
          config,
          id,
          prompt,
          slot.prompted,
          slot.policy,
          slot.ceiling,
          slot.grants,
          slot.network,
          slot.model,
        )
      for
        spawned <- ZIO.acquireRelease(spawner.spawn(argv.command, argv.args, slot.workspace))(_.destroy)
        _       <- markPrompted(id)
        _       <- Spawner.drain(spawned.stderr).forkScoped
      yield CliTurn.events(LinePipe.streams(spawned.stdout, spawned.stdin), spawned.exit, slot.hostCancel)
    }

  private def acpTurn(
      id: SessionId,
      prompt: Prompt,
      pipe: RpcPipe,
      slot: GrokHarness.Slot,
  ): ZStream[Any, HarnessFailure, Event] =
    ZStream.unwrap {
      nextId(id).map { number =>
        val expect = Json.Num(BigDecimal(number))
        ZStream.fromZIO(pipe.write(Wire.request(number, "session/prompt", promptBody(id, prompt)))) *>
          AcpTurn.prompt(pipe, expect, scope(slot))
      }
    }

  private def reload(
      id: SessionId,
      slot: GrokHarness.Slot,
      pipe: RpcPipe,
  ): ZIO[Any, HarnessFailure, ZStream[Any, HarnessFailure, Event]] =
    nextId(id).map { number =>
      val expect = Json.Num(BigDecimal(number))
      val body   = Json.Obj(
        "sessionId"  -> Json.Str(id.value),
        "cwd"        -> Json.Str(slot.workspace.path.toString),
        "mcpServers" -> McpServer.encodeAll(slot.mcpServers),
      )
      ZStream.fromZIO(pipe.write(Wire.request(number, "session/load", body))) *>
        AcpTurn.load(pipe, expect, scope(slot))
    }

  private def promptBody(id: SessionId, prompt: Prompt): Json =
    Json.Obj(
      "sessionId" -> Json.Str(id.value),
      "prompt"    -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(prompt.text))),
    )

  private def nextId(id: SessionId): IO[HarnessFailure, Int] =
    sessions
      .modify { rows =>
        rows.get(id) match
          case None       => (Left(HarnessFailure.UnknownSession(id)), rows)
          case Some(slot) => (Right(slot.nextId), rows.updated(id, slot.copy(nextId = slot.nextId + 1)))
      }
      .flatMap(ZIO.fromEither)

  private def markPrompted(id: SessionId): UIO[Unit] =
    sessions.update { rows =>
      rows.get(id) match
        case None       => rows
        case Some(slot) => rows.updated(id, slot.copy(prompted = true))
    }

  private def scope(slot: GrokHarness.Slot): AcpTurn.Scope =
    AcpTurn.Scope(
      slot.policy,
      slot.ceiling,
      slot.workspace,
      readRoots,
      writeRoots,
      files,
      slot.names,
      slot.refusals,
      slot.printed,
      slot.hostCancel,
      McpAccess(slot.grants, slot.mcpServers.map(_.server).toSet),
      slot.network,
      sandbox,
      terminals,
    )

  private def grantsOf(tools: SessionTools): McpPolicy =
    tools match
      case SessionTools.Unset          => mcp
      case SessionTools.Chosen(grants) => McpPolicy(grants.map(grant => McpGrant(grant.server, grant.tools)))

  private def append(id: SessionId, event: Event): UIO[Unit] =
    journal.append(id, event)

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
      prompted: Boolean,
      pipe: Option[RpcPipe],
      nextId: Int,
      names: Ref[Map[String, ToolName]],
      refusals: Ref[Map[String, String]],
      printed: Ref[Map[String, String]],
      hostCancel: Ref[Boolean],
      mcpServers: Chunk[McpServer] = Chunk.empty,
      network: NetworkPolicy = NetworkPolicy.empty,
      model: Option[ModelKey] = None,
      grants: McpPolicy = McpPolicy.empty,
  )

  private[grok] object Slot:
    def empty(
        workspace: Workspace,
        ceiling: Ceiling,
        policy: PermissionPolicy,
        names: Ref[Map[String, ToolName]],
        refusals: Ref[Map[String, String]],
        printed: Ref[Map[String, String]],
        hostCancel: Ref[Boolean],
    ): Slot =
      Slot(
        workspace,
        ceiling,
        policy,
        prompted = false,
        pipe = None,
        nextId = 1,
        names,
        refusals,
        printed,
        hostCancel,
      )

  def make(
      config: GrokConfig,
      spawner: Spawner,
      acp: AcpOpen,
      writeRoots: Chunk[Workspace] = Chunk.empty,
      mcp: McpPolicy = McpPolicy.empty,
      catalog: McpCatalog = McpCatalog.empty,
      hosts: HostAllowlist = HostAllowlist.closed,
      milestones: MilestoneSink = MilestoneSink.discard,
      journalBound: JournalBound = JournalBound.default,
      readRoots: Chunk[Workspace] = Chunk.empty,
      files: FileRoots = FileRoots.live,
      sandbox: ShellSandbox = ShellSandbox.live,
  ): ZIO[Scope, Nothing, GrokHarness] =
    for
      scope     <- ZIO.scope
      sessions  <- Ref.make(Map.empty[SessionId, Slot])
      gates     <- Ref.make(Map.empty[SessionId, Promise[HarnessFailure, Unit]])
      journal   <- Journal.make(milestones, journalBound)
      terminals <- ShellTerminals.make(sandbox)
      harness = GrokHarness(
        config,
        spawner,
        acp,
        scope,
        sessions,
        gates,
        journal,
        writeRoots,
        mcp,
        catalog,
        hosts,
        readRoots,
        files,
        sandbox,
        terminals,
      )
      _ <- ZIO.addFinalizer(harness.close)
    yield harness

  def layer(
      config: GrokConfig,
      writeRoots: Chunk[Workspace] = Chunk.empty,
      mcp: McpPolicy = McpPolicy.empty,
      catalog: McpCatalog = McpCatalog.live,
      network: NetworkPolicy = NetworkPolicy.empty,
      milestones: MilestoneSink = MilestoneSink.discard,
      journal: JournalBound = JournalBound.default,
      readRoots: Chunk[Workspace] = Chunk.empty,
  ): ZLayer[Scope, Nothing, Harness] =
    val services = ZLayer.succeed(config) ++ Spawner.layer
    (services ++ (services >>> AcpOpen.layer) ++ ShellSandbox.layer) >>> ZLayer.scoped {
      for
        acp     <- ZIO.service[AcpOpen]
        spawner <- ZIO.service[Spawner]
        sandbox <- ZIO.service[ShellSandbox]
        harness <- make(
          config,
          spawner,
          acp,
          writeRoots,
          mcp,
          catalog,
          HostAllowlist.fixed(network),
          milestones,
          journal,
          readRoots,
          sandbox = sandbox,
        )
      yield harness: Harness
    }
end GrokHarness
