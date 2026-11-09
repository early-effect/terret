package terret

import terret.config.TerretConfig
import terret.error.TerretError
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.{SessionId, SessionStatus}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, Promise, Ref, ZIO, ZLayer}

object TerretSpec extends ZIOSpecDefault:
  private val readCall = ToolCall(CallId("c1"), ToolName("read"), Json.Str("body"))

  def spec = suite("Terret")(
    test("run records the turn, export matches, and resume does not prompt again") {
      for
        calls           <- Ref.make(0)
        (memory, terret, session) <- opened(counting(calls))
        streamed        <- terret.run(session.id, Prompt("hello")).runCollect
        exported        <- terret.transcript(session.id)
        replayed        <- terret.resume(session.id).runCollect
        seen            <- calls.get
        cancelled       <- memory.cancelled
      yield assertTrue(
        streamed == Chunk(
          Event.Assistant("hello"),
          Event.ToolStarted(readCall),
          Event.TurnFinished(TurnOutcome.Completed),
        ),
        exported.events == streamed,
        replayed == streamed,
        seen == 1,
        cancelled.isEmpty,
      )
    },
    test("a second run while the first is open is busy") {
      for
        entered                   <- Promise.make[Nothing, Unit]
        release                   <- Promise.make[Nothing, Unit]
        (_, terret, session)      <- opened(holding(entered, release))
        decision                  <- ZIO.scoped {
                                       terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
                                         entered.await *>
                                           terret.run(session.id, Prompt("no")).runDrain.flip.flatMap { error =>
                                             release.succeed(()) *> fiber.join.as(error)
                                           }
                                       }
                                     }
      yield assertTrue(decision == TerretError.Busy(session.id))
    },
    test("interrupting a run cancels the harness") {
      for
        entered              <- Promise.make[Nothing, Unit]
        release              <- Promise.make[Nothing, Unit]
        (memory, terret, session) <- opened(holding(entered, release))
        _                    <- ZIO.scoped {
                                  terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
                                    entered.await *> fiber.interrupt
                                  }
                                }
        cancelled            <- memory.cancelled
      yield assertTrue(cancelled.contains(session.id))
    },
    test("a hook rewrites the prompt and sees each event") {
      for
        seen                      <- Ref.make[Option[Prompt]](None)
        recorded                  <- Ref.make[Chunk[Event]](Chunk.empty)
        hooks = Hooks(
          (_, prompt) => ZIO.succeed(Prompt(prompt.text.toUpperCase)),
          (_, event) => recorded.update(_ :+ event),
        )
        script = Script { (_, prompt) =>
          ZStream.fromZIO(seen.set(Some(prompt))).drain ++
            ZStream(Event.Assistant(prompt.text), Event.TurnFinished(TurnOutcome.Completed))
        }
        (_, terret, session) <- opened(script, hooks = hooks)
        _                    <- terret.run(session.id, Prompt("hello")).runDrain
        prompt               <- seen.get
        events               <- recorded.get
      yield assertTrue(
        prompt.contains(Prompt("HELLO")),
        events == Chunk(Event.Assistant("HELLO"), Event.TurnFinished(TurnOutcome.Completed)),
      )
    },
    test("a rejected hook does not cancel, and a later run still starts") {
      for
        root    <- workspace("/work")
        memory  <- InMemory.make(Script.echo)
        once    <- Ref.make(false)
        hooks = Hooks(
          (_, prompt) =>
            once.modify {
              case false => (Left(TerretError.UnknownSession(SessionId("hook"))), true)
              case true  => (Right(prompt), true)
            }.flatMap(ZIO.fromEither),
          (_, _) => ZIO.unit,
        )
        terret                <- Terret.make(memory, TerretConfig.closed, hooks)
        session               <- terret.open(root)
        rejected              <- terret.run(session.id, Prompt("no")).runDrain.flip
        cancelled             <- memory.cancelled
        streamed              <- terret.run(session.id, Prompt("yes")).runCollect
      yield assertTrue(
        rejected == TerretError.UnknownSession(SessionId("hook")),
        cancelled.isEmpty,
        streamed == Chunk(Event.Assistant("yes"), Event.TurnFinished(TurnOutcome.Completed)),
      )
    },
    test("cancel stops the open turn") {
      for
        entered                   <- Promise.make[Nothing, Unit]
        release                   <- Promise.make[Nothing, Unit]
        (memory, terret, session) <- opened(holding(entered, release))
        _                         <- ZIO.scoped {
                                       terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
                                         entered.await *> terret.cancel(session.id) *> fiber.await
                                       }
                                     }
        cancelled                 <- memory.cancelled
      yield assertTrue(cancelled.contains(session.id))
    },
    test("open under the closed policy drops every permission except read") {
      for
        (memory, _, session) <- opened(Script.echo)
        applied              <- memory.applied(session.id)
      yield assertTrue(
        session.effective.allow == Set(Permission.ReadWorkspace),
        session.dropped == Set(Permission.WriteWorkspace, Permission.RunCommands, Permission.Network),
        session.status == SessionStatus.Open,
        applied.ceiling == Ceiling.closed,
        applied.policy == PermissionPolicy.Closed,
      )
    },
    test("open rejects a workspace outside the configured root") {
      for
        root     <- workspace("/tmp/terret-root")
        outside  <- workspace("/tmp/other")
        memory   <- InMemory.make(Script.echo)
        terret   <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, Some(root)), Hooks.identity)
        rejected <- terret.open(outside).flip
      yield assertTrue(rejected == TerretError.WorkspaceDenied(outside, root))
    },
    test("an unknown session fails before the harness") {
      for
        memory  <- InMemory.make(Script.echo)
        terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
        missing = SessionId("missing")
        rejected <- terret.transcript(missing).flip
        cancelled <- memory.cancelled
      yield assertTrue(rejected == TerretError.UnknownSession(missing), cancelled.isEmpty)
    },
    test("the layer opens a session under the closed policy") {
      def program =
        for
          root    <- workspace("/work")
          terret  <- ZIO.service[Terret]
          session <- terret.open(root)
        yield assertTrue(session.effective == Ceiling.closed, session.status == SessionStatus.Open)
      program.provide(Terret.layer, InMemory.layer(Script.echo), ZLayer.succeed(TerretConfig.closed), Hooks.layer)
    },
  )

  private def workspace(raw: String) =
    ZIO.fromEither(Workspace.from(raw).left.map(TerretError.WorkspaceInvalid(_)))

  private def opened(
      script: Script,
      config: TerretConfig = TerretConfig.closed,
      hooks: Hooks = Hooks.identity,
  ) =
    for
      memory  <- InMemory.make(script)
      terret  <- Terret.make(memory, config, hooks)
      root    <- workspace("/work")
      session <- terret.open(root)
    yield (memory, terret, session)

  private def counting(calls: Ref[Int]): Script =
    Script { (_, prompt) =>
      ZStream.fromZIO(calls.update(_ + 1)).drain ++
        ZStream(
          Event.Assistant(prompt.text),
          Event.ToolStarted(readCall),
          Event.TurnFinished(TurnOutcome.Completed),
        )
    }

  private def holding(entered: Promise[Nothing, Unit], release: Promise[Nothing, Unit]): Script =
    Script { (_, _) =>
      ZStream.fromZIO(entered.succeed(())).drain ++
        ZStream.fromZIO(release.await).drain ++
        ZStream.succeed(Event.TurnFinished(TurnOutcome.Completed))
    }
end TerretSpec
