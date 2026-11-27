package terret

import terret.config.TerretConfig
import terret.model.ModelCatalog
import terret.error.TerretError
import terret.event.{CallId, Event, ToolCall, ToolName, TurnOutcome}
import terret.harness.HarnessFailure
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.prompt.Prompt
import terret.session.{Session, SessionId, SessionState}
import terret.transcript.Entry
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, Promise, Ref, Scope, ZIO, ZLayer}
import zio.durationInt

object TerretSpec extends ZIOSpecDefault:
  private val readCall = ToolCall(CallId("c1"), ToolName("read"), Json.Str("body"))

  def spec = suite("Terret")(
    test("run records the turn, export matches, and resume does not prompt again") {
      ZIO.scoped {
        for
          calls                     <- Ref.make(0)
          (memory, terret, session) <- opened(counting(calls))
          streamed                  <- terret.run(session.id, Prompt("hello")).runCollect
          exported                  <- terret.transcript(session.id)
          replayed                  <- terret.resume(session.id).runCollect
          seen                      <- calls.get
          cancelled                 <- memory.cancelled
          held                      <- terret.list
        yield assertTrue(
          held.map(_.id) == Chunk(session.id),
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
      }
    },
    test("a second run while the first is open is busy") {
      ZIO.scoped {
        for
          entered              <- Promise.make[Nothing, Unit]
          release              <- Promise.make[Nothing, Unit]
          (_, terret, session) <- opened(holding(entered, release))
          decision             <- ZIO.scoped {
            terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
              entered.await *>
                terret.run(session.id, Prompt("no")).runDrain.flip.flatMap { error =>
                  release.succeed(()) *> fiber.join.as(error)
                }
            }
          }
        yield assertTrue(decision == TerretError.Busy(session.id))
      }
    },
    test("interrupting a run cancels the harness") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(holding(entered, release))
          _                         <- ZIO.scoped {
            terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
              entered.await *> fiber.interrupt
            }
          }
          cancelled <- memory.cancelled
        yield assertTrue(cancelled.contains(session.id))
      }
    },
    test("a second start while the first is open is busy") {
      ZIO.scoped {
        for
          entered              <- Promise.make[Nothing, Unit]
          release              <- Promise.make[Nothing, Unit]
          (_, terret, session) <- opened(holding(entered, release))
          _                    <- terret.start(session.id, Prompt("stay"))
          _                    <- entered.await
          busy                 <- terret.start(session.id, Prompt("no")).flip
          _                    <- release.succeed(())
        yield assertTrue(busy == TerretError.Busy(session.id))
      }
    },
    test("interrupting the caller of start does not cancel the harness") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(holding(entered, release))
          _                         <- terret.start(session.id, Prompt("stay")).fork.flatMap { fiber =>
            entered.await *> fiber.interrupt
          }
          mid   <- memory.cancelled
          _     <- release.succeed(())
          ended <- terret
            .status(session.id)
            .repeatUntil(_.state == SessionState.Ended(TurnOutcome.Completed))
          log <- terret.transcript(session.id)
        yield assertTrue(
          mid.isEmpty,
          ended.state == SessionState.Ended(TurnOutcome.Completed),
          log.events == Chunk(Event.TurnFinished(TurnOutcome.Completed)),
        )
      }
    },
    test("closing Terret's scope cancels a started turn") {
      for
        entered <- Promise.make[Nothing, Unit]
        release <- Promise.make[Nothing, Unit]
        memory  <- InMemory.make(holding(entered, release))
        id      <- ZIO.scoped {
          for
            terret  <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
            root    <- workspace("/work")
            session <- terret.open(root)
            _       <- terret.start(session.id, Prompt("stay"))
            _       <- entered.await
          yield session.id
        }
        cancelled <- memory.cancelled
      yield assertTrue(cancelled.contains(id))
    },
    test("a hook rewrites the prompt and sees each event") {
      ZIO.scoped {
        for
          seen     <- Ref.make[Option[Prompt]](None)
          recorded <- Ref.make[Chunk[Event]](Chunk.empty)
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
      }
    },
    test("a rejected hook does not cancel, and a later run still starts") {
      ZIO.scoped {
        for
          root   <- workspace("/work")
          memory <- InMemory.make(Script.echo)
          once   <- Ref.make(false)
          hooks = Hooks(
            (_, prompt) =>
              once
                .modify {
                  case false => (Left(TerretError.UnknownSession(SessionId("hook"))), true)
                  case true  => (Right(prompt), true)
                }
                .flatMap(ZIO.fromEither),
            (_, _) => ZIO.unit,
          )
          terret    <- Terret.make(memory, TerretConfig.closed, hooks)
          session   <- terret.open(root)
          rejected  <- terret.run(session.id, Prompt("no")).runDrain.flip
          cancelled <- memory.cancelled
          streamed  <- terret.run(session.id, Prompt("yes")).runCollect
        yield assertTrue(
          rejected == TerretError.UnknownSession(SessionId("hook")),
          cancelled.isEmpty,
          streamed == Chunk(Event.Assistant("yes"), Event.TurnFinished(TurnOutcome.Completed)),
        )
      }
    },
    test("a harness failure ends the turn, and a later start still runs") {
      ZIO.scoped {
        for
          once <- Ref.make(false)
          script = Script { (_, prompt) =>
            ZStream.unwrap(
              once.modify {
                case false => (ZStream.fail(HarnessFailure.Exited(7)), true)
                case true  =>
                  (ZStream(Event.Assistant(prompt.text), Event.TurnFinished(TurnOutcome.Completed)), true)
              }
            )
          }
          (memory, terret, session) <- opened(script)
          _                         <- terret.start(session.id, Prompt("no"))
          ended                     <- terret
            .status(session.id)
            .repeatUntil(_.state == SessionState.Ended(TurnOutcome.Failed(HarnessFailure.Exited(7))))
          cancelled <- memory.cancelled
          _         <- terret.start(session.id, Prompt("yes"))
          done      <- terret
            .status(session.id)
            .repeatUntil(_.state == SessionState.Ended(TurnOutcome.Completed))
          log <- terret.transcript(session.id)
        yield assertTrue(
          ended.state == SessionState.Ended(TurnOutcome.Failed(HarnessFailure.Exited(7))),
          cancelled.isEmpty,
          done.state == SessionState.Ended(TurnOutcome.Completed),
          log.events == Chunk(Event.Assistant("yes"), Event.TurnFinished(TurnOutcome.Completed)),
        )
      }
    },
    test("a rejected hook on start does not cancel, and a later start still runs") {
      ZIO.scoped {
        for
          root   <- workspace("/work")
          memory <- InMemory.make(Script.echo)
          once   <- Ref.make(false)
          hooks = Hooks(
            (_, prompt) =>
              once
                .modify {
                  case false => (Left(TerretError.UnknownSession(SessionId("hook"))), true)
                  case true  => (Right(prompt), true)
                }
                .flatMap(ZIO.fromEither),
            (_, _) => ZIO.unit,
          )
          terret    <- Terret.make(memory, TerretConfig.closed, hooks)
          session   <- terret.open(root)
          rejected  <- terret.start(session.id, Prompt("no")).flip
          after     <- terret.status(session.id)
          cancelled <- memory.cancelled
          _         <- terret.start(session.id, Prompt("yes"))
          ended     <- terret.status(session.id).repeatUntil(_.state == SessionState.Ended(TurnOutcome.Completed))
          log       <- terret.transcript(session.id)
        yield assertTrue(
          rejected == TerretError.UnknownSession(SessionId("hook")),
          after.state == SessionState.Open,
          cancelled.isEmpty,
          ended.state == SessionState.Ended(TurnOutcome.Completed),
          log.events == Chunk(Event.Assistant("yes"), Event.TurnFinished(TurnOutcome.Completed)),
        )
      }
    },
    test("cancel stops the open turn") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(holding(entered, release))
          _                         <- ZIO.scoped {
            terret.run(session.id, Prompt("stay")).runDrain.forkScoped.flatMap { fiber =>
              entered.await *> terret.cancel(session.id) *> fiber.await
            }
          }
          cancelled <- memory.cancelled
        yield assertTrue(cancelled.contains(session.id))
      }
    },
    test("open under the closed policy drops every permission except read") {
      ZIO.scoped {
        for
          (memory, _, session) <- opened(Script.echo)
          applied              <- memory.applied(session.id)
        yield assertTrue(
          session.effective.allow == Set(Permission.ReadWorkspace),
          session.dropped == Set(Permission.WriteWorkspace, Permission.RunCommands, Permission.Network),
          session.state == SessionState.Open,
          applied.ceiling == Ceiling.closed,
          applied.policy == PermissionPolicy.Closed,
        )
      }
    },
    test("open rejects a workspace outside the configured root") {
      ZIO.scoped {
        for
          root     <- workspace("/tmp/terret-root")
          outside  <- workspace("/tmp/other")
          memory   <- InMemory.make(Script.echo)
          terret   <- Terret.make(memory, TerretConfig(PermissionPolicy.Closed, Some(root)), Hooks.identity)
          rejected <- terret.open(outside).flip
        yield assertTrue(rejected == TerretError.WorkspaceDenied(outside, root))
      }
    },
    test("an unknown session fails before the harness") {
      ZIO.scoped {
        for
          memory <- InMemory.make(Script.echo)
          terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
          missing = SessionId("missing")
          rejected  <- terret.transcript(missing).flip
          cancelled <- memory.cancelled
        yield assertTrue(rejected == TerretError.UnknownSession(missing), cancelled.isEmpty)
      }
    },
    test("cancel flushes an open text run onto an attached follow") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(held(entered, release))
          followed                  <- terret.follow(session.id, None).take(1).runCollect.fork
          _                         <- terret.start(session.id, Prompt("held"))
          _                         <- entered.await
          _                         <- terret.cancel(session.id)
          got                       <- followed.join
          page                      <- terret.page(session.id, None)
          cancelled                 <- memory.cancelled
        yield assertTrue(
          got == Chunk(Entry(0, Event.Assistant("held"))),
          page.events == got,
          page.cursor.contains(0L),
          cancelled.contains(session.id),
        )
      }
    } @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock,
    test("closing a follow does not cancel the turn") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(holding(entered, release))
          _                         <- terret.start(session.id, Prompt("stay"))
          _                         <- entered.await
          _                         <- ZIO.scoped(terret.follow(session.id, None).runDrain.forkScoped)
          mid                       <- memory.cancelled
          _                         <- release.succeed(())
          ended                     <- awaitEnded(terret, session.id)
          cancelled                 <- memory.cancelled
          log                       <- terret.transcript(session.id)
        yield assertTrue(
          mid.isEmpty,
          ended.state == SessionState.Ended(TurnOutcome.Completed),
          cancelled.isEmpty,
          log.events == Chunk(Event.TurnFinished(TurnOutcome.Completed)),
        )
      }
    } @@ TestAspect.timeout(10.seconds) @@ TestAspect.withLiveClock,
    test("shutdown cancels a started turn and closes the harness") {
      ZIO.scoped {
        for
          entered                   <- Promise.make[Nothing, Unit]
          release                   <- Promise.make[Nothing, Unit]
          (memory, terret, session) <- opened(holding(entered, release))
          _                         <- terret.start(session.id, Prompt("stay"))
          _                         <- entered.await
          _                         <- terret.shutdown
          cancelled                 <- memory.cancelled
          status                    <- terret.status(session.id)
        yield assertTrue(
          cancelled.contains(session.id),
          status.state == SessionState.Ended(TurnOutcome.Cancelled),
        )
      }
    },
    test("the layer opens a session under the closed policy") {
      def program =
        for
          root    <- workspace("/work")
          terret  <- ZIO.service[Terret]
          session <- terret.open(root)
        yield assertTrue(session.effective == Ceiling.closed, session.state == SessionState.Open)
      program.provide(
        Terret.layer,
        InMemory.layer(Script.echo),
        ZLayer.succeed(TerretConfig.closed),
        Hooks.layer,
        ModelCatalog.emptyLayer,
      )
    },
  )

  private def workspace(raw: String) =
    ZIO.fromEither(Workspace.from(raw).left.map(TerretError.WorkspaceInvalid(_)))

  private def opened(
      script: Script,
      config: TerretConfig = TerretConfig.closed,
      hooks: Hooks = Hooks.identity,
  ): ZIO[Scope, TerretError, (InMemory, Terret, Session)] =
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

  private def held(entered: Promise[Nothing, Unit], release: Promise[Nothing, Unit]): Script =
    Script { (_, prompt) =>
      ZStream(Event.Assistant(prompt.text)) ++
        ZStream.fromZIO(entered.succeed(()) *> release.await).drain
    }

  private def awaitEnded(terret: Terret, id: SessionId): ZIO[Any, TerretError, Session] =
    terret.status(id).flatMap { session =>
      session.state match
        case SessionState.Ended(_) => ZIO.succeed(session)
        case _                     => ZIO.yieldNow *> awaitEnded(terret, id)
    }
end TerretSpec
