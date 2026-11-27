package terret.app.watch

import heddle.http.Form
import terret.Terret
import terret.config.TerretConfig
import terret.event.{Event, TurnOutcome}
import terret.hooks.Hooks
import terret.memory.{InMemory, Script}
import terret.permission.{Ceiling, Permission, SessionTools, ToolGrant}
import terret.session.{SessionId, SessionState}
import terret.workspace.Workspace
import zio.stream.ZStream
import zio.test.*
import zio.{Chunk, Promise, Scope, ZIO}

import java.time.Instant

object SteerSpec extends ZIOSpecDefault:
  private val when = Instant.parse("2026-11-27T12:00:00Z")

  def spec = suite("watch steering")(
    test("a blank mcp field refuses every server and a bad token stays unspoken") {
      val blank = WatchForm.open(Form("workspace" -> "/allowed/proj"))
      val named = WatchForm.open(Form("workspace" -> "/allowed/proj", "mcp" -> "histos:version"))
      val bad   = WatchForm.open(Form("workspace" -> "/allowed/proj", "mcp" -> "!!!"))
      val allow = WatchForm.open(Form("workspace" -> "/allowed/proj", "allow" -> "ReadWorkspace,WriteWorkspace"))
      val odd   = WatchForm.open(Form("workspace" -> "/allowed/proj", "allow" -> "sudo"))
      assertTrue(
        blank.map(_.tools) == Right(SessionTools.refused),
        blank.map(_.allow) == Right(Ceiling.closed),
        blank.exists(_.model.isEmpty),
        named.map(_.tools) == Right(SessionTools.Chosen(Chunk(ToolGrant("histos", Some(Set("version")))))),
        bad == Left(SteerError.Mcp("bad token")),
        allow.exists(_.allow.allow == Set(Permission.ReadWorkspace, Permission.WriteWorkspace)),
        odd == Left(SteerError.BadAllow("sudo")),
      )
    },
    test("send waits for the running turn, and the audit time is the clock") {
      ZIO.scoped {
        for
          _       <- TestClock.setTime(when)
          hold    <- Promise.make[Nothing, Unit]
          opened  <- running(blocking(hold))
          (_, terret, steer, id) = opened
          queued  <- steer.send(id, "later", "viewer")
          waiting <- steer.queue(id)
          page    <- terret.page(id, None).mapError(_.toString)
          _       <- hold.succeed(())
          _       <- spin(terret, id) { events =>
            events.collect { case Event.Assistant(text) => text }.contains("later")
          }
          left <- steer.queue(id)
        yield assertTrue(
          queued == (),
          waiting.map(_.text) == Chunk("later"),
          page.events.map(_.event).exists {
            case Event.SteerQueued("viewer", _, "later", at) => at == when
            case _                                           => false
          },
          page.events.exists(_.at == when),
          left.isEmpty,
        )
      }
    },
    test("stop and send cancels the running turn and starts this prompt") {
      ZIO.scoped {
        for
          _      <- TestClock.setTime(when)
          hold   <- Promise.make[Nothing, Unit]
          opened <- running(blocking(hold))
          (memory, terret, steer, id) = opened
          _    <- steer.stopAndSend(id, "now", "viewer")
          _    <- spin(terret, id) { events =>
            events.collect { case Event.Assistant(text) => text }.contains("now")
          }
          page <- terret.page(id, None).mapError(_.toString)
          gone <- memory.cancelled
        yield assertTrue(
          page.events.map(_.event).exists {
            case Event.TurnCancelled("viewer", at) => at == when
            case _                                 => false
          },
          page.events.map(_.event).exists {
            case Event.SteerSent("viewer", "now", at) => at == when
            case _                                   => false
          },
          page.events.collect { case entry if entry.event == Event.Assistant("unblocked") => entry }.isEmpty,
          gone.exists(_.value == id.value),
        )
      }
    },
    test("cancel, resume, edit, and drop are audited or refused") {
      ZIO.scoped {
        for
          _      <- TestClock.setTime(when)
          hold   <- Promise.make[Nothing, Unit]
          opened <- running(blocking(hold))
          (_, terret, steer, id) = opened
          _     <- steer.send(id, "keep", "viewer")
          item  <- steer.queue(id).map(_.headOption)
          saved <- item match
            case Some(found) => steer.edit(id, found.id, "kept", "editor").as(found.id)
            case None        => ZIO.fail("no queued prompt")
          edited <- steer.queue(id)
          _      <- steer.drop(id, saved, "viewer")
          empty  <- steer.queue(id)
          missed <- steer.drop(id, saved, "viewer").either
          _      <- steer.cancel(id, "viewer")
          _      <- steer.resume(id, "viewer")
          page   <- terret.page(id, None).mapError(_.toString)
        yield assertTrue(
          edited.map(row => (row.id, row.text)) == Chunk(saved -> "kept"),
          empty.isEmpty,
          missed == Left(SteerError.UnknownItem(saved)),
          page.events.map(_.event).exists {
            case Event.TurnCancelled("viewer", at) => at == when
            case _                                 => false
          },
          page.events.map(_.event).exists {
            case Event.SessionResumed("viewer", at) => at == when
            case _                                  => false
          },
        )
      }
    },
    test("open admits a path under a root and rejects a parent segment") {
      ZIO.scoped {
        for
          _     <- TestClock.setTime(when)
          root  <- ZIO.fromEither(Workspace.from("/allowed")).mapError(_.toString)
          pair  <- opened(Script.echo, Chunk(root))
          (memory, _, steer) = pair
          good  <- steer.open(OpenRequest("/allowed/proj", None, Ceiling.closed, SessionTools.refused), "viewer")
          dots  <- steer.open(OpenRequest("/allowed/../etc", None, Ceiling.closed, SessionTools.refused), "viewer").either
          out   <- steer.open(OpenRequest("/etc", None, Ceiling.closed, SessionTools.refused), "viewer").either
          rel   <- steer.open(OpenRequest("proj", None, Ceiling.closed, SessionTools.refused), "viewer").either
          applied <- memory.applied(good.id).mapError(_.toString)
          page    <- pair._2.page(good.id, None).mapError(_.toString)
        yield assertTrue(
          good.workspace.path.toString == "/allowed/proj",
          applied.tools == SessionTools.refused,
          applied.ceiling == Ceiling.closed,
          dots == Left(SteerError.Workspace(WatchPathError.Dots)),
          out == Left(SteerError.Workspace(WatchPathError.Outside)),
          rel == Left(SteerError.Workspace(WatchPathError.Relative)),
          page.events.map(_.event).exists {
            case Event.SessionOpened("viewer", "/allowed/proj", None, at) => at == when
            case _                                                        => false
          },
        )
      }
    },
  )

  private def blocking(hold: Promise[Nothing, Unit]): Script =
    Script { (_, prompt) =>
      if prompt.text == "block" then
        ZStream.fromZIO(hold.await) *> ZStream(
          Event.Assistant("unblocked"),
          Event.TurnFinished(TurnOutcome.Completed),
        )
      else ZStream(Event.Assistant(prompt.text), Event.TurnFinished(TurnOutcome.Completed))
    }

  private def opened(
      script: Script,
      roots: Chunk[Workspace],
  ): ZIO[Scope, String, (InMemory, Terret, Steer)] =
    for
      memory <- InMemory.make(script)
      terret <- Terret.make(memory, TerretConfig.closed, Hooks.identity)
      steer  <- Steer.make(terret, roots)
    yield (memory, terret, steer)

  private def running(
      script: Script
  ): ZIO[Scope, String, (InMemory, Terret, Steer, SessionId)] =
    for
      space  <- ZIO.fromEither(Workspace.from("/work")).mapError(_.toString)
      triple <- opened(script, Chunk(space))
      (memory, terret, steer) = triple
      session <- terret.open(space).mapError(_.toString)
      _       <- steer.send(session.id, "block", "viewer").mapError(_.toString)
      _       <- spin(terret, session.id) { _ => true }
      state   <- terret.status(session.id).mapError(_.toString)
      _       <- ZIO.when(state.state != SessionState.Running)(ZIO.fail("turn did not start"))
    yield (memory, terret, steer, session.id)

  private def spin(terret: Terret, id: SessionId)(ready: Chunk[Event] => Boolean): ZIO[Any, String, Unit] =
    def go(left: Int): ZIO[Any, String, Unit] =
      terret.page(id, None).mapError(_.toString).flatMap { found =>
        val events = found.events.map(_.event)
        if ready(events) then ZIO.unit
        else if left <= 0 then ZIO.fail("turn did not settle")
        else ZIO.yieldNow *> go(left - 1)
      }
    go(10000)
end SteerSpec
