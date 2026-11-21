package terret.app

import slayer.*
import zio.test.*
import zio.{Chunk, ConfigProvider, Duration, Exit, Ref, ZIO, durationInt}

import java.nio.file.Path

object RestartSpec extends ZIOSpecDefault:
  private val plan = RestartPlan(
    host = "127.0.0.1",
    port = 9,
    pace = 1.second,
    exitLimit = 5.seconds,
    portLimit = 5.seconds,
    listenLimit = 5.seconds,
  )

  def spec = suite("restart")(
    test("stop, wait, start, and report listening") {
      driven(Duration.Zero) {
        for
          started <- Ref.make(false)
          _       <- ready(exit = ZIO.succeed(true), free = ZIO.succeed(true), listening = ZIO.succeed(true))
          _       <- stub[Supervisor](_.start)(started.set(true))
          _       <- Restart.run(plan)
          called  <- started.get
        yield assertTrue(called)
      }
    },
    test("a slow exit waits out the pace and then starts") {
      driven(2.seconds) {
        for
          calls <- Ref.make(0)
          _     <- ready(
            exit = calls.updateAndGet(_ + 1).map(_ >= 3),
            free = ZIO.succeed(true),
            listening = ZIO.succeed(true),
          )
          _    <- Restart.run(plan)
          seen <- calls.get
        yield assertTrue(seen == 3)
      }
    },
    test("a port that never frees times out before start") {
      driven(plan.portLimit) {
        for
          started <- Ref.make(false)
          _       <- ready(exit = ZIO.succeed(true), free = ZIO.succeed(false), listening = ZIO.succeed(true))
          _       <- stub[Supervisor](_.start)(started.set(true))
          failed  <- Restart.run(plan).exit
          called  <- started.get
        yield assertTrue(
          failed == Exit.fail(RestartFailure.TimedOut(RestartStep.PortFree, plan.portLimit)),
          !called,
        )
      }
    },
    test("a start that never listens times out") {
      driven(plan.listenLimit) {
        for
          _      <- ready(exit = ZIO.succeed(true), free = ZIO.succeed(true), listening = ZIO.succeed(false))
          failed <- Restart.run(plan).exit
        yield assertTrue(failed == Exit.fail(RestartFailure.TimedOut(RestartStep.Listening, plan.listenLimit)))
      }
    },
    test("a failing supervisor does not start") {
      driven(Duration.Zero) {
        for
          started <- Ref.make(false)
          _       <- ready(exit = ZIO.succeed(true), free = ZIO.succeed(true), listening = ZIO.succeed(true))
          _       <- stub[Supervisor](_.stop)(ZIO.fail(SupervisorFailure.Exit(SupervisorAction.Stop, 7)))
          _       <- stub[Supervisor](_.start)(started.set(true))
          failed  <- Restart.run(plan).exit
          called  <- started.get
        yield assertTrue(
          failed == Exit.fail(RestartFailure.Stop(SupervisorFailure.Exit(SupervisorAction.Stop, 7))),
          !called,
        )
      }
    },
    test("no arguments serves, and restart is the other command") {
      ZIO.succeed {
        assertTrue(
          Command.parse(Chunk.empty) == Right(Command.Serve),
          Command.parse(Chunk("restart")) == Right(Command.Restart),
          Command.parse(Chunk("nope")) == Left(CommandError.Unknown("nope")),
        )
      }
    },
    test("stdio has no port to restart") {
      assertTrue(Restart.plan(Listen.Stdio, RestartBudget.default) == Left(RestartFailure.NotHttp))
    },
    test("none is the default, and launchd labels come only from config") {
      val missing = ServiceConfig.load(ServiceConfig.defaultRaw.copy(supervisor = "launchd"))
      val loaded  = ServiceConfig.load(
        ServiceConfig.defaultRaw.copy(
          supervisor = "launchd",
          labels = Some("rocks.earlyeffect.terret, rocks.earlyeffect.terret-tunnel"),
          plistDir = Some("/Users/russ/Library/LaunchAgents"),
        )
      )
      val foreground = ServiceConfig.load(ServiceConfig.defaultRaw.copy(supervisor = "foreground"))
      val unknown    = ServiceConfig.load(ServiceConfig.defaultRaw.copy(supervisor = "systemd"))
      assertTrue(
        ServiceConfig.load(ServiceConfig.defaultRaw).map(_.supervisor) == Right(SupervisorChoice.None),
        missing == Left(ServiceError.LabelsMissing),
        foreground == Left(ServiceError.CommandMissing),
        unknown == Left(ServiceError.Supervisor("systemd")),
        labelsOf(loaded) == Right(Chunk("rocks.earlyeffect.terret", "rocks.earlyeffect.terret-tunnel")),
        dirOf(loaded) == Right(Path.of("/Users/russ/Library/LaunchAgents")),
      )
    },
    test("TERRET_SUPERVISOR selects the supervisor") {
      ZIO
        .config(ServiceConfig.raw)
        .flatMap(raw => ZIO.fromEither(ServiceConfig.load(raw)))
        .withConfigProvider(
          ConfigProvider.fromMap(
            Map(
              "terret.supervisor"         -> "foreground",
              "terret.foreground.command" -> "/bin/true",
            )
          )
        )
        .map { config =>
          assertTrue(config.supervisor == SupervisorChoice.Foreground("/bin/true"))
        }
    },
  ) @@ TestAspect.sequential

  private def ready(
      exit: ZIO[Any, LivenessFailure, Boolean],
      free: ZIO[Any, PortProbeFailure, Boolean],
      listening: ZIO[Any, PortProbeFailure, Boolean],
  ) =
    stub[Supervisor](_.stop)(ZIO.unit) *>
      stub[Supervisor](_.start)(ZIO.unit) *>
      stub[Liveness](_.exited)(exit) *>
      stub[PortProbe](_.free(any[String], any[Int]))(free) *>
      stub[PortProbe](_.listening(any[String], any[Int]))(listening)

  private def driven[E, A](waited: Duration)(
      effect: ZIO[
        Supervisor & Stubbed[Supervisor] & Liveness & Stubbed[Liveness] & PortProbe & Stubbed[PortProbe],
        E,
        A,
      ]
  ) =
    (for
      fiber <- effect.fork
      _     <- TestClock.adjust(waited)
      value <- fiber.join
    yield value).provide(stubbed[Supervisor], stubbed[Liveness], stubbed[PortProbe])

  private def labelsOf(loaded: Either[ServiceError, ServiceConfig]): Either[ServiceError, Chunk[String]] =
    loaded.map {
      case ServiceConfig(SupervisorChoice.Launchd(jobs), _, _) => jobs.labels.map(_.value).toChunk
      case _                                                   => Chunk.empty
    }

  private def dirOf(loaded: Either[ServiceError, ServiceConfig]): Either[ServiceError, Path] =
    loaded.flatMap {
      case ServiceConfig(SupervisorChoice.Launchd(jobs), _, _) => Right(jobs.plistDir)
      case _                                                   => Left(ServiceError.LabelsMissing)
    }
end RestartSpec
