package terret.app

import terret.dsh.DshHarness
import terret.grok.{GrokHarness, GrokModels, SandboxError}
import terret.harness.Harness
import terret.milestone.MilestoneGate
import zio.{ExitCode, Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

object Main extends ZIOAppDefault:
  def run =
    ZIOAppArgs.getArgs.flatMap { args =>
      Command.parse(args) match
        case Left(error)            => report(error.render)
        case Right(Command.Serve)   => serve
        case Right(Command.Restart) => restart
    }

  private def serve =
    val served =
      for
        parsed  <- ZIO.config(ProcessConfig.raw).mapError(error => AppFailure.Process(ProcessError.Config(error)))
        config  <- ZIO.fromEither(ProcessConfig.load(parsed)).mapError(AppFailure.Process(_))
        service <- ZIO.config(ServiceConfig.raw).mapError(error => AppFailure.Service(ServiceError.Config(error)))
        loaded  <- ZIO.fromEither(ServiceConfig.load(service)).mapError(AppFailure.Service(_))
        _       <- PidFile.write(loaded.pidFile).mapError(AppFailure.Service(_))
        gate    <- MilestoneGate.make
        _       <- Process
          .listen(config.copy(subscriptions = Subscriptions.beside(loaded.pidFile)), gate)
          .provideSomeLayer(wired(config, gate) ++ GrokModels.layer(config.grokConfig))
          .mapError {
            case error: ProcessError => AppFailure.Process(error)
            case error: SandboxError => AppFailure.Sandbox(error)
          }
      yield ()
    served.foldZIO(
      error => report(error.render),
      _ => ZIO.succeed(ExitCode.success),
    )

  private def restart =
    val ran =
      for
        parsed  <- ZIO.config(ProcessConfig.raw).mapError(error => AppFailure.Process(ProcessError.Config(error)))
        process <- ZIO.fromEither(ProcessConfig.load(parsed)).mapError(AppFailure.Process(_))
        service <- ZIO.config(ServiceConfig.raw).mapError(error => AppFailure.Service(ServiceError.Config(error)))
        loaded  <- ZIO.fromEither(ServiceConfig.load(service)).mapError(AppFailure.Service(_))
        plan    <- ZIO.fromEither(Restart.plan(process.listen, loaded.budget)).mapError(AppFailure.Restart(_))
        _       <- Restart
          .run(plan)
          .provide(Supervisor.layer(loaded), Liveness.layer(loaded.pidFile), PortProbe.live)
          .mapError(AppFailure.Restart(_))
        _ <- ZIO.consoleWith(_.printLine(s"terret listening on ${plan.host}:${plan.port}")).ignore
      yield ()
    ran.foldZIO(
      error => report(error.render),
      _ => ZIO.succeed(ExitCode.success),
    )

  private def report(message: String): UIO[ExitCode] =
    // Returning ExitCode.failure is not enough: a recovered failure is a successful fiber, and the process
    // exits 0. launchd KeepAlive with SuccessfulExit=false then leaves it down. exit forces the status.
    ZIO.consoleWith(_.printLineError(message)).orDie *>
      exit(ExitCode.failure).as(ExitCode.failure)

  private def wired(config: ProcessConfig, milestones: MilestoneGate): ZLayer[Scope, SandboxError, Harness] =
    config.harness match
      case ProcessHarness.Grok(grok) =>
        GrokHarness.layer(
          grok,
          config.writeRoots,
          config.mcp,
          network = config.network,
          milestones = milestones,
          journal = config.journal,
          readRoots = config.readRoots,
        )
      case ProcessHarness.Dsh(dsh) => DshHarness.layer(dsh, milestones, config.journal)
end Main

enum AppFailure:
  case Process(error: ProcessError)
  case Service(error: ServiceError)
  case Restart(error: RestartFailure)
  case Sandbox(error: SandboxError)

  def render: String = this match
    case Process(error) => error.render
    case Service(error) => error.render
    case Restart(error) => error.render
    case Sandbox(error) => error.explain("shell")
end AppFailure
