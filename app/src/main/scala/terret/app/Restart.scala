package terret.app

import zio.{Duration, Schedule, ZIO}

enum RestartStep:
  case Exit
  case PortFree
  case Listening

  def render: String = this match
    case Exit      => "process exit"
    case PortFree  => "a free port"
    case Listening => "listening"
end RestartStep

enum RestartFailure:
  case Stop(failure: SupervisorFailure)
  case Start(failure: SupervisorFailure)
  case ExitProbe(failure: LivenessFailure)
  case Port(step: RestartStep, failure: PortProbeFailure)
  case TimedOut(step: RestartStep, waited: Duration)
  case NotHttp

  def render: String = this match
    case Stop(failure)          => s"stop failed: ${failure.render}"
    case Start(failure)         => s"start failed: ${failure.render}"
    case ExitProbe(failure)     => s"exit probe failed: ${failure.render}"
    case Port(step, failure)    => s"${step.render} probe failed: ${failure.render}"
    case TimedOut(step, waited) => s"timed out waiting for ${step.render} after ${waited.toMillis} ms"
    case NotHttp                => "restart needs an http listener"
end RestartFailure

final case class RestartPlan(
    host: String,
    port: Int,
    pace: Duration,
    exitLimit: Duration,
    portLimit: Duration,
    listenLimit: Duration,
)

object Restart:
  def plan(listen: Listen, budget: RestartBudget): Either[RestartFailure, RestartPlan] =
    listen match
      case Listen.Stdio            => Left(RestartFailure.NotHttp)
      case Listen.Http(host, port) =>
        Right(RestartPlan(host, port, budget.pace, budget.exit, budget.port, budget.listen))

  def run(plan: RestartPlan): ZIO[Supervisor & Liveness & PortProbe, RestartFailure, Unit] =
    for
      _ <- ZIO.serviceWithZIO[Supervisor](_.stop).mapError(RestartFailure.Stop(_))
      _ <- await(exited, RestartStep.Exit, plan.pace, plan.exitLimit)
      _ <- await(free(plan), RestartStep.PortFree, plan.pace, plan.portLimit)
      _ <- ZIO.serviceWithZIO[Supervisor](_.start).mapError(RestartFailure.Start(_))
      _ <- await(listening(plan), RestartStep.Listening, plan.pace, plan.listenLimit)
    yield ()

  private def exited: ZIO[Liveness, RestartFailure, Boolean] =
    ZIO.serviceWithZIO[Liveness](_.exited).mapError(RestartFailure.ExitProbe(_))

  private def free(plan: RestartPlan): ZIO[PortProbe, RestartFailure, Boolean] =
    ZIO
      .serviceWithZIO[PortProbe](_.free(plan.host, plan.port))
      .mapError(RestartFailure.Port(RestartStep.PortFree, _))

  private def listening(plan: RestartPlan): ZIO[PortProbe, RestartFailure, Boolean] =
    ZIO
      .serviceWithZIO[PortProbe](_.listening(plan.host, plan.port))
      .mapError(RestartFailure.Port(RestartStep.Listening, _))

  private def await[R](
      ready: ZIO[R, RestartFailure, Boolean],
      step: RestartStep,
      pace: Duration,
      limit: Duration,
  ): ZIO[R, RestartFailure, Unit] =
    ready
      .repeat(Schedule.upTo(limit) *> Schedule.spaced(pace) *> Schedule.recurUntil[Boolean](identity))
      .flatMap { done =>
        if done then ZIO.unit else ZIO.fail(RestartFailure.TimedOut(step, limit))
      }
end Restart
