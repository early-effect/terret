package terret.app

import zio.{IO, ULayer, ZIO, ZLayer}

/** Starts and stops the server process. Launchd, systemd, and a foreground child are layers behind this. */
trait Supervisor:
  def stop: IO[SupervisorFailure, Unit]
  def start: IO[SupervisorFailure, Unit]
  def running: IO[SupervisorFailure, Boolean]

enum SupervisorAction:
  case Stop
  case Start
  case Status

  def render: String = this match
    case Stop   => "stop"
    case Start  => "start"
    case Status => "status"
end SupervisorAction

enum SupervisorFailure:
  case Exit(action: SupervisorAction, code: Int)
  case Launch(action: SupervisorAction)
  case Busy

  def render: String = this match
    case Exit(action, code) => s"${action.render} exited $code"
    case Launch(action)     => s"could not launch ${action.render}"
    case Busy               => "a foreground server is already running"
end SupervisorFailure

object Supervisor:
  val none: ULayer[Supervisor] =
    ZLayer.succeed(new Supervisor:
      def stop: IO[SupervisorFailure, Unit]       = ZIO.unit
      def start: IO[SupervisorFailure, Unit]      = ZIO.unit
      def running: IO[SupervisorFailure, Boolean] = ZIO.succeed(false))

  def layer(config: ServiceConfig): ULayer[Supervisor] =
    config.supervisor match
      case SupervisorChoice.None                => none
      case SupervisorChoice.Launchd(jobs)       => LaunchdSupervisor.layer(jobs)
      case SupervisorChoice.Foreground(command) => ForegroundSupervisor.layer(command)
end Supervisor
