package terret.app

import zio.{IO, Ref, ULayer, ZIO, ZLayer}

/** One child process, with no service manager. `command` is an executable path, not a shell line. */
final class ForegroundSupervisor private (command: String, child: Ref[Option[java.lang.Process]]) extends Supervisor:
  def stop: IO[SupervisorFailure, Unit] =
    child.modify(current => (current, None)).flatMap {
      case None          => ZIO.unit
      case Some(process) =>
        ZIO
          .attemptBlockingInterrupt {
            process.destroy()
            process.waitFor()
            ()
          }
          .mapError(_ => SupervisorFailure.Exit(SupervisorAction.Stop, 1))
    }

  def start: IO[SupervisorFailure, Unit] =
    child.get.flatMap {
      case Some(_) => ZIO.fail(SupervisorFailure.Busy)
      case None    =>
        ZIO
          .attemptBlockingInterrupt(new java.lang.ProcessBuilder(command).start())
          .mapError(_ => SupervisorFailure.Launch(SupervisorAction.Start))
          .flatMap(process => child.set(Some(process)))
    }

  def running: IO[SupervisorFailure, Boolean] =
    child.get.map {
      case None          => false
      case Some(process) => process.isAlive
    }
end ForegroundSupervisor

object ForegroundSupervisor:
  def layer(command: String): ULayer[Supervisor] =
    ZLayer.scoped {
      for
        child <- Ref.make(Option.empty[java.lang.Process])
        supervisor = ForegroundSupervisor(command, child)
        _ <- ZIO.addFinalizer(supervisor.stop.ignore)
      yield supervisor
    }
