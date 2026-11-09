package terret.dsh

import terret.harness.{HarnessFailure, ProtocolFault}
import terret.workspace.Workspace
import zio.{Chunk, IO, Schedule, UIO, ZIO}

import java.io.{InputStream, OutputStream}

trait Spawned:
  def stdin: OutputStream
  def stdout: InputStream
  def stderr: InputStream
  def exit: IO[HarnessFailure, Int]
  def destroy: UIO[Unit]
end Spawned

trait Spawner:
  def spawn(command: String, args: Chunk[String], cwd: Workspace): IO[HarnessFailure, Spawned]
end Spawner

object Spawner:
  def drain(in: InputStream): UIO[Unit] =
    ZIO.attemptBlocking(in.read()).repeat(Schedule.recurWhile(_ >= 0)).unit.catchAll(_ => ZIO.unit)

  val jdk: Spawner = JdkSpawner

  private object JdkSpawner extends Spawner:
    def spawn(command: String, args: Chunk[String], cwd: Workspace): IO[HarnessFailure, Spawned] =
      ZIO
        .attemptBlocking {
          val process = ProcessBuilder((command +: args).toList*).directory(cwd.path.toFile).start()
          new Spawned:
            def stdin: OutputStream           = process.getOutputStream
            def stdout: InputStream           = process.getInputStream
            def stderr: InputStream           = process.getErrorStream
            def exit: IO[HarnessFailure, Int] =
              ZIO.attemptBlocking(process.waitFor()).mapError(_ => HarnessFailure.Exited(-1))
            def destroy: UIO[Unit]            = ZIO.attemptBlocking(process.destroy()).ignore
        }
        .mapError(_ => HarnessFailure.Protocol(ProtocolFault.Incomplete))
end Spawner
