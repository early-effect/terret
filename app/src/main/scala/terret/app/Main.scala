package terret.app

import terret.dsh.DshHarness
import terret.grok.GrokHarness
import terret.harness.Harness
import zio.{ExitCode, Scope, ZIO, ZIOAppDefault, ZLayer}

object Main extends ZIOAppDefault:
  def run =
    val served =
      for
        parsed <- ZIO.config(ProcessConfig.raw).mapError(ProcessError.Config(_))
        config <- ZIO.fromEither(ProcessConfig.load(parsed))
        _      <- Process.listen(config).provideSomeLayer(wired(config))
      yield ()
    served.foldZIO(
      error => ZIO.consoleWith(_.printLineError(error.render)).as(ExitCode.failure),
      _ => ZIO.succeed(ExitCode.success),
    )

  private def wired(config: ProcessConfig): ZLayer[Scope, Nothing, Harness] =
    config.harness match
      case ProcessHarness.Grok(grok) => GrokHarness.layer(grok)
      case ProcessHarness.Dsh(dsh)   => DshHarness.layer(dsh)
end Main
