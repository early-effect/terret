package terret.app

import terret.grok.GrokHarness
import zio.{ExitCode, ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault:
  def run =
    val served =
      for
        parsed <- ZIO.config(ProcessConfig.raw).mapError(ProcessError.Config(_))
        config <- ZIO.fromEither(ProcessConfig.load(parsed))
        _      <- Process.listen(config).provideSomeLayer(GrokHarness.layer(config.grok))
      yield ()
    served.foldZIO(
      error => ZIO.consoleWith(_.printLineError(error.render)).as(ExitCode.failure),
      _ => ZIO.succeed(ExitCode.success),
    )
end Main
