package terret.hooks

import terret.error.TerretError
import terret.event.Event
import terret.prompt.Prompt
import terret.session.Session
import zio.{UIO, ZIO, ZLayer}

final case class Hooks(
    beforePrompt: (Session, Prompt) => ZIO[Any, TerretError, Prompt],
    onEvent: (Session, Event) => UIO[Unit],
)

object Hooks:
  val identity: Hooks = Hooks(
    (_, prompt) => ZIO.succeed(prompt),
    (_, _) => ZIO.unit,
  )

  val layer: ZLayer[Any, Nothing, Hooks] = ZLayer.succeed(identity)
end Hooks
