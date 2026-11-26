package terret.find

import zio.ZIO

/** Runs the shared guard, then the runner. A refusal never reaches the runner. */
object Calls:
  def decide(
      server: Option[String],
      tool: String,
      access: Access,
  ): Either[CallError, (ServerName, ToolId)] =
    for
      name   <- ToolId.from(tool).left.map(_ => CallError.BadTool)
      ref    <- referenced(server, name)
      _      <- Guard(ref, access)
      target <- ref.server.toRight(CallError.NotCallable)
    yield (target, name)

  def invoke(
      server: Option[String],
      tool: String,
      arguments: String,
      access: Access,
  ): ZIO[Runner, CallError, String] =
    ZIO.fromEither(decide(server, tool, access)).flatMap { (name, id) =>
      Runner.call(name, id, arguments)
    }

  private def referenced(server: Option[String], tool: ToolId): Either[CallError, ToolRef] =
    server match
      case None      => Right(ToolRef(None, tool, ""))
      case Some(raw) =>
        ServerName.from(raw) match
          case Left(_)     => Left(CallError.NotCallable)
          case Right(name) => Right(ToolRef(Some(name), tool, ""))
end Calls
