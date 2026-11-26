package terret.find

/** The one check `find` and `call` share.
  *
  * No server is `NotCallable`. A grant miss is `NotGranted`, including a server `.mcp.json` also omits. A granted
  * server that the file does not define is `UnknownServer`.
  */
object Guard:
  def apply(ref: ToolRef, access: Access): Either[CallError, Unit] =
    ref.server match
      case None         => Left(CallError.NotCallable)
      case Some(server) =>
        if !access.allows(server, ref.tool) then Left(CallError.NotGranted(server, ref.tool))
        else if !access.defines(server) then Left(CallError.UnknownServer(server))
        else Right(())
