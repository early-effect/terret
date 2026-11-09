package terret.grok

enum GrokTransport:
  case Cli
  case AcpStdio
  case AcpWebSocket
end GrokTransport

enum GrokApproval:
  case Ask
  case Always
end GrokApproval

final case class AcpBind private (host: String, port: Int, secret: String)

object AcpBind:
  def make(host: String, port: Int, secret: String): Either[GrokConfigError, AcpBind] =
    if host.isEmpty then Left(GrokConfigError.EmptyHost)
    else if secret.isEmpty then Left(GrokConfigError.EmptySecret)
    else if port < 1 || port > 65535 then Left(GrokConfigError.PortOutOfRange(port))
    else Right(AcpBind(host, port, secret))
end AcpBind

final case class GrokConfig private (
    command: String,
    transport: GrokTransport,
    approval: GrokApproval,
    bind: Option[AcpBind],
):
  def withApproval(next: GrokApproval): GrokConfig = copy(approval = next)

object GrokConfig:
  val cli: GrokConfig   = GrokConfig("grok", GrokTransport.Cli, GrokApproval.Ask, None)
  val stdio: GrokConfig = GrokConfig("grok", GrokTransport.AcpStdio, GrokApproval.Ask, None)

  def socket(bind: AcpBind): GrokConfig =
    GrokConfig("grok", GrokTransport.AcpWebSocket, GrokApproval.Ask, Some(bind))

  val unbound: GrokConfig = GrokConfig("grok", GrokTransport.AcpWebSocket, GrokApproval.Ask, None)

  def make(
      command: String,
      transport: GrokTransport,
      approval: GrokApproval,
      bind: Option[AcpBind],
  ): Either[GrokConfigError, GrokConfig] =
    if command.isEmpty then Left(GrokConfigError.EmptyCommand)
    else Right(GrokConfig(command, transport, approval, bind))
end GrokConfig

enum GrokConfigError:
  case EmptyCommand
  case EmptyHost
  case EmptySecret
  case PortOutOfRange(port: Int)
end GrokConfigError
