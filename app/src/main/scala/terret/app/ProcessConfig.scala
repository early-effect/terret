package terret.app

import terret.grok.{AcpBind, GrokApproval, GrokConfig, GrokConfigError, GrokTransport}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.{Workspace, WorkspaceError}
import zio.Config

final case class ProcessConfig(
    listen: Listen,
    policy: PermissionPolicy,
    root: Option[Workspace],
    version: String,
    grok: GrokConfig,
)

object ProcessConfig:
  val local: ProcessConfig = ProcessConfig(
    listen = Listen.Http(WarpLocal.Host, WarpLocal.Port),
    policy = PermissionPolicy.Closed,
    root = None,
    version = "dev",
    grok = GrokConfig.cli,
  )

  val raw: Config[Raw] =
    (
      Config.string("listen").withDefault("http") ++
        Config.string("host").withDefault(WarpLocal.Host) ++
        Config.int("port").withDefault(WarpLocal.Port) ++
        Config.string("policy").withDefault("Closed") ++
        Config.string("allow").withDefault("") ++
        Config.string("root").optional ++
        Config.string("version").withDefault(local.version) ++
        Config.string("command").withDefault("grok") ++
        Config.string("transport").withDefault("Cli") ++
        Config.string("approval").withDefault("Ask") ++
        Config.string("bindHost").optional ++
        Config.int("bindPort").optional ++
        Config.string("bindSecret").optional
    ).nested("terret").map {
      (
          listen,
          host,
          port,
          policy,
          allow,
          root,
          version,
          command,
          transport,
          approval,
          bindHost,
          bindPort,
          bindSecret,
      ) =>
        Raw(
          listen,
          host,
          port,
          policy,
          allow,
          root,
          version,
          command,
          transport,
          approval,
          bindHost,
          bindPort,
          bindSecret,
        )
    }

  def load(raw: Raw): Either[ProcessError, ProcessConfig] =
    for
      listen  <- listenOf(raw)
      policy  <- policyOf(raw.policy, raw.allow)
      root    <- rootOf(raw.root)
      version <- versionOf(raw.version)
      grok    <- grokOf(raw)
    yield ProcessConfig(listen, policy, root, version, grok)

  private def listenOf(raw: Raw): Either[ProcessError, Listen] =
    raw.listen match
      case "http"  => http(raw.host, raw.port)
      case "stdio" => Right(Listen.Stdio)
      case other   => Left(ProcessError.Listen(other))

  private def http(host: String, port: Int): Either[ProcessError, Listen.Http] =
    if host.isEmpty then Left(ProcessError.Host)
    else if port < 0 || port > 65535 then Left(ProcessError.Port(port))
    else Right(Listen.Http(host, port))

  private def versionOf(version: String): Either[ProcessError, String] =
    if version.isEmpty then Left(ProcessError.Version) else Right(version)

  private def rootOf(root: Option[String]): Either[ProcessError, Option[Workspace]] =
    root match
      case None      => Right(None)
      case Some(raw) => Workspace.from(raw).left.map(ProcessError.Workspace(_)).map(Some(_))

  private def policyOf(name: String, allow: String): Either[ProcessError, PermissionPolicy] =
    name match
      case "Closed"  => Right(PermissionPolicy.Closed)
      case "Inherit" => Right(PermissionPolicy.Inherit)
      case "Clamp"   => permissions(allow).map(set => PermissionPolicy.Clamp(Ceiling(set)))
      case other     => Left(ProcessError.Policy(other))

  private def permissions(allow: String): Either[ProcessError, Set[Permission]] =
    allow.split(',').map(_.trim).filter(_.nonEmpty).foldLeft[Either[ProcessError, Set[Permission]]](Right(Set.empty)) {
      case (Left(error), _)     => Left(error)
      case (Right(acc), name) =>
        permission(name) match
          case Left(error)  => Left(error)
          case Right(value) => Right(acc + value)
    }

  private def permission(name: String): Either[ProcessError, Permission] =
    name match
      case "ReadWorkspace"  => Right(Permission.ReadWorkspace)
      case "WriteWorkspace" => Right(Permission.WriteWorkspace)
      case "RunCommands"    => Right(Permission.RunCommands)
      case "Network"        => Right(Permission.Network)
      case other            => Left(ProcessError.Permission(other))

  private def grokOf(raw: Raw): Either[ProcessError, GrokConfig] =
    for
      transport <- transportOf(raw.transport)
      approval  <- approvalOf(raw.approval)
      bind      <- bindOf(transport, raw.bindHost, raw.bindPort, raw.bindSecret)
      config    <- GrokConfig.make(raw.command, transport, approval, bind).left.map(ProcessError.Grok(_))
    yield config

  private def transportOf(name: String): Either[ProcessError, GrokTransport] =
    name match
      case "Cli"          => Right(GrokTransport.Cli)
      case "AcpStdio"     => Right(GrokTransport.AcpStdio)
      case "AcpWebSocket" => Right(GrokTransport.AcpWebSocket)
      case other          => Left(ProcessError.Transport(other))

  private def approvalOf(name: String): Either[ProcessError, GrokApproval] =
    name match
      case "Ask"    => Right(GrokApproval.Ask)
      case "Always" => Right(GrokApproval.Always)
      case other    => Left(ProcessError.Approval(other))

  private def bindOf(
      transport: GrokTransport,
      host: Option[String],
      port: Option[Int],
      secret: Option[String],
  ): Either[ProcessError, Option[AcpBind]] =
    (transport, host, port, secret) match
      case (GrokTransport.AcpWebSocket, Some(host), Some(port), Some(secret)) =>
        AcpBind.make(host, port, secret).left.map(ProcessError.Grok(_)).map(Some(_))
      case (GrokTransport.AcpWebSocket, _, _, _) => Left(ProcessError.BindIncomplete)
      case (_, None, None, None)                 => Right(None)
      case _                                     => Left(ProcessError.BindUnexpected)

  final case class Raw(
      listen: String,
      host: String,
      port: Int,
      policy: String,
      allow: String,
      root: Option[String],
      version: String,
      command: String,
      transport: String,
      approval: String,
      bindHost: Option[String],
      bindPort: Option[Int],
      bindSecret: Option[String],
  )
end ProcessConfig

enum ProcessError:
  case Listen(value: String)
  case Host
  case Port(value: Int)
  case Version
  case Policy(value: String)
  case Permission(value: String)
  case Transport(value: String)
  case Approval(value: String)
  case BindIncomplete
  case BindUnexpected
  case Workspace(error: WorkspaceError)
  case Grok(error: GrokConfigError)
  case Mcp(error: terret.mcp.TerretMcpError)
  case Server(error: heddle.error.ServerError)
  case Stdio(cause: Throwable)
  case Config(error: zio.Config.Error)

  def render: String = this match
    case Listen(value)       => s"listen is http or stdio, got $value"
    case Host                => "host is empty"
    case Port(value)         => s"port is 0 to 65535, got $value"
    case Version             => "version is empty"
    case Policy(value)       => s"policy is Closed, Inherit, or Clamp, got $value"
    case Permission(value)   => s"unknown permission $value"
    case Transport(value)    => s"transport is Cli, AcpStdio, or AcpWebSocket, got $value"
    case Approval(value)     => s"approval is Ask or Always, got $value"
    case BindIncomplete      => "a socket transport needs bindHost, bindPort, and bindSecret"
    case BindUnexpected      => "bind fields belong on a socket transport"
    case Workspace(error)    => s"workspace ${error.toString}"
    case Grok(error)         => s"grok ${error.toString}"
    case Mcp(error)          => s"mcp ${error.toString}"
    case Server(error)       => error.message
    case Stdio(cause)        => s"stdio ${cause.toString}"
    case Config(error)       => error.toString
end ProcessError
