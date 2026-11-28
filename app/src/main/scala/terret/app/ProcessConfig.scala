package terret.app

import heddle.mcp.events.StoreError
import terret.dsh.{DshConfig, DshConfigError}
import terret.grok.{
  AcpBind,
  GrokApproval,
  GrokConfig,
  GrokConfigError,
  GrokModels,
  GrokModelsPathError,
  GrokTransport,
  McpGrantError,
  McpPolicy,
  NetworkGrantError,
  NetworkPolicy,
}
import terret.github.{GhAccess, GhGrantError}
import terret.mcp.{EventPolicy, EventPolicyError}
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.transcript.{JournalBound, JournalBoundError}
import terret.workspace.{Workspace, WorkspaceError}
import zio.{Chunk, Config, Duration}

import java.nio.file.Path

enum ProcessHarness:
  case Grok(config: GrokConfig)
  case Dsh(config: DshConfig)

final case class ProcessConfig(
    listen: Listen,
    policy: PermissionPolicy,
    root: Option[Workspace],
    version: String,
    harness: ProcessHarness,
    writeRoots: Chunk[Workspace] = Chunk.empty,
    readRoots: Chunk[Workspace] = Chunk.empty,
    mcp: McpPolicy = McpPolicy.empty,
    network: NetworkPolicy = NetworkPolicy.empty,
    gh: GhAccess = GhAccess.empty,
    events: EventPolicy = EventPolicy.terret,
    key: Option[KeySource] = None,
    grokConfig: Path,
    subscriptions: Path = Subscriptions.file,
    journal: JournalBound = JournalBound.default,
    access: Option[AccessConfig] = None,
)

object ProcessConfig:
  val local: ProcessConfig = ProcessConfig(
    listen = Listen.Http(WarpLocal.Host, WarpLocal.Port),
    policy = PermissionPolicy.Closed,
    root = None,
    version = "dev",
    harness = ProcessHarness.Grok(GrokConfig.stdio),
    grokConfig = GrokModels.defaultPath,
  )

  private final case class EventRaw(
      verification: String,
      floor: Duration,
      default: Duration,
      cap: Duration,
  )

  private val journalLimit: Config[Int] =
    Config.int("limit").withDefault(JournalBound.DefaultSize).nested("journal")

  private val accessTeam: Config[Option[String]] =
    Config.string("team").optional.nested("access")

  private val accessAudience: Config[Option[String]] =
    Config.string("audience").optional.nested("access")

  private val accessOwner: Config[Option[String]] =
    Config.string("owner").optional.nested("access")

  private val accessPort: Config[Option[Int]] =
    Config.int("port").optional.nested("access")

  private val keyFile: Config[Option[String]] =
    Config.string("file").optional.nested("key")

  private val keyService: Config[Option[String]] =
    Config.string("service").optional.nested("key")

  private val keyAccount: Config[Option[String]] =
    Config.string("account").optional.nested("key")

  private final case class BoundRaw(
      journal: Int,
      team: Option[String],
      audience: Option[String],
      owner: Option[String],
      port: Option[Int],
      keyFile: Option[String],
      keyService: Option[String],
      keyAccount: Option[String],
  )

  /** One value so the process config product stays inside what `Config.++` flattens. */
  private val boundRaw: Config[BoundRaw] =
    (journalLimit ++ accessTeam ++ accessAudience ++ accessOwner ++ accessPort ++ keyFile ++ keyService ++ keyAccount)
      .map { (journal, team, audience, owner, port, file, service, account) =>
        BoundRaw(journal, team, audience, owner, port, file, service, account)
      }

  private final case class OptInRaw(
      mcp: Option[String],
      network: Option[String],
      gh: Option[String],
  )

  private val optInRaw: Config[OptInRaw] =
    (Config.string("mcp").optional ++ Config
      .string("network")
      .optional ++ Config.string("gh").optional).map { (mcp, network, gh) =>
      OptInRaw(mcp, network, gh)
    }

  private final case class RootsRaw(read: Option[String], write: Option[String])

  private val rootsRaw: Config[RootsRaw] =
    (Config
      .string("readRoots")
      .optional ++ Config.string("writeRoots").optional).map { (read, write) =>
      RootsRaw(read, write)
    }

  private val eventRaw: Config[EventRaw] =
    (
      Config.string("verification").withDefault(EventPolicy.name(EventPolicy.terret.verification)) ++
        Config.duration("floor").withDefault(EventPolicy.terret.floor) ++
        Config.duration("default").withDefault(EventPolicy.terret.default) ++
        Config.duration("cap").withDefault(EventPolicy.terret.cap)
    ).nested("events").map { (verification, floor, default, cap) =>
      EventRaw(verification, floor, default, cap)
    }

  val raw: Config[Raw] =
    (
      Config.string("listen").withDefault("http") ++
        Config.string("host").withDefault(WarpLocal.Host) ++
        Config.int("port").withDefault(WarpLocal.Port) ++
        Config.string("policy").withDefault("Closed") ++
        Config.string("allow").withDefault("") ++
        Config.string("root").optional ++
        Config.string("version").withDefault(local.version) ++
        Config.string("harness").withDefault("grok") ++
        Config.string("command").optional ++
        Config.string("profile").optional ++
        Config.string("transport").optional ++
        Config.string("approval").optional ++
        Config.string("bindHost").optional ++
        Config.int("bindPort").optional ++
        Config.string("bindSecret").optional ++
        rootsRaw ++
        optInRaw ++
        eventRaw ++
        boundRaw ++
        GrokModels.config
    ).nested("terret").map {
      (
          listen,
          host,
          port,
          policy,
          allow,
          root,
          version,
          harness,
          command,
          profile,
          transport,
          approval,
          bindHost,
          bindPort,
          bindSecret,
          roots,
          optIn,
          events,
          bound,
          grokConfig,
      ) =>
        Raw(
          listen,
          host,
          port,
          policy,
          allow,
          root,
          version,
          harness,
          command,
          profile,
          transport,
          approval,
          bindHost,
          bindPort,
          bindSecret,
          roots.read,
          roots.write,
          optIn.mcp,
          optIn.network,
          optIn.gh,
          events.verification,
          events.floor,
          events.default,
          events.cap,
          bound.keyFile,
          bound.keyService,
          bound.keyAccount,
          bound.journal,
          bound.team,
          bound.audience,
          bound.owner,
          bound.port,
          grokConfig,
        )
    }

  def load(raw: Raw): Either[ProcessError, ProcessConfig] =
    for
      listen     <- listenOf(raw)
      policy     <- policyOf(raw.policy, raw.allow)
      root       <- rootOf(raw.root)
      version    <- versionOf(raw.version)
      harness    <- harnessOf(raw, policy)
      reads      <- rootsOf(raw.readRoots)
      writes     <- rootsOf(raw.writeRoots)
      grants     <- grantsOf(raw.mcp)
      network    <- hostsOf(raw.network)
      gh         <- ghOf(raw.gh)
      events     <- eventsOf(raw)
      grokConfig <- grokConfigOf(raw.grokConfig)
      journal    <- journalOf(raw.journalLimit)
      access     <- accessOf(
        raw.accessTeam,
        raw.accessAudience,
        raw.accessOwner,
        raw.accessPort,
        listen,
      )
      key <- KeySource.parse(raw.keyFile, raw.keyService, raw.keyAccount)
    yield ProcessConfig(
      listen,
      policy,
      root,
      version,
      harness,
      writes,
      reads,
      grants,
      network,
      gh,
      events,
      key,
      grokConfig,
      journal = journal,
      access = access,
    )

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

  private def rootOf(
      root: Option[String]
  ): Either[ProcessError, Option[Workspace]] =
    root match
      case None      => Right(None)
      case Some(raw) =>
        Workspace.from(raw).left.map(ProcessError.Workspace(_)).map(Some(_))

  private def policyOf(
      name: String,
      allow: String,
  ): Either[ProcessError, PermissionPolicy] =
    name match
      case "Closed"  => Right(PermissionPolicy.Closed)
      case "Inherit" => Right(PermissionPolicy.Inherit)
      case "Clamp"   =>
        permissions(allow).map(set => PermissionPolicy.Clamp(Ceiling(set)))
      case other => Left(ProcessError.Policy(other))

  private def permissions(
      allow: String
  ): Either[ProcessError, Set[Permission]] =
    allow
      .split(',')
      .map(_.trim)
      .filter(_.nonEmpty)
      .foldLeft[Either[ProcessError, Set[Permission]]](Right(Set.empty)) {
        case (Left(error), _)   => Left(error)
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

  private def harnessOf(
      raw: Raw,
      policy: PermissionPolicy,
  ): Either[ProcessError, ProcessHarness] =
    raw.harness match
      case "grok" => grokOf(raw, policy).map(ProcessHarness.Grok(_))
      case "dsh"  => dshOf(raw).map(ProcessHarness.Dsh(_))
      case other  => Left(ProcessError.Harness(other))

  private def grokOf(
      raw: Raw,
      policy: PermissionPolicy,
  ): Either[ProcessError, GrokConfig] =
    raw.profile match
      case Some(_) => Left(ProcessError.ProfileUnexpected)
      case None    =>
        for
          approval  <- approvalOf(raw.approval.getOrElse("Ask"))
          transport <- transportOf(raw.transport, approval, policy)
          bind      <- bindOf(transport, raw.bindHost, raw.bindPort, raw.bindSecret)
          config    <- GrokConfig
            .make(raw.command.getOrElse("grok"), transport, approval, bind)
            .left
            .map(ProcessError.Grok(_))
        yield config

  private def rootsOf(
      raw: Option[String]
  ): Either[ProcessError, Chunk[Workspace]] =
    raw match
      case None       => Right(Chunk.empty)
      case Some(text) =>
        text
          .split(',')
          .map(_.trim)
          .filter(_.nonEmpty)
          .foldLeft[Either[ProcessError, Chunk[Workspace]]](
            Right(Chunk.empty)
          ) {
            case (Left(error), _)   => Left(error)
            case (Right(acc), part) =>
              Workspace.from(part) match
                case Left(error)  => Left(ProcessError.Workspace(error))
                case Right(found) => Right(acc :+ found)
          }

  private def dshOf(raw: Raw): Either[ProcessError, DshConfig] =
    val grokKeys =
      raw.transport.nonEmpty || raw.approval.nonEmpty || raw.bindHost.nonEmpty || raw.bindPort.nonEmpty ||
        raw.bindSecret.nonEmpty
    if grokKeys then Left(ProcessError.GrokKeys)
    else
      DshConfig
        .make(raw.command.getOrElse("dsh"), raw.profile.getOrElse("headless"))
        .left
        .map(ProcessError.Dsh(_))

  private def transportOf(
      named: Option[String],
      approval: GrokApproval,
      policy: PermissionPolicy,
  ): Either[ProcessError, GrokTransport] =
    (named, approval, policy) match
      case (Some("Cli"), GrokApproval.Ask, PermissionPolicy.Inherit) =>
        Right(GrokTransport.Cli)
      case (None, GrokApproval.Ask, PermissionPolicy.Inherit) =>
        Right(GrokTransport.Cli)
      case (Some("Cli"), GrokApproval.Ask, _) => Left(ProcessError.CliAsk)
      case (None, GrokApproval.Ask, _)        => Right(GrokTransport.AcpStdio)
      case (None, GrokApproval.Always, _)     => Right(GrokTransport.Cli)
      case (Some(name), _, _)                 => namedTransport(name)

  private def namedTransport(
      name: String
  ): Either[ProcessError, GrokTransport] =
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
        AcpBind
          .make(host, port, secret)
          .left
          .map(ProcessError.Grok(_))
          .map(Some(_))
      case (GrokTransport.AcpWebSocket, _, _, _) =>
        Left(ProcessError.BindIncomplete)
      case (_, None, None, None) => Right(None)
      case _                     => Left(ProcessError.BindUnexpected)

  final case class Raw(
      listen: String,
      host: String,
      port: Int,
      policy: String,
      allow: String,
      root: Option[String],
      version: String,
      harness: String,
      command: Option[String],
      profile: Option[String],
      transport: Option[String],
      approval: Option[String],
      bindHost: Option[String],
      bindPort: Option[Int],
      bindSecret: Option[String],
      readRoots: Option[String] = None,
      writeRoots: Option[String] = None,
      mcp: Option[String] = None,
      network: Option[String] = None,
      gh: Option[String] = None,
      eventVerification: String = EventPolicy.name(EventPolicy.terret.verification),
      eventFloor: Duration = EventPolicy.terret.floor,
      eventDefault: Duration = EventPolicy.terret.default,
      eventCap: Duration = EventPolicy.terret.cap,
      keyFile: Option[String] = None,
      keyService: Option[String] = None,
      keyAccount: Option[String] = None,
      journalLimit: Int = JournalBound.DefaultSize,
      accessTeam: Option[String] = None,
      accessAudience: Option[String] = None,
      accessOwner: Option[String] = None,
      accessPort: Option[Int] = None,
      grokConfig: Option[String] = None,
  )

  private def grantsOf(raw: Option[String]): Either[ProcessError, McpPolicy] =
    raw match
      case None       => Right(McpPolicy.empty)
      case Some(text) => McpPolicy.parse(text).left.map(ProcessError.McpGrant(_))

  private def ghOf(raw: Option[String]): Either[ProcessError, GhAccess] =
    raw match
      case None       => Right(GhAccess.empty)
      case Some(text) => GhAccess.parse(text).left.map(ProcessError.GhGrant(_))

  private def hostsOf(raw: Option[String]): Either[ProcessError, NetworkPolicy] =
    raw match
      case None       => Right(NetworkPolicy.empty)
      case Some(text) => NetworkPolicy.parse(text).left.map(ProcessError.NetworkGrant(_))

  private def grokConfigOf(raw: Option[String]): Either[ProcessError, Path] =
    GrokModels.resolve(raw).left.map { case GrokModelsPathError.Empty =>
      ProcessError.GrokConfigPath
    }

  private def eventsOf(raw: Raw): Either[ProcessError, EventPolicy] =
    EventPolicy
      .from(raw.eventVerification, raw.eventFloor, raw.eventDefault, raw.eventCap)
      .left
      .map(ProcessError.Events(_))

  private def journalOf(limit: Int): Either[ProcessError, JournalBound] =
    JournalBound.from(limit).left.map(ProcessError.Journal(_))

  private def accessOf(
      team: Option[String],
      audience: Option[String],
      owner: Option[String],
      port: Option[Int],
      listen: Listen,
  ): Either[ProcessError, Option[AccessConfig]] =
    (present(team), present(audience), present(owner), port) match
      case (None, None, None, None)                     => Right(None)
      case (Some(host), Some(tag), Some(email), chosen) =>
        for
          parsedHost  <- TeamHost.from(host).left.map(ProcessError.Access(_))
          parsedTag   <- Audience.from(tag).left.map(ProcessError.Access(_))
          parsedOwner <- OwnerEmail.from(email).left.map(ProcessError.Access(_))
          tunnel      <- tunnelPort(chosen, listen)
        yield Some(AccessConfig(parsedHost, parsedTag, parsedOwner, tunnel))
      case _ => Left(ProcessError.Access(AccessConfigError.Incomplete))

  private def tunnelPort(chosen: Option[Int], listen: Listen): Either[ProcessError, Int] =
    val port = chosen.getOrElse(AccessConfig.TunnelPort)
    if port < 1 || port > 65535 then Left(ProcessError.Access(AccessConfigError.Port(port)))
    else
      listen match
        case Listen.Http(_, local) if local == port => Left(ProcessError.Access(AccessConfigError.SamePort(port)))
        case _                                      => Right(port)

  private def present(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty)
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
  case Harness(value: String)
  case ProfileUnexpected
  case GrokKeys
  case BindIncomplete
  case BindUnexpected
  case CliAsk
  case Workspace(error: WorkspaceError)
  case Grok(error: GrokConfigError)
  case GrokConfigPath
  case Dsh(error: DshConfigError)
  case NetworkGrant(error: NetworkGrantError)
  case Mcp(error: terret.mcp.TerretMcpError)
  case McpGrant(error: McpGrantError)
  case GhGrant(error: GhGrantError)
  case Events(error: EventPolicyError)
  case Journal(error: JournalBoundError)
  case Access(error: AccessConfigError)
  case Subscriptions(error: StoreError)
  case Server(error: heddle.error.ServerError)
  case Stdio(cause: Throwable)
  case Config(error: zio.Config.Error)
  case KeyMissing
  case Key(error: KeyError)
  case KeyBoth
  case KeyPair
  case KeyPath

  def render: String = this match
    case Listen(value)     => s"listen is http or stdio, got $value"
    case Host              => "host is empty"
    case Port(value)       => s"port is 0 to 65535, got $value"
    case Version           => "version is empty"
    case Policy(value)     => s"policy is Closed, Inherit, or Clamp, got $value"
    case Permission(value) => s"unknown permission $value"
    case Transport(value)  =>
      s"transport is Cli, AcpStdio, or AcpWebSocket, got $value"
    case Approval(value)   => s"approval is Ask or Always, got $value"
    case Harness(value)    => s"harness is grok or dsh, got $value"
    case ProfileUnexpected => "profile belongs on the dsh harness"
    case GrokKeys          => "transport, approval, and bind belong on the grok harness"
    case BindIncomplete    =>
      "a socket transport needs bindHost, bindPort, and bindSecret"
    case BindUnexpected       => "bind fields belong on a socket transport"
    case CliAsk               => "the CLI transport cannot answer a permission prompt"
    case Workspace(error)     => s"workspace ${error.toString}"
    case Grok(error)          => s"grok ${error.toString}"
    case GrokConfigPath       => "grok.config is empty"
    case Dsh(error)           => s"dsh ${error.toString}"
    case NetworkGrant(error)  => error.render
    case Mcp(error)           => s"mcp ${error.toString}"
    case McpGrant(error)      => error.render
    case GhGrant(error)       => error.render
    case Events(error)        => error.render
    case Journal(error)       => journalMessage(error)
    case Access(error)        => accessMessage(error)
    case Subscriptions(error) => subscriptionMessage(error)
    case Server(error)        => error.message
    case Stdio(cause)         => s"stdio ${cause.toString}"
    case Config(error)        => error.toString
    case KeyMissing           =>
      "HTTP needs a key: set terret.key.file to a mode 600 env file, or terret.key.service and terret.key.account"
    case Key(error) => error.render
    case KeyBoth    => "set terret.key.file or the keychain item, not both"
    case KeyPair    => "terret.key.service and terret.key.account are set together"
    case KeyPath    => "terret.key.file must be an absolute path"

  private def journalMessage(error: JournalBoundError): String =
    error match
      case JournalBoundError.NonPositive(value) =>
        s"journal.limit must be positive, got $value"

  private def accessMessage(error: AccessConfigError): String =
    error match
      case AccessConfigError.Incomplete =>
        "access needs terret.access.team, terret.access.audience, and terret.access.owner"
      case AccessConfigError.Team =>
        "access team is a hostname with no scheme or path"
      case AccessConfigError.Audience =>
        "access audience is a tag with no whitespace"
      case AccessConfigError.Owner       => "access owner is an email address"
      case AccessConfigError.Port(value) =>
        s"access.port is 1 to 65535, got $value"
      case AccessConfigError.SamePort(value) =>
        s"access.port $value is the local listener, and the tunnel needs its own port"

  private def subscriptionMessage(error: StoreError): String =
    error match
      case StoreError.Unreadable(_)   => "could not read event subscriptions"
      case StoreError.Undecodable     => "event subscriptions could not be decoded"
      case StoreError.Unwritable(_)   => "could not write event subscriptions"
      case StoreError.Unavailable     => "event subscriptions are unavailable"
      case StoreError.Capacity(limit) => s"event subscriptions exceed $limit"
      case StoreError.Restore(_)      => "event subscriptions could not be restored"
end ProcessError
