package terret.app

import zio.{Config, Duration, NonEmptyChunk, duration2DurationOps, durationInt}

import java.nio.file.Path

enum SupervisorChoice:
  case None
  case Launchd(jobs: LaunchdJobs)
  case Foreground(command: String)

final case class RestartBudget(pace: Duration, exit: Duration, port: Duration, listen: Duration)

object RestartBudget:
  val default: RestartBudget = RestartBudget(250.millis, 20.seconds, 20.seconds, 2.minutes)

  def from(pace: Duration, exit: Duration, port: Duration, listen: Duration): Either[ServiceError, RestartBudget] =
    if pace <= Duration.Zero then Left(ServiceError.Budget("pace"))
    else if exit <= Duration.Zero then Left(ServiceError.Budget("exit"))
    else if port <= Duration.Zero then Left(ServiceError.Budget("port"))
    else if listen <= Duration.Zero then Left(ServiceError.Budget("listen"))
    else Right(RestartBudget(pace, exit, port, listen))
end RestartBudget

final case class ServiceConfig(supervisor: SupervisorChoice, pidFile: Path, budget: RestartBudget)

enum ServiceError:
  case Supervisor(value: String)
  case Label(value: String)
  case LabelsMissing
  case PlistDirMissing
  case CommandMissing
  case Budget(name: String)
  case Pid
  case Config(error: zio.Config.Error)

  def render: String = this match
    case Supervisor(value) => s"supervisor is launchd, foreground, or none, got $value"
    case Label(value)      => s"job label is empty or has whitespace, got $value"
    case LabelsMissing     => "launchd needs at least one label"
    case PlistDirMissing   => "launchd needs a plist directory"
    case CommandMissing    => "foreground needs a command"
    case Budget(name)      => s"restart $name must be greater than zero"
    case Pid               => "could not write the pid file"
    case Config(error)     => error.toString
end ServiceError

object ServiceConfig:
  final case class Raw(
      supervisor: String,
      pidFile: String,
      labels: Option[String],
      plistDir: Option[String],
      command: Option[String],
      pace: Duration,
      exit: Duration,
      port: Duration,
      listen: Duration,
  )

  val defaultRaw: Raw = Raw(
    supervisor = "none",
    pidFile = PidFile.defaultPath.toString,
    labels = None,
    plistDir = None,
    command = None,
    pace = RestartBudget.default.pace,
    exit = RestartBudget.default.exit,
    port = RestartBudget.default.port,
    listen = RestartBudget.default.listen,
  )

  val raw: Config[Raw] =
    (
      Config.string("supervisor").withDefault(defaultRaw.supervisor) ++
        Config.string("pidFile").withDefault(defaultRaw.pidFile) ++
        Config.string("labels").nested("launchd").optional ++
        Config.string("plistDir").nested("launchd").optional ++
        Config.string("command").nested("foreground").optional ++
        Config.duration("pace").nested("restart").withDefault(defaultRaw.pace) ++
        Config.duration("exit").nested("restart").withDefault(defaultRaw.exit) ++
        Config.duration("port").nested("restart").withDefault(defaultRaw.port) ++
        Config.duration("listen").nested("restart").withDefault(defaultRaw.listen)
    ).nested("terret").map { (supervisor, pidFile, labels, plistDir, command, pace, exit, port, listen) =>
      Raw(supervisor, pidFile, labels, plistDir, command, pace, exit, port, listen)
    }

  def load(raw: Raw): Either[ServiceError, ServiceConfig] =
    for
      choice <- choiceOf(raw)
      budget <- RestartBudget.from(raw.pace, raw.exit, raw.port, raw.listen)
      path   <- pathOf(raw.pidFile)
    yield ServiceConfig(choice, path, budget)

  private def pathOf(raw: String): Either[ServiceError, Path] =
    if raw.isEmpty then Left(ServiceError.Pid) else Right(Path.of(raw))

  private def choiceOf(raw: Raw): Either[ServiceError, SupervisorChoice] =
    raw.supervisor match
      case "none"       => Right(SupervisorChoice.None)
      case "launchd"    => launchd(raw)
      case "foreground" =>
        raw.command match
          case Some(command) if command.nonEmpty => Right(SupervisorChoice.Foreground(command))
          case _                                 => Left(ServiceError.CommandMissing)
      case other => Left(ServiceError.Supervisor(other))

  private def launchd(raw: Raw): Either[ServiceError, SupervisorChoice.Launchd] =
    for
      labels <- labelsOf(raw.labels)
      dir    <- raw.plistDir match
        case Some(value) if value.nonEmpty => Right(Path.of(value))
        case _                             => Left(ServiceError.PlistDirMissing)
    yield SupervisorChoice.Launchd(LaunchdJobs(labels, dir))

  private def labelsOf(raw: Option[String]): Either[ServiceError, NonEmptyChunk[JobLabel]] =
    raw match
      case None       => Left(ServiceError.LabelsMissing)
      case Some(text) =>
        val parsed = text.split(',').iterator.map(_.trim).filter(_.nonEmpty).map(JobLabel.from).toList
        parsed.collectFirst { case Left(error) => error } match
          case Some(error) => Left(error)
          case None        =>
            parsed.collect { case Right(label) => label } match
              case head :: tail => Right(NonEmptyChunk.fromIterable(head, tail))
              case Nil          => Left(ServiceError.LabelsMissing)
end ServiceConfig
