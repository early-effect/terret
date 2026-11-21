package terret.app

import zio.Chunk

enum Command:
  case Serve
  case Restart

enum CommandError:
  case Unknown(arguments: String)

  def render: String = this match
    case Unknown(arguments) =>
      val shown = if arguments.isEmpty then "(empty)" else arguments
      s"unknown command $shown; expected no arguments, or restart"
end CommandError

object Command:
  def parse(args: Chunk[String]): Either[CommandError, Command] =
    args match
      case Chunk()          => Right(Command.Serve)
      case Chunk("restart") => Right(Command.Restart)
      case other            => Left(CommandError.Unknown(other.mkString(" ")))
