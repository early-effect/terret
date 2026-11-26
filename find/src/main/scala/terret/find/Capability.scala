package terret.find

/** What a tool may touch. `wire` is the form the index stores. */
enum Capability:
  case ReadWorkspace
  case WriteWorkspace
  case RunCommands
  case Network

  def wire: String = this match
    case ReadWorkspace  => "read-workspace"
    case WriteWorkspace => "write-workspace"
    case RunCommands    => "run-commands"
    case Network        => "network"
end Capability

object Capability:
  def from(raw: String): Either[FindError, Capability] =
    raw match
      case "read-workspace"  => Right(Capability.ReadWorkspace)
      case "write-workspace" => Right(Capability.WriteWorkspace)
      case "run-commands"    => Right(Capability.RunCommands)
      case "network"         => Right(Capability.Network)
      case other             => Left(FindError.UnknownCapability(other))
