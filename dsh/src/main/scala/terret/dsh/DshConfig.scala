package terret.dsh

final case class DshConfig private (command: String, profile: String):
  def withProfile(next: String): Either[DshConfigError, DshConfig] =
    if next.isEmpty then Left(DshConfigError.EmptyProfile)
    else Right(copy(profile = next))

object DshConfig:
  val headless: DshConfig = DshConfig("dsh", "headless")

  def make(command: String, profile: String): Either[DshConfigError, DshConfig] =
    if command.isEmpty then Left(DshConfigError.EmptyCommand)
    else if profile.isEmpty then Left(DshConfigError.EmptyProfile)
    else Right(DshConfig(command, profile))
end DshConfig

enum DshConfigError:
  case EmptyCommand
  case EmptyProfile
end DshConfigError
