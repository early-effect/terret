package terret.grok

import zio.IO

import java.nio.file.Path

/** Why `current` did not yield a JDK directory. */
enum JdkRefuse:
  case Absent
  case Outside
  case Dangling
  case Loop
  case BadPath
  case Unreadable
  case NotDirectory

  def explain: String = text("jdk", "candidates/java")

  def explainSbt: String = text("sbt", "sdks")

  private def text(name: String, place: String): String =
    this match
      case Absent       => s"sdkman $name absent"
      case Outside      => s"sdkman $name outside $place"
      case Dangling     => s"sdkman $name link is dangling"
      case Loop         => s"sdkman $name link loops"
      case BadPath      => s"sdkman $name path is rejected"
      case Unreadable   => s"sdkman $name path is unreadable"
      case NotDirectory => s"sdkman $name is not a directory"

/** The resolved directory and the `current` symlink node. The link is not a subpath. */
final case class Granted(root: AbsPath, link: AbsPath)

/** The directory `current` resolves to, when that directory sits inside `.sdkman/candidates/java`.
  *
  * `JdkGrant.decide(home)` grants `home/.sdkman/candidates/java/21` when `current` points at it, and the
  * literal path `candidates/java/current`. A link to `/`, the home directory, or anywhere else is refused.
  */
object JdkGrant:
  def decide(home: Path): IO[JdkRefuse, Granted] =
    SdkmanLink.decide(home, "java", LinkBound.UnderBoundary)
end JdkGrant
