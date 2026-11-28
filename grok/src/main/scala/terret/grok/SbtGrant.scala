package terret.grok

import zio.IO

import java.nio.file.Path

/** The directory `candidates/sbt/current` resolves to, when that directory sits inside `candidates/sbt` or `sdks`.
  *
  * `SbtGrant.decide(home)` grants `home/sdks/sbt-2` when `current` points at it, and the literal path
  * `candidates/sbt/current`. `home/sdks` has to be a real directory. The `sdks` directory itself is not granted,
  * and a link anywhere else in home is refused.
  */
object SbtGrant:
  def decide(home: Path): IO[JdkRefuse, Granted] =
    SdkmanLink.decide(home, "sbt", LinkBound.UnderSdks)
end SbtGrant
