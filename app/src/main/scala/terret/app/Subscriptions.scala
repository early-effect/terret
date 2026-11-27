package terret.app

import java.nio.file.Path

/** Webhook subscription snapshot. It sits beside the pid file, under terret's state directory. */
object Subscriptions:
  def file: Path =
    beside(PidFile.defaultPath)

  def beside(pidFile: Path): Path =
    Option(pidFile.getParent).fold(Path.of(".terret", "subscriptions.json"))(
      _.resolve("subscriptions.json")
    )
end Subscriptions
