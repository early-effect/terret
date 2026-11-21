package terret.grok

import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

object LiveGate:
  enum Decision:
    case Ready
    case Skip(reason: String)

  lazy val current: Decision =
    val found =
      try decide(sys.env, (command, arg) => run(command, arg))
      catch case _: IOException => Decision.Skip("grok is not on PATH")
    found match
      case Decision.Skip(reason) => System.err.println(s"SKIP live grok: $reason")
      case Decision.Ready        => ()
    found

  private[terret] def decide(
      env: Map[String, String],
      run: (String, String) => Option[(Int, String)],
  ): Decision =
    if truthy(env.get("CI")) || truthy(env.get("GITHUB_ACTIONS")) then Decision.Skip("CI")
    else
      run("grok", "--version") match
        case Some((0, _)) =>
          run("grok", "models") match
            case Some((0, text)) if text.contains("You are logged in") => Decision.Ready
            case _                                                     => Decision.Skip("grok is not authenticated")
        case _ => Decision.Skip("grok is not on PATH")

  private def truthy(value: Option[String]): Boolean =
    value.exists(_.equalsIgnoreCase("true"))

  // Drain stdout on another thread. A full pipe would stall waitFor.
  private def run(args: String*): Option[(Int, String)] =
    val process  = new ProcessBuilder(args*).redirectErrorStream(true).start()
    val captured = ByteArrayOutputStream()
    val pump     = Thread(() => transfer(process.getInputStream, captured), "live-gate")
    pump.setDaemon(true)
    pump.start()
    val finished = process.waitFor(20, TimeUnit.SECONDS)
    if !finished then process.destroyForcibly()
    pump.join(1000)
    if finished then Some((process.exitValue(), captured.toString(StandardCharsets.UTF_8))) else None

  private def transfer(in: InputStream, out: ByteArrayOutputStream): Unit =
    val buffer = new Array[Byte](4096)
    def loop(): Unit =
      val count = in.read(buffer)
      if count >= 0 then
        out.write(buffer, 0, count)
        loop()
    try loop()
    catch case _: IOException => ()
end LiveGate
