package terret.grok

import terret.permission.{Ceiling, Permission}
import terret.workspace.Workspace
import zio.{Chunk, Fiber, IO, Promise, Ref, Scope, UIO, ZIO, ZLayer}

import java.io.{ByteArrayOutputStream, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, Path}
import scala.jdk.CollectionConverters.*

/** Why a confined command did not produce an exit code. */
enum SandboxError:
  case Unavailable
  case BadPath(path: String)
  case Rejected
  case TooLarge

  def explain(name: String): String =
    this match
      case Unavailable   => s"$name cannot be sandboxed on this host"
      case BadPath(path) => s"$name cannot be sandboxed at $path"
      case Rejected      => s"$name cannot be sandboxed"
      case TooLarge      => s"$name cannot be sandboxed"

final case class ShellBounds(
    cwd: Path,
    reads: Chunk[Path],
    writes: Chunk[Path],
    network: Boolean,
    writable: Boolean,
)

object ShellBounds:
  def of(
      ceiling: Ceiling,
      workspace: Workspace,
      readRoots: Chunk[Workspace],
      writeRoots: Chunk[Workspace],
      cwd: Path,
  ): ShellBounds =
    val reads = (workspace +: readRoots).map(_.path)
    val writes =
      if ceiling.allow.contains(Permission.WriteWorkspace) then (workspace +: writeRoots).map(_.path)
      else Chunk.empty
    ShellBounds(
      cwd,
      reads,
      writes,
      ceiling.allow.contains(Permission.Network),
      ceiling.allow.contains(Permission.WriteWorkspace),
    )

final case class SandboxOutput(code: Int, stdout: String, stderr: String)

trait SandboxHandle:
  def snapshot: UIO[(String, Boolean, Option[Int])]
  def awaitExit: IO[SandboxError, Int]
  def kill: UIO[Unit]

/** Runs a command inside the session roots.
  *
  * `profile` renders the policy and does not start a process. `run` waits. A non-zero exit is data. When this host
  * cannot apply a profile, every call fails with `Unavailable` and no process is started.
  *
  * `sandbox.run(Chunk("/bin/sh", "-c", "pwd"), bounds)`
  */
trait ShellSandbox:
  def profile(bounds: ShellBounds): IO[SandboxError, String]
  def run(argv: Chunk[String], bounds: ShellBounds): IO[SandboxError, SandboxOutput]
  def start(argv: Chunk[String], bounds: ShellBounds, limit: Int): IO[SandboxError, SandboxHandle]

object ShellSandbox:
  val OutputCap: Int = 1000000

  val ready: ShellSandbox   = Ready
  val missing: ShellSandbox = Missing

  /** One sandbox and its temp directory. The directory is removed when the scope closes. */
  def make(home: Path): ZIO[Scope, SandboxError, ShellSandbox] =
    ZIO.acquireRelease(openTemp)(path => SessionTemp.remove(Path.of(path.text))).flatMap { temp =>
      Ref.make(Noted.empty).map(noted => Live(home, temp, noted))
    }

  val layer: ZLayer[Any, SandboxError, ShellSandbox] =
    ZLayer.scoped(make(FileRoots.home))

  private def openTemp: IO[SandboxError, AbsPath] =
    SessionTemp.openDefault.flatMap { made =>
      ZIO
        .fromEither(AbsPath.from(made.toString))
        .mapError {
          case AbsPathError.NotAbsolute => SandboxError.BadPath(made.toString)
          case AbsPathError.Illegal     => SandboxError.BadPath(made.toString)
        }
        .tapError(_ => SessionTemp.remove(made))
    }

  def available: Boolean =
    val mac = Option(System.getProperty("os.name")).exists(_.toLowerCase.contains("mac"))
    mac && executable

  def skipReason(present: Boolean): Option[String] =
    if present then None else Some("sandbox-exec is missing")

  /** `sandbox-exec -p <profile> -- <argv>`. The profile is one element. Nothing is interpolated. */
  def confined(profile: String, argv: Chunk[String]): Chunk[String] =
    Chunk("/usr/bin/sandbox-exec", "-p", profile, "--") ++ argv

  private def executable: Boolean =
    try Files.isExecutable(Path.of("/usr/bin/sandbox-exec"))
    catch case _: IOException | _: SecurityException | _: InvalidPathException => false

  private object Ready extends ShellSandbox:
    private val text = "(version 1)\n(deny default)\n"

    def profile(bounds: ShellBounds): IO[SandboxError, String] =
      ZIO.succeed(text)

    def run(argv: Chunk[String], bounds: ShellBounds): IO[SandboxError, SandboxOutput] =
      ZIO.dieMessage("ShellSandbox.ready does not run a command")

    def start(argv: Chunk[String], bounds: ShellBounds, limit: Int): IO[SandboxError, SandboxHandle] =
      ZIO.dieMessage("ShellSandbox.ready does not run a command")

  private object Missing extends ShellSandbox:
    def profile(bounds: ShellBounds): IO[SandboxError, String] =
      ZIO.fail(SandboxError.Unavailable)

    def run(argv: Chunk[String], bounds: ShellBounds): IO[SandboxError, SandboxOutput] =
      ZIO.fail(SandboxError.Unavailable)

    def start(argv: Chunk[String], bounds: ShellBounds, limit: Int): IO[SandboxError, SandboxHandle] =
      ZIO.fail(SandboxError.Unavailable)

  private val SystemReads: Chunk[String] = Chunk(
    "/usr",
    "/bin",
    "/sbin",
    "/System",
    "/Library",
    "/dev",
    "/opt/homebrew",
    "/usr/local",
    "/private/var/select",
    "/var/select",
  )

  /** Reads, writes, ancestors, and link literals for one command. Caches are writable only when `writable` is set.
    * The temp directory is writable either way. `existingSystem` reports which system paths are present.
    */
  private[grok] def policyOf(
      reads: Chunk[AbsPath],
      writes: Chunk[AbsPath],
      cwd: AbsPath,
      network: Boolean,
      writable: Boolean,
      jdk: Option[Granted],
      sbt: Option[Granted],
      caches: Chunk[CacheGrant],
      temp: Option[AbsPath],
      existingSystem: String => Option[AbsPath],
  ): SeatbeltPolicy =
    val toolReads   = rootOf(jdk) ++ rootOf(sbt)
    val toolLinks   = linkOf(jdk) ++ linkOf(sbt)
    val cacheReads  = caches.map(_.root)
    val cacheWrites = if writable then cacheReads else Chunk.empty
    val tempPaths   = temp.fold(Chunk.empty[AbsPath])(path => Chunk(path))
    val system      = SystemReads.flatMap(existingSystem)
    val allReads    = dedup(reads ++ system ++ toolReads ++ cacheReads ++ tempPaths)
    val allWrites   = dedup(writes ++ cacheWrites ++ tempPaths)
    val reached     = allReads.foldLeft(Reach(Chunk.empty, Chunk.empty)) { (acc, path) =>
      Option(Path.of(path.text).getParent) match
        case None         => acc
        case Some(parent) =>
          val next = parentsOf(parent)
          Reach(acc.ancestors ++ next.ancestors, acc.links ++ next.links)
    }
    SeatbeltPolicy(
      allReads,
      allWrites,
      dedup(SeatbeltProfile.ancestors(cwd) ++ reached.ancestors ++ toolLinks),
      network,
      dedup(reached.links ++ toolLinks),
      secrets(caches),
    )

  private def rootOf(grant: Option[Granted]): Chunk[AbsPath] =
    grant.fold(Chunk.empty[AbsPath])(got => Chunk(got.root))

  private def linkOf(grant: Option[Granted]): Chunk[AbsPath] =
    grant.fold(Chunk.empty[AbsPath])(got => Chunk(got.link))

  private def secrets(caches: Chunk[CacheGrant]): Chunk[AbsPath] =
    caches.flatMap { grant =>
      grant.dir match
        case CacheDir.Maven => names(grant.root, Chunk("settings.xml", "settings-security.xml"))
        case CacheDir.Ivy   => names(grant.root, Chunk(".credentials"))
        case CacheDir.Histos | CacheDir.CacheHistos | CacheDir.Coursier | CacheDir.MacCoursier | CacheDir.CacheSbt =>
          Chunk.empty
    }

  private def names(root: AbsPath, files: Chunk[String]): Chunk[AbsPath] =
    files.flatMap(file => child(root, file))

  private def child(root: AbsPath, name: String): Chunk[AbsPath] =
    AbsPath.from(root.text + "/" + name) match
      case Right(path)                    => Chunk(path)
      case Left(AbsPathError.NotAbsolute) => Chunk.empty
      case Left(AbsPathError.Illegal)     => Chunk.empty

  private def existing(raw: String): Option[AbsPath] =
    val path = Path.of(raw)
    if Files.exists(path) then AbsPath.from(path.toAbsolutePath.normalize.toString).toOption else None

  // A subpath allow does not cover its parents, so lookup stops at the first missing one.
  // readlink of a symlink parent is file-read-data. Metadata alone never reaches /var/select.
  private def parentsOf(start: Path): Reach =
    val text = start.toString
    if text == "/" then Reach(Chunk.empty, Chunk.empty)
    else
      val above =
        Option(start.getParent) match
          case None         => Reach(Chunk.empty, Chunk.empty)
          case Some(parent) => parentsOf(parent)
      val linked = Files.isSymbolicLink(start)
      (linked, AbsPath.from(text)) match
        case (true, Right(path))                 => Reach(path +: above.ancestors, path +: above.links)
        case (false, Right(path))                => Reach(path +: above.ancestors, above.links)
        case (_, Left(AbsPathError.NotAbsolute)) => above
        case (_, Left(AbsPathError.Illegal))     => above

  private def dedup(paths: Chunk[AbsPath]): Chunk[AbsPath] =
    paths.foldLeft(Chunk.empty[AbsPath])((acc, path) => if acc.contains(path) then acc else acc :+ path)

  private final case class Reach(ancestors: Chunk[AbsPath], links: Chunk[AbsPath])

  private final case class Noted(jdk: Option[String], sbt: Option[String], caches: Chunk[String])

  private object Noted:
    val empty: Noted = Noted(None, None, Chunk.empty)

  private final class Live(home: Path, temp: AbsPath, noted: Ref[Noted]) extends ShellSandbox:
    def profile(bounds: ShellBounds): IO[SandboxError, String] =
      prepare(bounds).map(_.text)

    def run(argv: Chunk[String], bounds: ShellBounds): IO[SandboxError, SandboxOutput] =
      if argv.isEmpty then ZIO.fail(SandboxError.Rejected)
      else
        ZIO.scoped:
          ZIO.acquireRelease(open(argv, bounds, combined = false))(stop).flatMap(collect)

    def start(argv: Chunk[String], bounds: ShellBounds, limit: Int): IO[SandboxError, SandboxHandle] =
      if argv.isEmpty || limit < 1 then ZIO.fail(SandboxError.Rejected)
      else
        ZIO.uninterruptibleMask { restore =>
          restore(open(argv, bounds, combined = true)).flatMap { process =>
            for
              buffer <- Ref.make(Captured.empty)
              exit   <- Promise.make[Nothing, Int]
              fiber  <- pump(process, buffer, limit, exit).forkDaemon
            yield LiveHandle(process, buffer, exit, fiber)
          }
        }

    private def prepare(bounds: ShellBounds): IO[SandboxError, Prepared] =
      if !ShellSandbox.available then ZIO.fail(SandboxError.Unavailable)
      else
        for
          cwd    <- real(bounds.cwd)
          reads  <- ZIO.foreach(bounds.reads)(real)
          writes <- ZIO.foreach(bounds.writes)(real)
          host   <- hostGrants
          policy <- assembled(reads, writes, cwd, bounds, host, temp)
        yield Prepared(SeatbeltProfile.render(policy), Path.of(cwd.text), temp)

    private def assembled(
        reads: Chunk[AbsPath],
        writes: Chunk[AbsPath],
        cwd: AbsPath,
        bounds: ShellBounds,
        host: HostGrants,
        temp: AbsPath,
    ): IO[SandboxError, SeatbeltPolicy] =
      // A path that cannot be stated refuses the command. It is not a defect in the sandbox.
      ZIO
        .attemptBlocking(
          policyOf(
            reads,
            writes,
            cwd,
            bounds.network,
            bounds.writable,
            host.jdk,
            host.sbt,
            host.caches,
            Some(temp),
            existing,
          )
        )
        .refineOrDie {
          case _: IOException | _: SecurityException | _: InvalidPathException => SandboxError.Unavailable
        }

    private def real(path: Path): IO[SandboxError, AbsPath] =
      FileRoots.live.resolve(path).flatMap {
        case Some(found) =>
          AbsPath.from(found.toAbsolutePath.normalize.toString) match
            case Right(abs)                     => ZIO.succeed(abs)
            case Left(AbsPathError.NotAbsolute) => ZIO.fail(SandboxError.BadPath(found.toString))
            case Left(AbsPathError.Illegal)     => ZIO.fail(SandboxError.BadPath(found.toString))
        case None        => ZIO.fail(SandboxError.BadPath(path.toString))
      }

    private def hostGrants: UIO[HostGrants] =
      for
        jdk    <- tool(JdkGrant.decide(home), _.explain)
        sbt    <- tool(SbtGrant.decide(home), _.explainSbt)
        looked <- CacheDir.survey(home)
        _      <- publish(jdk._2, sbt._2, looked._2)
      yield HostGrants(jdk._1, sbt._1, looked._1)

    private def tool(
        got: IO[JdkRefuse, Granted],
        explain: JdkRefuse => String,
    ): UIO[(Option[Granted], Option[String])] =
      got.foldZIO(
        {
          case JdkRefuse.Absent => ZIO.succeed((None, None))
          case why              => ZIO.succeed((None, Some(explain(why))))
        },
        grant => ZIO.succeed((Some(grant), None)),
      )

    private def publish(jdk: Option[String], sbt: Option[String], caches: Chunk[String]): UIO[Unit] =
      val next = Noted(jdk, sbt, caches)
      noted.modify { prev =>
        val lines =
          fresh(prev.jdk, next.jdk) ++ fresh(prev.sbt, next.sbt) ++ next.caches.filterNot(prev.caches.contains)
        (lines, next)
      }.flatMap(lines => ZIO.foreachDiscard(lines)(ZIO.logInfo(_)))

    private def fresh(prev: Option[String], next: Option[String]): Chunk[String] =
      next match
        case Some(line) if !prev.contains(line) => Chunk(line)
        case _                                   => Chunk.empty

    private def open(argv: Chunk[String], bounds: ShellBounds, combined: Boolean): IO[SandboxError, Process] =
      prepare(bounds).flatMap(ready => spawn(ready.text, argv, ready.cwd, ready.temp, combined))

    private def spawn(
        profile: String,
        argv: Chunk[String],
        cwd: Path,
        temp: AbsPath,
        combined: Boolean,
    ): IO[SandboxError, Process] =
      ZIO
        .attemptBlockingInterrupt {
          val builder = new ProcessBuilder(ShellSandbox.confined(profile, argv).toList.asJava)
          builder.directory(cwd.toFile)
          builder.redirectErrorStream(combined)
          val env  = builder.environment()
          val path = Option(env.get("PATH")).filter(_.nonEmpty).getOrElse("/usr/bin:/bin")
          env.clear()
          env.put("PATH", path)
          env.put("LANG", "C")
          // Git exits fatal when a config file is denied. /dev/null keeps the host copies unopened.
          env.put("GIT_CONFIG_NOSYSTEM", "1")
          env.put("GIT_CONFIG_GLOBAL", "/dev/null")
          env.put("GIT_CONFIG_SYSTEM", "/dev/null")
          env.put("TMPDIR", temp.text)
          // The JVM splits JAVA_TOOL_OPTIONS on whitespace. Quotes keep a temp path that contains a space.
          env.put("JAVA_TOOL_OPTIONS", s"""-Djava.io.tmpdir="${temp.text}"""")
          val process = builder.start()
          close(process.getOutputStream)
          process
        }
        .refineToOrDie[IOException]
        .mapError(_ => SandboxError.Unavailable)

    private def close(out: OutputStream): Unit =
      try out.close()
      catch case _: IOException => ()

    private def stop(process: Process): UIO[Unit] =
      ZIO
        .attemptBlocking {
          val children = process.descendants()
          try children.forEach(child => child.destroy())
          finally children.close()
          process.destroy()
        }
        .ignore

    private def collect(process: Process): IO[SandboxError, SandboxOutput] =
      val code: IO[SandboxError, Int] =
        ZIO.attemptBlockingInterrupt(process.waitFor()).orDie
      val pipes: IO[SandboxError, (String, String)] =
        slurp(process.getInputStream).zipWithPar(slurp(process.getErrorStream))((out, err) => (out, err))
      pipes.zipWithPar(code) { (texts, status) =>
        val (out, err) = texts
        SandboxOutput(status, out, err)
      }

    private def slurp(in: InputStream): IO[SandboxError, String] =
      ZIO
        .attemptBlockingInterrupt {
          val out   = new ByteArrayOutputStream
          val buf   = new Array[Byte](8192)
          var extra = false
          var n     = in.read(buf)
          while n >= 0 do
            if !extra && out.size() + n <= ShellSandbox.OutputCap then out.write(buf, 0, n)
            else extra = true
            n = in.read(buf)
          if extra then Left(SandboxError.TooLarge)
          else Right(new String(out.toByteArray, StandardCharsets.UTF_8))
        }
        .refineToOrDie[IOException]
        .mapError(_ => SandboxError.Unavailable)
        .flatMap(ZIO.fromEither)

    private def pump(
        process: Process,
        buffer: Ref[Captured],
        limit: Int,
        exit: Promise[Nothing, Int],
    ): UIO[Unit] =
      drain(process.getInputStream, buffer, limit).catchAll(_ => ZIO.unit) *>
        ZIO.attemptBlockingInterrupt(process.waitFor()).orDie.flatMap(code => exit.succeed(code).unit)

    private def drain(in: InputStream, buffer: Ref[Captured], limit: Int): IO[IOException, Unit] =
      ZIO
        .attemptBlockingInterrupt {
          val buf = new Array[Byte](8192)
          val n   = in.read(buf)
          if n < 0 then None else Some(java.util.Arrays.copyOf(buf, n))
        }
        .refineToOrDie[IOException]
        .flatMap {
          case None       => ZIO.unit
          case Some(next) => buffer.update(_.add(next, limit)) *> drain(in, buffer, limit)
        }

    private final case class Prepared(text: String, cwd: Path, temp: AbsPath)

    private final case class HostGrants(jdk: Option[Granted], sbt: Option[Granted], caches: Chunk[CacheGrant])

    private final class LiveHandle(
        process: Process,
        buffer: Ref[Captured],
        exit: Promise[Nothing, Int],
        fiber: Fiber[Nothing, Unit],
    ) extends SandboxHandle:
      def snapshot: UIO[(String, Boolean, Option[Int])] =
        buffer.get.zip(done).map { (got, code) => (got.text, got.truncated, code) }

      def awaitExit: IO[SandboxError, Int] =
        exit.await

      def kill: UIO[Unit] =
        (
          stop(process) *> fiber.interrupt.ignore *> done.flatMap {
            case Some(_) => ZIO.unit
            case None    =>
              ZIO.attemptBlockingInterrupt(process.waitFor()).orDie.flatMap(code => exit.succeed(code).unit)
          }
        ).uninterruptible

      private def done: UIO[Option[Int]] =
        exit.poll.flatMap {
          case Some(waiting) => waiting.map(Some(_))
          case None          => ZIO.succeed(None)
        }

    private final case class Captured(bytes: Array[Byte], truncated: Boolean):
      def text: String = new String(bytes, StandardCharsets.UTF_8)

      def add(next: Array[Byte], limit: Int): Captured =
        if truncated || next.isEmpty then this
        else if bytes.length >= limit then copy(truncated = true)
        else if bytes.length + next.length <= limit then Captured(bytes ++ next, truncated = false)
        else Captured(bytes ++ next.take(limit - bytes.length), truncated = true)

    private object Captured:
      val empty: Captured = Captured(Array.emptyByteArray, truncated = false)
end ShellSandbox
