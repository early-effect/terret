package terret.grok

import slayer.*
import terret.permission.{Ceiling, Permission, PermissionPolicy}
import terret.workspace.Workspace
import zio.json.ast.Json
import zio.test.*
import zio.{Chunk, Ref, ZIO}

import java.nio.file.Path

object ConfineSpec extends ZIOSpecDefault:
  private val options = Json.Arr(
    Json.Obj("optionId" -> Json.Str("allow-once"), "kind"  -> Json.Str("allow_once")),
    Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
  )

  private val home    = Path.of("/Users/someone")
  private val reads   = Chunk("read", "search", "grep", "glob", "list")
  private val writes  = Chunk("write", "edit", "create", "delete", "move", "rename", "other")
  private val kinds   = reads ++ writes
  private val reading = Ceiling(Set(Permission.ReadWorkspace))
  private val editing = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace))
  private val shellOk = Ceiling(Set(Permission.ReadWorkspace, Permission.WriteWorkspace, Permission.RunCommands))

  def spec = suite("workspace confinement")(
    test("a path inside the workspace is allowed for every file kind") {
      for
        _    <- workOnly
        got  <- ZIO.foreach(kinds)(kind => ask(kind, "/work/A.scala", ceiling(kind)))
        want = kinds.map(_ => allow)
      yield assertTrue(got == want)
    }.provide(stubbed[FileRoots]),
    test("a sibling is allowed only for the mode that root grants") {
      for
        _     <- covering
        sib   <- root("/sib")
        read  <- ask("read", "/sib/A.scala", reading, read = Chunk(sib))
        write <- ask("edit", "/sib/A.scala", editing, write = Chunk(sib))
        noRead  <- ask("grep", "/sib/A.scala", reading, write = Chunk(sib))
        noWrite <- ask("edit", "/sib/A.scala", editing, read = Chunk(sib))
        workspaceWrite <- ask("edit", "/work/A.scala", reading)
      yield assertTrue(
        read == allow,
        write == allow,
        noRead == refused("grep reads /sib/A.scala, outside the workspace; grant it in terret.readRoots"),
        noWrite == refused("edit writes /sib/A.scala, outside the workspace; grant it in terret.writeRoots"),
        workspaceWrite == refused("edit needs WriteWorkspace, clamped by server policy"),
      )
    }.provide(stubbed[FileRoots]),
    test("home, cursor, absolute, dotdot, and symlink escapes are refused for every file kind") {
      val paths = Chunk(
        "~",
        "~/.cursor",
        "/Users/someone",
        "/etc/passwd",
        "/sib/A.scala",
        "/work/../outside",
        "../outside",
        "/work/link",
      )
      for
        _   <- workOnly
        got <- ZIO.foreach(kinds) { kind =>
          ZIO.foreach(paths) { path =>
            ask(kind, path, ceiling(kind)).map(answer => (kind, path, answer))
          }
        }
      yield assertTrue(got.flatten.forall { (kind, path, answer) =>
        answer == refused(outside(kind, path))
      })
    }.provide(stubbed[FileRoots]),
    test("a symlink created after an allow is refused on the next call") {
      for
        escaped <- Ref.make(false)
        _       <- stub[FileRoots](_.resolve(any[Path]))((path: Path) =>
          escaped.get.map { flag =>
            if flag && path.toString.endsWith("/link") then Some(Path.of("/Users/someone/.cursor"))
            else if path.toString == "/work" || path.toString.startsWith("/work/") then Some(path)
            else None
          }
        )
        before <- ask("read", "/work/link", reading)
        _      <- escaped.set(true)
        after  <- ask("read", "/work/link", reading)
      yield assertTrue(
        before == allow,
        after == refused("read reads /work/link, outside the workspace; grant it in terret.readRoots"),
      )
    }.provide(stubbed[FileRoots]),
    test("a path refusal names the missing grant and the reply lets the turn continue") {
      for
        _      <- workOnly
        answer <- ask("grep", "~/.cursor", reading)
      yield assertTrue(
        answer == refused("grep reads ~/.cursor, outside the workspace; grant it in terret.readRoots"),
        PermissionChoice.body(answer) == Json.Obj(
          "outcome" -> Json.Obj("outcome" -> Json.Str("selected"), "optionId" -> Json.Str("reject-once")),
          "_meta"   -> Json.Obj(
            "followup_message" -> Json.Str("grep reads ~/.cursor, outside the workspace; grant it in terret.readRoots")
          ),
        ),
      )
    }.provide(stubbed[FileRoots]),
    test("a shell stays off without RunCommands, and an outside cwd is refused") {
      for
        _       <- workOnly
        denied  <- shell("./histos --version", None, Ceiling.closed, PermissionPolicy.Closed)
        shifted <- shell("/bin/ls", Some("/tmp"), shellOk, PermissionPolicy.Clamp(shellOk))
        kept    <- shell("./histos --version", None, shellOk, PermissionPolicy.Clamp(shellOk))
      yield assertTrue(
        denied == refused("run_terminal_command needs RunCommands, clamped by server policy"),
        shifted == refused("run_terminal_command cwd is /tmp, outside the workspace"),
        kept == allow,
      )
    }.provide(stubbed[FileRoots]),
  )

  private def ceiling(kind: String): Ceiling =
    if writes.contains(kind) then editing else reading

  private def allow: PermissionAnswer = PermissionAnswer.Selected("allow-once")

  private def refused(reason: String): PermissionAnswer =
    PermissionAnswer.Refused("reject-once", "call_1", reason)

  private def outside(kind: String, path: String): String =
    val name = if kind == "other" then "other" else kind
    val verb = if writes.contains(kind) then "writes" else "reads"
    val key  = if writes.contains(kind) then "terret.writeRoots" else "terret.readRoots"
    s"$name $verb $path, outside the workspace; grant it in $key"

  private def workOnly =
    stub[FileRoots](_.resolve(any[Path]))((path: Path) =>
      val text = path.toString
      if text == "/work" || text.startsWith("/work/") then
        if text.endsWith("/link") then ZIO.succeed(Some(Path.of("/Users/someone/.cursor")))
        else ZIO.succeed(Some(path))
      else ZIO.die(new IllegalStateException(text))
    )

  private def covering =
    stub[FileRoots](_.resolve(any[Path]))((path: Path) =>
      val text = path.toString
      if text == "/work" || text.startsWith("/work/") || text == "/sib" || text.startsWith("/sib/") then
        ZIO.succeed(Some(path))
      else ZIO.die(new IllegalStateException(text))
    )

  private def ask(
      kind: String,
      path: String,
      ceiling: Ceiling,
      read: Chunk[Workspace] = Chunk.empty,
      write: Chunk[Workspace] = Chunk.empty,
  ): ZIO[FileRoots, Nothing, PermissionAnswer] =
    ZIO.serviceWithZIO[FileRoots] { files =>
      root("/work").flatMap { workspace =>
        PermissionChoice.decide(
          PermissionPolicy.Clamp(ceiling),
          ceiling,
          workspace,
          read,
          write,
          call(kind, path),
          files,
          home = home,
          sandbox = ShellSandbox.missing,
        )
      }
    }

  private def shell(
      command: String,
      cwd: Option[String],
      ceiling: Ceiling,
      policy: PermissionPolicy,
  ): ZIO[FileRoots, Nothing, PermissionAnswer] =
    ZIO.serviceWithZIO[FileRoots] { files =>
      root("/work").flatMap { workspace =>
        PermissionChoice.decide(
          policy,
          ceiling,
          workspace,
          Chunk.empty,
          Chunk.empty,
          commandOf(command, cwd),
          files,
          home = home,
          sandbox = ShellSandbox.ready,
        )
      }
    }

  private def root(raw: String) =
    ZIO.fromEither(Workspace.from(raw)).orDieWith(_ => new IllegalArgumentException(raw))

  private def call(kind: String, path: String): Json =
    val raw =
      if kind == "other" then Json.Obj("file_path" -> Json.Str(path))
      else Json.Obj()
    val locations = Json.Arr(Json.Obj("path" -> Json.Str(path)))
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str(kind),
        "locations"  -> locations,
        "rawInput"   -> raw,
      ),
      "options" -> options,
    )

  private def commandOf(command: String, cwd: Option[String]): Json =
    val fields = cwd match
      case Some(dir) => Chunk("command" -> Json.Str(command), "cwd" -> Json.Str(dir))
      case None      => Chunk("command" -> Json.Str(command))
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("call_1"),
        "kind"       -> Json.Str("execute"),
        "rawInput"   -> Json.Obj(fields),
      ),
      "options" -> options,
    )
end ConfineSpec
