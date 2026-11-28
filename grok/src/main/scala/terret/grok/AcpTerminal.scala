package terret.grok

import terret.event.Event
import terret.harness.HarnessFailure
import zio.json.ast.Json
import zio.{Chunk, IO}

import java.nio.file.Path

/** Terminal methods for the session that advertised them. A refusal is an RPC error and does not start a process. */
object AcpTerminal:
  def isTerminal(method: String): Boolean =
    canon(method).isDefined

  def canon(method: String): Option[String] =
    val bare = if method.startsWith("_") then method.drop(1) else method
    val name = if bare.startsWith("x.ai/") then bare.drop("x.ai/".length) else bare
    name match
      case "terminal/create" | "terminal/output" | "terminal/wait_for_exit" | "terminal/kill" | "terminal/release" =>
        Some(name)
      case _ => None

  /** Grok sends `/bin/bash -lc <script>`, sometimes as one string. Login bash reads profiles outside the jail.
    * The script stays one argument of `/bin/sh -c`. A wrapper that is not exactly one script is refused, not split,
    * and the script is never interpolated into another string. A login flag that is not that one script is refused,
    * so the login shell is not started.
    */
  def execArgv(command: String, args: Chunk[String]): Option[Chunk[String]] =
    val chosen =
      peel(command, args) match
        case Some(script)                          => Some(Chunk("/bin/sh", "-c", script))
        case None if refusesWrapper(command, args) => None
        case None                                  => Some(command +: args)
    chosen.filterNot(argv => argv.exists(_.exists(_ == '\u0000')))

  /** The text the visible-write scan sees. Peeled to the script when that is what will run. */
  def commandText(command: String, args: Chunk[String]): String =
    peel(command, args) match
      case Some(script) => script
      case None         => show(command +: args)

  def answer(
      pipe: RpcPipe,
      requestId: Json,
      method: String,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    canon(method) match
      case Some("terminal/create")        => create(pipe, requestId, params, scope)
      case Some("terminal/output")        => output(pipe, requestId, params, scope)
      case Some("terminal/wait_for_exit") => waitExit(pipe, requestId, params, scope)
      case Some("terminal/kill")          => kill(pipe, requestId, params, scope)
      case Some("terminal/release")       => release(pipe, requestId, params, scope)
      case _                              =>
        pipe.write(Wire.fail(requestId, Wire.methodNotFound, "Method not found")).as(Chunk.empty)

  private def create(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    parseCreate(params) match
      case Left(message) => reject(pipe, requestId, message)
      case Right(request) =>
        val text = commandText(request.command, request.args)
        admit(scope, text, request.cwd).flatMap {
          case Some(reason) => reject(pipe, requestId, reason)
          case None         =>
            val bounds = ShellBounds.of(
              scope.ceiling,
              scope.workspace,
              scope.readRoots,
              scope.writeRoots,
              cwdOf(scope, request.cwd),
            )
            execArgv(request.command, request.args) match
              case None => reject(pipe, requestId, "run_terminal_command is not an argument vector")
              case Some(argv) =>
                scope.terminals
                  .create(argv, bounds, request.limit)
                  .foldZIO(
                    err => reject(pipe, requestId, err.explain("run_terminal_command")),
                    id => ok(pipe, requestId, Json.Obj("terminalId" -> Json.Str(id))),
                  )
        }

  private def output(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    terminalId(params) match
      case None     => reject(pipe, requestId, "invalid terminal params")
      case Some(id) =>
        scope.terminals.output(id).foldZIO(
          err => reject(pipe, requestId, missing(err)),
          view => ok(pipe, requestId, outputBody(view)),
        )

  private def waitExit(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    terminalId(params) match
      case None     => reject(pipe, requestId, "invalid terminal params")
      case Some(id) =>
        scope.terminals.wait(id).foldZIO(
          err => reject(pipe, requestId, missing(err)),
          code => ok(pipe, requestId, Json.Obj("exitCode" -> Json.Num(BigDecimal(code)))),
        )

  private def kill(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    stop(pipe, requestId, params, scope, _.kill)

  private def release(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
  ): IO[HarnessFailure, Chunk[Event]] =
    stop(pipe, requestId, params, scope, _.release)

  private def stop(
      pipe: RpcPipe,
      requestId: Json,
      params: Json,
      scope: AcpTurn.Scope,
      act: ShellTerminals => String => IO[SandboxError, Unit],
  ): IO[HarnessFailure, Chunk[Event]] =
    terminalId(params) match
      case None     => reject(pipe, requestId, "invalid terminal params")
      case Some(id) =>
        act(scope.terminals)(id).foldZIO(
          err => reject(pipe, requestId, missing(err)),
          _ => ok(pipe, requestId, Json.Obj()),
        )

  private def missing(error: SandboxError): String =
    error match
      case SandboxError.Rejected => "unknown terminal"
      case other                 => other.explain("run_terminal_command")

  private def admit(scope: AcpTurn.Scope, text: String, cwd: Option[String]): zio.UIO[Option[String]] =
    PermissionChoice
      .decide(
        scope.policy,
        scope.ceiling,
        scope.workspace,
        scope.readRoots,
        scope.writeRoots,
        permissionParams(text, cwd),
        scope.files,
        scope.mcp,
        scope.network,
        sandbox = scope.sandbox,
      )
      .map {
        case PermissionAnswer.Selected(_)           => None
        case PermissionAnswer.Refused(_, _, reason) => Some(reason)
        case PermissionAnswer.Cancelled             => Some("run_terminal_command was refused")
      }

  private def cwdOf(scope: AcpTurn.Scope, cwd: Option[String]): Path =
    cwd.flatMap(text => FileRoots.lexical(text, scope.workspace.path, FileRoots.home)) match
      case Some(path) => path
      case None       => scope.workspace.path

  private def permissionParams(text: String, cwd: Option[String]): Json =
    val fields = List("command" -> Json.Str(text)) ++ cwd.map(dir => "cwd" -> Json.Str(dir))
    Json.Obj(
      "toolCall" -> Json.Obj(
        "toolCallId" -> Json.Str("terminal"),
        "kind"       -> Json.Str("execute"),
        "rawInput"   -> Json.Obj(fields*),
      ),
      "options" -> Json.Arr(
        Json.Obj("optionId" -> Json.Str("allow-once"), "kind" -> Json.Str("allow_once")),
        Json.Obj("optionId" -> Json.Str("reject-once"), "kind" -> Json.Str("reject_once")),
      ),
    )

  private[grok] def parseCreate(params: Json): Either[String, Create] =
    JsonFields.obj(params) match
      case None      => Left("invalid terminal params")
      case Some(obj) =>
        JsonFields.str(obj, "command").filter(_.nonEmpty) match
          case None          => Left("invalid terminal params")
          case Some(command) =>
            argsOf(obj).flatMap { args =>
              limitOf(JsonFields.field(obj, "outputByteLimit")).map { limit =>
                Create(command, args, JsonFields.str(obj, "cwd").filter(_.nonEmpty), limit)
              }
            }

  private def argsOf(obj: Json.Obj): Either[String, Chunk[String]] =
    JsonFields.field(obj, "args") match
      case None                    => Right(Chunk.empty)
      case Some(Json.Arr(values)) =>
        val texts = values.collect { case Json.Str(text) => text }
        if texts.length == values.length then Right(texts) else Left("invalid terminal params")
      case Some(_) => Left("invalid terminal params")

  private def limitOf(raw: Option[Json]): Either[String, Int] =
    raw match
      case None => Right(ShellSandbox.OutputCap)
      case Some(Json.Num(number)) =>
        val whole = number.stripTrailingZeros
        if whole.scale > 0 || whole.compareTo(java.math.BigDecimal.ONE) < 0 then Left("invalid terminal params")
        else if whole.compareTo(java.math.BigDecimal.valueOf(ShellSandbox.OutputCap.toLong)) > 0 then
          Right(ShellSandbox.OutputCap)
        else Right(whole.intValue)
      case Some(_) => Left("invalid terminal params")

  private def terminalId(params: Json): Option[String] =
    JsonFields.obj(params).flatMap(obj => JsonFields.str(obj, "terminalId").filter(_.nonEmpty))

  private def outputBody(view: TerminalView): Json =
    val fields = List(
      "output"    -> Json.Str(view.output),
      "truncated" -> Json.Bool(view.truncated),
    )
    val withExit = view.exit match
      case Some(code) => fields :+ ("exitStatus" -> Json.Obj("exitCode" -> Json.Num(BigDecimal(code))))
      case None       => fields
    Json.Obj(withExit*)

  private def peel(command: String, args: Chunk[String]): Option[String] =
    splitPeel(command, args).orElse(if args.isEmpty then joinedPeel(command) else None)

  private def splitPeel(command: String, args: Chunk[String]): Option[String] =
    if !loginShell(command) || command.exists(_.isWhitespace) then None
    else
      (args.lift(0), args.lift(1), args.lift(2), args.length) match
        case (Some("-lc"), Some(script), None, 2)      => Some(script)
        case (Some("-l"), Some("-c"), Some(script), 3) => Some(script)
        case _                                         => None

  private def joinedPeel(command: String): Option[String] =
    oneToken(command.trim) match
      case Some((exe, rest)) if loginShell(exe) => flagsThenScript(skipBlank(rest))
      case _                                    => None

  private def wrappedShell(command: String): Boolean =
    oneToken(command.trim) match
      case Some((exe, rest)) => loginShell(exe) && rest.exists(ch => !isBlank(ch))
      case None              => false

  private def refusesWrapper(command: String, args: Chunk[String]): Boolean =
    wrappedShell(command) || loginWrapper(command, args)

  /** `-l` reads the login profiles. Anything except the one peeled script stays unstarted. */
  private def loginWrapper(command: String, args: Chunk[String]): Boolean =
    loginShell(command) && !command.exists(_.isWhitespace) && args.headOption.exists(isLoginFlag)

  private def isLoginFlag(flag: String): Boolean =
    flag == "--login" || (flag.startsWith("-") && !flag.startsWith("--") && flag.contains('l'))

  private def flagsThenScript(rest: String): Option[String] =
    oneToken(rest) match
      case Some(("-lc", after)) => onlyWord(after)
      case Some(("-l", after))  =>
        oneToken(skipBlank(after)) match
          case Some(("-c", script)) => onlyWord(script)
          case _                    => None
      case _ => None

  private def onlyWord(raw: String): Option[String] =
    oneToken(skipBlank(raw)) match
      case Some((word, rest)) if rest.forall(isBlank) => Some(word)
      case _                                          => None

  private def oneToken(raw: String): Option[(String, String)] =
    val text = skipBlank(raw)
    if text.isEmpty then None
    else
      val acc = new StringBuilder
      text.charAt(0) match
        case '\'' => quoted(text, 1, '\'', acc)
        case '"'  => quoted(text, 1, '"', acc)
        case _    => bare(text, 0, acc)

  private def quoted(raw: String, index: Int, mark: Char, acc: StringBuilder): Option[(String, String)] =
    if index >= raw.length then None
    else if mark == '\'' && startsWith(raw, index, "'\\''") then
      acc.append('\'')
      quoted(raw, index + 4, mark, acc)
    else
      val ch = raw.charAt(index)
      if ch == mark then Some((acc.toString, raw.substring(index + 1)))
      else if mark == '"' && ch == '\\' && index + 1 < raw.length then
        val next = raw.charAt(index + 1)
        if next == '\n' then quoted(raw, index + 2, mark, acc)
        else if "\"\\$`".contains(next) then
          acc.append(next)
          quoted(raw, index + 2, mark, acc)
        else
          acc.append('\\')
          acc.append(next)
          quoted(raw, index + 2, mark, acc)
      else
        acc.append(ch)
        quoted(raw, index + 1, mark, acc)

  private def bare(raw: String, index: Int, acc: StringBuilder): Option[(String, String)] =
    if index >= raw.length || isBlank(raw.charAt(index)) then
      val word = acc.toString
      if bareWord(word) then Some((word, raw.substring(index))) else None
    else
      val ch = raw.charAt(index)
      if ch == '\'' || ch == '"' || ch == '\\' then None
      else
        acc.append(ch)
        bare(raw, index + 1, acc)

  private def bareWord(text: String): Boolean =
    text.nonEmpty && text.forall(bareChar)

  private def bareChar(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_' || ch == '@' || ch == '%' || ch == '+' || ch == '=' || ch == ':' || ch == ',' ||
      ch == '.' || ch == '/' || ch == '-'

  private def skipBlank(raw: String): String =
    raw.dropWhile(isBlank)

  private def isBlank(ch: Char): Boolean =
    ch == ' ' || ch == '\t' || ch == '\n'

  private def startsWith(raw: String, index: Int, token: String): Boolean =
    index + token.length <= raw.length && raw.regionMatches(index, token, 0, token.length)

  private def loginShell(command: String): Boolean =
    val slash = command.lastIndexOf('/')
    val name  = if slash < 0 then command else command.drop(slash + 1)
    name == "bash" || name == "sh" || name == "zsh"

  private def show(argv: Chunk[String]): String =
    argv.map(quoteWord).mkString(" ")

  private def quoteWord(raw: String): String =
    if raw.isEmpty then "''"
    else if raw.exists(needsQuote) then "'" + raw.replace("'", "'\\''") + "'"
    else raw

  private def needsQuote(ch: Char): Boolean =
    ch.isWhitespace || ch == '\'' || ch == '"' || ch == '>' || ch == '<' || ch == '|' || ch == ';' || ch == '&' ||
      ch == '$' || ch == '`' || ch == '\\'

  private def ok(pipe: RpcPipe, id: Json, result: Json): IO[HarnessFailure, Chunk[Event]] =
    pipe.write(Wire.ok(id, result)).as(Chunk.empty)

  private def reject(pipe: RpcPipe, id: Json, message: String): IO[HarnessFailure, Chunk[Event]] =
    pipe.write(Wire.fail(id, Wire.invalidParams, message)).as(Chunk.empty)

  private[grok] final case class Create(command: String, args: Chunk[String], cwd: Option[String], limit: Int)
end AcpTerminal
