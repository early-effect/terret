package terret.github

import zio.Chunk

/** The argument vector and the optional stdin. The vector is never a shell string. */
final case class Invocation(argv: Chunk[String], stdin: Option[String])

/** Builds the `gh` vector for one op. A body is stdin, not an argument. */
object GhArgs:
  private val MaxLimit = 30
  private val MaxTitle = 256
  private val MaxBody  = 65536

  private val PrListFields =
    "number,title,state,url,headRefName,baseRefName"

  private val PrViewFields =
    "number,title,state,body,url,headRefName,baseRefName,mergeable"

  private val PrCheckFields =
    "name,state,bucket,link,workflow"

  private val IssueListFields =
    "number,title,state,url"

  private val IssueViewFields =
    "number,title,state,body,url"

  private val RunFields =
    "databaseId,name,status,conclusion,url,headBranch,event,displayTitle,headSha"

  def prepare(op: GhOp): Either[GhProblem, Invocation] =
    op match
      case GhOp.PrList(limit, state) =>
        bounded(limit).map { kept =>
          Invocation(
            Chunk("gh", "pr", "list", "--json", PrListFields, "--limit", kept.toString, "--state", state.wire),
            None,
          )
        }
      case GhOp.PrView(number) =>
        positive(number).map { kept =>
          Invocation(Chunk("gh", "pr", "view", kept.toString, "--json", PrViewFields), None)
        }
      case GhOp.PrChecks(number) =>
        positive(number).map { kept =>
          Invocation(Chunk("gh", "pr", "checks", kept.toString, "--json", PrCheckFields), None)
        }
      case GhOp.IssueList(limit, state) =>
        bounded(limit).map { kept =>
          Invocation(
            Chunk("gh", "issue", "list", "--json", IssueListFields, "--limit", kept.toString, "--state", state.wire),
            None,
          )
        }
      case GhOp.IssueView(number) =>
        positive(number).map { kept =>
          Invocation(Chunk("gh", "issue", "view", kept.toString, "--json", IssueViewFields), None)
        }
      case GhOp.RunView(id) =>
        runId(id).map { kept =>
          Invocation(Chunk("gh", "run", "view", kept.toString, "--json", RunFields), None)
        }
      case GhOp.PrCreate(title, body, base, head) =>
        for
          keptTitle <- titleOf(title)
          keptBody  <- bodyOf(body, allowEmpty = true)
          keptBase  <- optionalBranch(base)
          keptHead  <- requiredBranch(head)
        yield Invocation(
          Chunk("gh", "pr", "create", s"--title=$keptTitle", "--body-file", "-") ++
            flag("base", keptBase) ++
            flag("head", keptHead),
          Some(keptBody),
        )
      case GhOp.PrComment(number, body) =>
        for
          kept     <- positive(number)
          keptBody <- bodyOf(body, allowEmpty = false)
        yield Invocation(
          Chunk("gh", "pr", "comment", kept.toString, "--body-file", "-"),
          Some(keptBody),
        )
      case GhOp.PrMerge(number, method) =>
        positive(number).map { kept =>
          Invocation(Chunk("gh", "pr", "merge", kept.toString, method.flag), None)
        }
      case GhOp.IssueCreate(title, body) =>
        for
          keptTitle <- titleOf(title)
          keptBody  <- bodyOf(body, allowEmpty = true)
        yield Invocation(
          Chunk("gh", "issue", "create", s"--title=$keptTitle", "--body-file", "-"),
          Some(keptBody),
        )
      case GhOp.IssueComment(number, body) =>
        for
          kept     <- positive(number)
          keptBody <- bodyOf(body, allowEmpty = false)
        yield Invocation(
          Chunk("gh", "issue", "comment", kept.toString, "--body-file", "-"),
          Some(keptBody),
        )

  private def bounded(limit: Int): Either[GhProblem, Int] =
    if limit >= 1 && limit <= MaxLimit then Right(limit) else Left(GhProblem.Limit(limit))

  private def positive(number: Int): Either[GhProblem, Int] =
    if number >= 1 then Right(number) else Left(GhProblem.Number(number))

  private def runId(id: Long): Either[GhProblem, Long] =
    if id >= 1 then Right(id) else Left(GhProblem.Run(id))

  private def titleOf(raw: String): Either[GhProblem, String] =
    if raw.nonEmpty && raw.length <= MaxTitle && !raw.exists(broken) then Right(raw)
    else Left(GhProblem.Title)

  private def bodyOf(raw: String, allowEmpty: Boolean): Either[GhProblem, String] =
    if raw.length <= MaxBody && !raw.exists(_ == '\u0000') && (allowEmpty || raw.nonEmpty) then Right(raw)
    else Left(GhProblem.Body)

  private def requiredBranch(raw: Option[String]): Either[GhProblem, Option[String]] =
    raw match
      case None        => Left(GhProblem.Detached)
      case Some(value) => branchOf(value).map(Some(_))

  private def optionalBranch(raw: Option[String]): Either[GhProblem, Option[String]] =
    raw match
      case None        => Right(None)
      case Some(value) => branchOf(value).map(Some(_))

  private def branchOf(raw: String): Either[GhProblem, String] =
    if validBranch(raw) then Right(raw) else Left(GhProblem.Branch(raw))

  private def validBranch(raw: String): Boolean =
    raw.nonEmpty &&
      raw.length <= 128 &&
      !raw.contains(':') &&
      !raw.contains("..") &&
      !raw.startsWith("-") &&
      !raw.startsWith("/") &&
      !raw.endsWith("/") &&
      !raw.endsWith(".") &&
      raw.forall(allowed)

  private def allowed(char: Char): Boolean =
    char.isLetterOrDigit || char == '.' || char == '_' || char == '/' || char == '-'

  private def broken(char: Char): Boolean =
    char == '\n' || char == '\r' || char == '\u0000'

  private def flag(name: String, value: Option[String]): Chunk[String] =
    value match
      case Some(text) => Chunk(s"--$name=$text")
      case None       => Chunk.empty
end GhArgs
