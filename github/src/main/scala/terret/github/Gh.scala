package terret.github

import terret.redact.Redact
import terret.workspace.Workspace
import zio.{Chunk, IO, ZIO, ZLayer}

import java.nio.file.Path

/** Runs `gh` against one session workspace.
  *
  * An empty `access` denies every op. The refusal names the grant that op needs. The command is an argument vector in
  * `workspace`, and stdout is redacted before it is returned. Merge stays on the write grant and is not implied by
  * create.
  */
trait Gh:
  def perform(op: GhOp, workspace: Workspace, access: GhAccess): IO[GhError, String]

object Gh:
  /** Unwired default. Every op is refused, and nothing is spawned. */
  val closed: Gh = new Gh:
    def perform(op: GhOp, _workspace: Workspace, _access: GhAccess): IO[GhError, String] =
      ZIO.fail(GhError.NotGranted(op.grant, op.label))

  def layer: ZLayer[GhCommand, Nothing, Gh] =
    ZLayer.fromFunction(Live.apply)

  def live(project: Option[Path] = None): ZLayer[Any, Nothing, Gh] =
    GhCommand.layer(project) >>> layer

  private final class Live(command: GhCommand) extends Gh:
    def perform(op: GhOp, workspace: Workspace, access: GhAccess): IO[GhError, String] =
      if !access.allows(op.grant) then ZIO.fail(GhError.NotGranted(op.grant, op.label))
      else
        ready(op, workspace).flatMap { invocation =>
          command.execute(invocation.argv, workspace, invocation.stdin).flatMap { output =>
            val stdout = Redact.text(output.stdout)
            val stderr = Redact.text(output.stderr)
            if op.accepts(output.code) then ZIO.succeed(stdout)
            else ZIO.fail(GhError.Failed(output.code, if stderr.nonEmpty then stderr else stdout))
          }
        }

    private def ready(op: GhOp, workspace: Workspace): IO[GhError, Invocation] =
      op match
        case create: GhOp.PrCreate if create.head.isEmpty =>
          branch(workspace).flatMap { name =>
            fromEither(GhArgs.prepare(create.copy(head = Some(name))))
          }
        case other =>
          fromEither(GhArgs.prepare(other))

    private def branch(workspace: Workspace): IO[GhError, String] =
      command.execute(Chunk("git", "rev-parse", "--abbrev-ref", "HEAD"), workspace, None).flatMap { output =>
        val stderr = Redact.text(output.stderr)
        val stdout = Redact.text(output.stdout)
        if output.code != 0 then ZIO.fail(GhError.Failed(output.code, if stderr.nonEmpty then stderr else stdout))
        else
          val name = stdout.trim
          if name.isEmpty || name == "HEAD" then ZIO.fail(GhError.BadRequest(GhProblem.Detached))
          else ZIO.succeed(name)
      }

    private def fromEither(parsed: Either[GhProblem, Invocation]): IO[GhError, Invocation] =
      ZIO.fromEither(parsed).mapError(GhError.BadRequest(_))
  end Live
end Gh
