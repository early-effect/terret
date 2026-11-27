package terret.app.watch

import terret.workspace.{Workspace, WorkspaceError}
import zio.Chunk

/** Why a watch open refused a workspace path. The check is lexical. It does not resolve the path. */
enum WatchPathError:
  case Empty
  case Relative
  case Invalid
  case Dots
  case Outside

/** A workspace opened from `/watch` must sit under an allow-listed root. A `..` segment is refused before normalize, so
  * a path is not collapsed into a root it started outside.
  */
object WatchRoots:
  val Limit: Int = 4096

  def admit(raw: String, roots: Chunk[Workspace]): Either[WatchPathError, Workspace] =
    if raw.isEmpty then Left(WatchPathError.Empty)
    else if raw.length > Limit then Left(WatchPathError.Invalid)
    else if dotDot(raw) then Left(WatchPathError.Dots)
    else
      Workspace.from(raw) match
        case Left(WorkspaceError.Empty)       => Left(WatchPathError.Empty)
        case Left(WorkspaceError.Relative(_))  => Left(WatchPathError.Relative)
        case Left(WorkspaceError.Invalid(_))   => Left(WatchPathError.Invalid)
        case Right(_) if roots.isEmpty        => Left(WatchPathError.Outside)
        case Right(workspace)                 =>
          val path = workspace.path.normalize
          if roots.exists(root => path.startsWith(root.path.normalize)) then Right(workspace)
          else Left(WatchPathError.Outside)

  private def dotDot(raw: String): Boolean =
    raw.split("/", -1).exists(_ == "..")
end WatchRoots
