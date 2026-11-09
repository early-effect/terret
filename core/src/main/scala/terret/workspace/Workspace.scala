package terret.workspace

import java.nio.file.{InvalidPathException, Path}

final case class Workspace private (path: Path)

object Workspace:
  def from(raw: String): Either[WorkspaceError, Workspace] =
    if raw.isEmpty then Left(WorkspaceError.Empty)
    else
      try
        val path = Path.of(raw)
        if path.isAbsolute then Right(Workspace(path.normalize))
        else Left(WorkspaceError.Relative(raw))
      catch case _: InvalidPathException => Left(WorkspaceError.Invalid(raw))
end Workspace

enum WorkspaceError:
  case Empty
  case Relative(raw: String)
  case Invalid(raw: String)
end WorkspaceError
