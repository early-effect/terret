package terret.github

/** Why a gh operation did not finish. `Failed.detail` is already redacted. */
enum GhError:
  case NotGranted(grant: GhGrant, op: String)
  case BadRequest(problem: GhProblem)
  case Failed(code: Int, detail: String)
  case Unavailable
  case TooLarge
end GhError

enum GhProblem:
  case Limit(value: Int)
  case Number(value: Int)
  case Run(value: Long)
  case Title
  case Body
  case Branch(raw: String)
  case Detached
end GhProblem

enum GhGrant:
  case Read, Write

  def name: String = this match
    case Read  => "read"
    case Write => "write"
end GhGrant
