package terret.permission

enum PermissionPolicy:
  case Closed
  case Clamp(cap: Ceiling)
  case Inherit

  def ceiling: Ceiling = this match
    case Closed         => Ceiling.closed
    case Clamp(ceiling) => ceiling
    case Inherit        => Ceiling.all
end PermissionPolicy
