package terret.github

/** Denied by default. An empty set grants nothing. `write` does not include `read`. */
final case class GhAccess(grants: Set[GhGrant]):
  def allows(grant: GhGrant): Boolean =
    grants.contains(grant)

object GhAccess:
  val empty: GhAccess = GhAccess(Set.empty)

  def parse(raw: String): Either[GhGrantError, GhAccess] =
    val trimmed = raw.trim
    if trimmed.isEmpty then Right(empty)
    else
      trimmed
        .split(",", -1)
        .toList
        .foldLeft[Either[GhGrantError, Set[GhGrant]]](Right(Set.empty)): (acc, part) =>
          acc.flatMap: grants =>
            val token = part.trim
            token match
              case "read" if grants.contains(GhGrant.Read)   => Left(GhGrantError.Duplicate("read"))
              case "write" if grants.contains(GhGrant.Write) => Left(GhGrantError.Duplicate("write"))
              case "read"                                    => Right(grants + GhGrant.Read)
              case "write"                                   => Right(grants + GhGrant.Write)
              case other                                     => Left(GhGrantError.BadToken(other))
        .map(GhAccess(_))
end GhAccess

enum GhGrantError:
  case BadToken(value: String)
  case Duplicate(grant: String)

  def render: String = this match
    case BadToken(value)  => s"'$value' is not a gh grant"
    case Duplicate(grant) => s"$grant is granted twice"
end GhGrantError
