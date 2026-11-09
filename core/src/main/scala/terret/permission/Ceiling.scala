package terret.permission

final case class Ceiling(allow: Set[Permission]):
  def clamp(cap: Ceiling): (Ceiling, Set[Permission]) =
    val kept = allow.intersect(cap.allow)
    (Ceiling(kept), allow.diff(cap.allow))

object Ceiling:
  val all: Ceiling    = Ceiling(Permission.values.toSet)
  val closed: Ceiling = Ceiling(Set(Permission.ReadWorkspace))
end Ceiling
