package terret

import terret.permission.{Ceiling, Permission}
import zio.test.*

object CeilingSpec extends ZIOSpecDefault:
  def spec = suite("Ceiling")(
    test("clamp keeps the intersection and reports the dropped permissions") {
      val (effective, dropped) = Ceiling.all.clamp(Ceiling.closed)
      assertTrue(
        effective.allow == Set(Permission.ReadWorkspace),
        dropped == Set(Permission.WriteWorkspace, Permission.RunCommands, Permission.Network),
      )
    },
    test("closed preset is read-workspace only") {
      assertTrue(Ceiling.closed.allow == Set(Permission.ReadWorkspace))
    },
    test("clamping to all drops nothing") {
      val (effective, dropped) = Ceiling.all.clamp(Ceiling.all)
      assertTrue(effective == Ceiling.all, dropped.isEmpty)
    },
  )
end CeilingSpec
