package terret.github

import zio.test.*

object GhAccessSpec extends ZIOSpecDefault:
  def spec = suite("gh grant")(
    test("an empty value grants nothing") {
      assertTrue(
        GhAccess.parse("") == Right(GhAccess.empty),
        GhAccess.parse("   ") == Right(GhAccess.empty),
        GhAccess.empty.allows(GhGrant.Read) == false,
        GhAccess.empty.allows(GhGrant.Write) == false,
      )
    },
    test("read and write are separate") {
      val read  = GhAccess.parse("read")
      val write = GhAccess.parse("write")
      val both  = GhAccess.parse("write, read")
      assertTrue(
        read == Right(GhAccess(Set(GhGrant.Read))),
        write == Right(GhAccess(Set(GhGrant.Write))),
        both == Right(GhAccess(Set(GhGrant.Read, GhGrant.Write))),
        read.exists(_.allows(GhGrant.Write)) == false,
        write.exists(_.allows(GhGrant.Read)) == false,
      )
    },
    test("a bad token or a duplicate is refused") {
      assertTrue(
        GhAccess.parse("merge") == Left(GhGrantError.BadToken("merge")),
        GhAccess.parse("read,") == Left(GhGrantError.BadToken("")),
        GhAccess.parse("read,read") == Left(GhGrantError.Duplicate("read")),
        GhAccess.parse("Read") == Left(GhGrantError.BadToken("Read")),
      )
    },
  )
