package terret.dsh

import zio.test.*

object DshConfigSpec extends ZIOSpecDefault:
  def spec = suite("DshConfig")(
    test("headless names the command and the profile") {
      assertTrue(
        DshConfig.make("dsh", "headless") == Right(DshConfig.headless),
        DshConfig.headless.withProfile("sdk").map(_.profile) == Right("sdk"),
      )
    },
    test("an empty command or profile is rejected") {
      assertTrue(
        DshConfig.make("", "headless") == Left(DshConfigError.EmptyCommand),
        DshConfig.make("dsh", "") == Left(DshConfigError.EmptyProfile),
        DshConfig.headless.withProfile("") == Left(DshConfigError.EmptyProfile),
      )
    },
  )
end DshConfigSpec
