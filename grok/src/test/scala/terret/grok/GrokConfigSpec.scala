package terret.grok

import zio.test.*

object GrokConfigSpec extends ZIOSpecDefault:
  def spec = suite("GrokConfig")(
    test("command, host, secret, and port are checked") {
      assertTrue(
        GrokConfig.make("", GrokTransport.Cli, GrokApproval.Ask, None) == Left(GrokConfigError.EmptyCommand),
        AcpBind.make("", 1, "s") == Left(GrokConfigError.EmptyHost),
        AcpBind.make("127.0.0.1", 1, "") == Left(GrokConfigError.EmptySecret),
        AcpBind.make("127.0.0.1", 0, "s") == Left(GrokConfigError.PortOutOfRange(0)),
        GrokConfig.unbound.bind.isEmpty,
      )
    },
  )
end GrokConfigSpec
