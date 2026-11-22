package terret.grok

import zio.test.*

object NetworkPolicySpec extends ZIOSpecDefault:
  def spec = suite("network hosts")(
    test("an empty grant denies every host") {
      assertTrue(
        NetworkPolicy.parse("") == Right(NetworkPolicy.empty),
        NetworkPolicy.parse("   ") == Right(NetworkPolicy.empty),
        !NetworkPolicy.empty.allows("example.com"),
      )
    },
    test("hosts are exact, folded, and unique") {
      assertTrue(
        NetworkPolicy.parse("Example.COM, api.github.com") == Right(
          NetworkPolicy(Set("example.com", "api.github.com"))
        ),
        NetworkPolicy.parse("example.com").exists(_.allows("Example.COM")),
        NetworkPolicy.parse("example.com, example.com") == Left(NetworkGrantError.Duplicate("example.com")),
        NetworkPolicy.parse("Example.com, example.com") == Left(NetworkGrantError.Duplicate("example.com")),
        NetworkPolicy.parse("127.0.0.1").exists(_.allows("127.0.0.1")),
      )
    },
    test("a bad token is not a host") {
      assertTrue(
        NetworkPolicy.parse("example.com,") == Left(NetworkGrantError.BadToken("")),
        NetworkPolicy.parse("*.example.com") == Left(NetworkGrantError.BadToken("*.example.com")),
        NetworkPolicy.parse("https://example.com") == Left(NetworkGrantError.BadToken("https://example.com")),
        NetworkPolicy.parse("example.com:443") == Left(NetworkGrantError.BadToken("example.com:443")),
        NetworkPolicy.parse("01.2.3.4") == Left(NetworkGrantError.BadToken("01.2.3.4")),
        NetworkPolicy.parse("1.2.3") == Left(NetworkGrantError.BadToken("1.2.3")),
      )
    },
  )
