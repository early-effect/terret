package terret.redact

import zio.test.*

object RedactSpec extends ZIOSpecDefault:
  def spec = suite("redact")(
    test("tokens, cloudflare secrets, aws keys, and email addresses become one token") {
      val aws = "0123456789abcdef0123456789abcdef01234567"
      val raw =
        "ghp_Abcdefghij gho_Abcdefghij ghs_Abcdefghij ghr_Abcdefghij github_pat_abc_DEF whsec_abc-DEF " +
          "Bearer hunter2 xai-secretvalue x-terret-key: hunter2 AKIAIOSFODNN7EXAMPLE " +
          s"aws_secret_access_key=$aws user@example.com still here"
      val cleaned = Redact.text(raw)
      assertTrue(
        cleaned ==
          "[redacted] [redacted] [redacted] [redacted] [redacted] [redacted] " +
          "[redacted] [redacted] x-terret-key: [redacted] [redacted] " +
          "aws_secret_access_key=[redacted] [redacted] still here",
        !cleaned.contains("ghp_"),
        !cleaned.contains("xai-"),
        !cleaned.contains("hunter2"),
        !cleaned.contains(aws),
        !cleaned.contains("user@example.com"),
        Redact.text("a plain sentence") == "a plain sentence",
        Redact.text("ASIAIOSFODNN7EXAMPLE") == "[redacted]",
      )
    }
  )
