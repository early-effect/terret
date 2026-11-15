package terret.grok

import zio.json.*
import zio.test.*

object ToolTextSpec extends ZIOSpecDefault:
  def spec = suite("ToolText")(
    test("a string and a byte array both decode, and encoding writes a string") {
      assertTrue(
        "\"hi\"".fromJson[ToolText].map(_.text) == Right("hi"),
        "[60,119,111,114,107]".fromJson[ToolText].map(_.text) == Right("<work"),
        "[]".fromJson[ToolText].map(_.text) == Right(""),
        ToolText("<work").toJson == "\"<work\"",
      )
    },
    test("a byte outside 0 to 255 and invalid utf-8 fail inside the codec") {
      assertTrue(
        "[256]".fromJson[ToolText] == Left("(byte out of range)"),
        "[255]".fromJson[ToolText] == Left("(invalid utf-8)"),
      )
    },
    test("stdout, stderr, and output are text, and a bad value stays as it was") {
      val raw =
        """{"stdout":[256],"stderr":[255],"output":"ok","noise":[60,119],""" +
          """"nested":{"stdout":[60,119,111,114,107]},"gone":null}"""
      val expected =
        """{"stdout":[256],"stderr":[255],"output":"ok","noise":[60,119],""" +
          """"nested":{"stdout":"<work"},"gone":null}"""
      assertTrue(raw.fromJson[ToolPayload].map(_.json.toJson) == Right(expected))
    },
  )
