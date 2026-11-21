package terret.grok

import zio.Chunk
import zio.test.*

object ShellWriteSpec extends ZIOSpecDefault:
  def spec = suite("ShellWrite")(
    test("redirects to a file write, and /dev/null does not") {
      assertTrue(
        ShellWrite.effect("echo hi > /dev/null") == ShellEffect.ReadOnly,
        ShellWrite.effect("echo hi > out.txt") == ShellEffect.Writes(Chunk("out.txt")),
        ShellWrite.effect("echo hi >> out.txt") == ShellEffect.Writes(Chunk("out.txt")),
        ShellWrite.effect("echo hi >/dev/null") == ShellEffect.ReadOnly,
        ShellWrite.effect("echo hi 2>&1") == ShellEffect.ReadOnly,
      )
    },
    test("tee, cp, and an in-place edit name the file they write") {
      assertTrue(
        ShellWrite.effect("echo hi | tee") == ShellEffect.ReadOnly,
        ShellWrite.effect("echo hi | tee out.txt") == ShellEffect.Writes(Chunk("out.txt")),
        ShellWrite.effect("echo hi | tee /tmp/a.log") == ShellEffect.Writes(Chunk("/tmp/a.log")),
        ShellWrite.effect("cp README.md target/x.md") == ShellEffect.Writes(Chunk("target/x.md")),
        ShellWrite.effect("sed -i 's/a/b/' file") == ShellEffect.Writes(Chunk("s/a/b/", "file")),
        ShellWrite.effect("sudo tee /tmp/x") == ShellEffect.Writes(Chunk("/tmp/x")),
        ShellWrite.effect("dd of=out.bin") == ShellEffect.Writes(Chunk("out.bin")),
      )
    },
    test("a command with no visible write is read-only, and a broken quote is opaque") {
      assertTrue(
        ShellWrite.effect("./histos --version") == ShellEffect.ReadOnly,
        ShellWrite.effect("echo hi | cat") == ShellEffect.ReadOnly,
        ShellWrite.effect("echo 'hi") == ShellEffect.Opaque,
        ShellWrite.effect("   ") == ShellEffect.Opaque,
      )
    },
  )
