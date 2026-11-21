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
    test("a heredoc body does not hide the redirect, and the delimiter is not a target") {
      val quoted =
        "mkdir -p /tmp/fts5-probe && cat > /tmp/fts5-probe/Fts5Probe.java << 'EOF'\nclass Probe { String q = \"x; }\nEOF"
      val sameLine = "cat << 'EOF' > /tmp/out.txt\nhello\nEOF"
      val dashed   = "cat <<-EOF > /tmp/out.txt\n\thello\nEOF"
      val plain    = "cat << out.txt\nhello\nout.txt"
      val here     = "cat <<< 'hi'"
      val open     = "cat > out.txt << 'EOF'\nclass Probe { String q = \"x;"
      assertTrue(
        ShellWrite.effect(quoted) == ShellEffect.Writes(Chunk("/tmp/fts5-probe/Fts5Probe.java")),
        ShellWrite.effect(sameLine) == ShellEffect.Writes(Chunk("/tmp/out.txt")),
        ShellWrite.effect(dashed) == ShellEffect.Writes(Chunk("/tmp/out.txt")),
        ShellWrite.effect(plain) == ShellEffect.ReadOnly,
        ShellWrite.effect(here) == ShellEffect.ReadOnly,
        ShellWrite.effect(open) == ShellEffect.Opaque,
      )
    },
  )
