package terret.redact

import java.util.regex.Pattern

/** Strips secrets from text before it leaves terret. The replacement is a fixed token, so a value is not echoed. */
object Redact:
  val Token: String = "[redacted]"

  def text(raw: String): String =
    patterns.foldLeft(raw)(apply)

  private val patterns: List[Pattern] = List(
    Pattern.compile("ghp_[A-Za-z0-9]+"),
    Pattern.compile("gho_[A-Za-z0-9]+"),
    Pattern.compile("github_pat_[A-Za-z0-9_]+"),
    Pattern.compile("whsec_[A-Za-z0-9_\\-+=/]+"),
    Pattern.compile("(?i)bearer\\s+[A-Za-z0-9\\-._~+/=]+"),
    Pattern.compile("xai-[A-Za-z0-9_\\-]+"),
    Pattern.compile("(?i)(x-terret-key\\s*[:=]\\s*)([^\\s\"',}]+)"),
    Pattern.compile("(?:AKIA|ASIA)[0-9A-Z]{16}"),
    Pattern.compile("(?i)(aws_secret_access_key\\s*[=:]\\s*)([A-Za-z0-9/+=]{40})"),
    Pattern.compile("[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}"),
  )

  private def apply(raw: String, pattern: Pattern): String =
    val matcher = pattern.matcher(raw)
    val out     = new StringBuffer
    while matcher.find() do
      val replacement =
        if matcher.groupCount == 2 then matcher.group(1) + Token else Token
      matcher.appendReplacement(out, java.util.regex.Matcher.quoteReplacement(replacement))
    matcher.appendTail(out)
    out.toString
end Redact
