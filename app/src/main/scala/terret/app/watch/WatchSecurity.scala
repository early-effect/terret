package terret.app.watch

import heddle.http.{MediaType, Method, Request, Response, Status}
import zio.{UIO, ZIO}

import java.util.Base64

/** Headers, the generic denial, and the cross-site check. HSTS belongs on the tunnel listener only. */
object WatchSecurity:
  val Denied: String = "access denied"

  def denied: Response =
    notice(Denied, Status.Unauthorized)

  def forbidden: Response =
    notice(Denied, Status.Forbidden)

  def badQuery: Response =
    notice("bad query", Status.BadRequest)

  def busy: Response =
    notice("too many streams", Status.TooManyRequests)

  def unsupported: Response =
    notice("unsupported media type", Status.UnsupportedMediaType)

  /** A same-origin document for a short refusal. The message is the only fact the page states. */
  def notice(message: String, status: Status): Response =
    Response.html(sheet(message), status)

  private def sheet(message: String): String =
    s"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><meta name="color-scheme" content="light dark"><title>terret watch</title><link rel="stylesheet" href="/watch.css"></head><body><main data-state="notice"><p data-brand="mark">terret <span data-brand="sub">watch</span></p><h1>${escape(message)}</h1></main></body></html>"""

  private def escape(raw: String): String =
    raw.flatMap {
      case '&' => "&amp;"
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case ch  => ch.toString
    }

  def html(body: String, nonce: String): Response =
    Response
      .html(body)
      .withHeader("Content-Security-Policy", csp(Some(nonce)))
      .withHeader("Cache-Control", "no-store")

  def seal(response: Response, hsts: Boolean): Response =
    val policy = response.header("Content-Security-Policy").getOrElse(csp(None))
    val marked = response
      .withHeader("X-Content-Type-Options", "nosniff")
      .withHeader("X-Frame-Options", "DENY")
      // `no-referrer` makes Chromium send Origin: null on a same-origin form POST, which sameSite then refuses.
      .withHeader("Referrer-Policy", "strict-origin")
      .withHeader("Content-Security-Policy", policy)
    if hsts then marked.withHeader("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
    else marked

  def csp(nonce: Option[String]): String =
    val style = nonce match
      case Some(value) => s"style-src 'self' 'nonce-$value'"
      case None        => "style-src 'self'"
    s"default-src 'self'; script-src 'self'; $style; base-uri 'self'; form-action 'self'; frame-ancestors 'none'"

  /** `/mcp` keeps the events-key check. Every other tunnel path requires the Access JWT. */
  def mcp(request: Request): Boolean =
    request.path.segments.headOption.contains("mcp")

  def watch(request: Request): Boolean =
    request.path.segments.headOption.exists(segment =>
      segment == "watch" || segment == "watch.js" || segment == "watch.css"
    )

  /** A present `Sec-Fetch-Site: cross-site`, or an Origin whose host is not the request Host, refuses a mutating
    * request, including `/mcp`. Absent browser headers are allowed, so a local client that sends no Origin still
    * reaches the JSON check.
    */
  def crossSite(request: Request): Option[Response] =
    if !mutating(request.method) || !crossed(request) then None
    else Some(forbidden)

  /** Mutating requests, in order: cross-site metadata, then a positive same-site check on `/watch`, then the content
    * type. JSON routes require `application/json`. Watch forms require `application/x-www-form-urlencoded` and a
    * same-site `Origin` plus `Sec-Fetch-Site`. The synchronizer token is checked by the watch handler.
    */
  def screen(request: Request): Option[Response] =
    if !mutating(request.method) then None
    else if crossed(request) then Some(forbidden)
    else if watch(request) then
      if sameSite(request) && formContent(request) then None
      else if !sameSite(request) then Some(forbidden)
      else Some(unsupported)
    else if jsonContent(request) then None
    else Some(unsupported)

  private def sameSite(request: Request): Boolean =
    val fetch  = request.header("Sec-Fetch-Site").map(_.trim.toLowerCase)
    val origin = request.header("Origin").map(_.trim).filter(_.nonEmpty)
    val host   = request.header("Host").map(hostOnly)
    val byFetch = fetch.contains("same-origin") || fetch.contains("same-site")
    val byOrigin = origin.exists { raw =>
      hostOf(raw) match
        case Some(originHost) => host.contains(originHost)
        case None             => false
    }
    byFetch && byOrigin

  private def jsonContent(request: Request): Boolean =
    request.header("Content-Type").flatMap(MediaType.parse).exists(_.isJson)

  private def formContent(request: Request): Boolean =
    request.header("Content-Type").flatMap(MediaType.parse).exists { kind =>
      kind.mainType == "application" && kind.subType == "x-www-form-urlencoded"
    }

  private def mutating(method: Method): Boolean =
    method match
      case Method.POST | Method.PUT | Method.DELETE | Method.PATCH                                      => true
      case Method.GET | Method.HEAD | Method.CONNECT | Method.OPTIONS | Method.TRACE | Method.Custom(_) => false

  private def crossed(request: Request): Boolean =
    val fetch    = request.header("Sec-Fetch-Site").map(_.trim.toLowerCase)
    val origin   = request.header("Origin").map(_.trim).filter(_.nonEmpty)
    val host     = request.header("Host").map(hostOnly)
    val byFetch  = fetch.contains("cross-site")
    val byOrigin = origin.exists { raw =>
      hostOf(raw) match
        case None             => true
        case Some(originHost) =>
          host match
            case None              => true
            case Some(requestHost) => originHost != requestHost
    }
    byFetch || byOrigin

  private def hostOf(origin: String): Option[String] =
    val rest = origin.indexOf("://") match
      case -1    => origin
      case index => origin.substring(index + 3)
    val host = rest.takeWhile(ch => ch != '/' && ch != '?' && ch != '#')
    if host.isEmpty then None else Some(hostOnly(host))

  private def hostOnly(host: String): String =
    val bare = host.trim.toLowerCase
    if bare.startsWith("[") then bare
    else
      bare.lastIndexOf(':') match
        case -1    => bare
        case index => bare.substring(0, index)
end WatchSecurity

object Nonce:
  /** `SecureRandom` is the nonce source. A failure here is fatal: a guessed nonce would make the style policy useless.
    */
  def next: UIO[String] =
    ZIO.attempt {
      val bytes = new Array[Byte](16)
      random.nextBytes(bytes)
      Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
    }.orDie

  private val random = new java.security.SecureRandom()
end Nonce
