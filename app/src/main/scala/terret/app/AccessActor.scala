package terret.app

import heddle.http.header.Headers

/** The viewer hash of a presented Access JWT. A missing header is nobody. The token itself is not kept. */
object AccessActor:
  def of(headers: Headers): Option[String] =
    headers.get(Header).filter(_.nonEmpty).map(AccessJwt.viewer)

  val Header: String = "cf-access-jwt-assertion"
end AccessActor
