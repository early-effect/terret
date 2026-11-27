package terret.app

import heddle.http.{Request, Response}
import heddle.route.{Handler, Routes}
import terret.app.watch.WatchSecurity
import zio.ZIO

/** Local traffic has no Access check. Tunnel traffic requires a JWT except on `/mcp`, which keeps the events key. */
object Ingress:
  def local[R](routes: Routes[R, Response]): Routes[R & AccessGate & AuthLog, Nothing] =
    gate(routes, watch = Routes.empty, hsts = false)

  def tunnel[R](
      routes: Routes[R, Response],
      watch: Routes[AuthLog, Nothing],
  ): Routes[R & AccessGate & AuthLog, Nothing] =
    gate(routes, watch, hsts = true)

  private def gate[R](
      routes: Routes[R, Response],
      watch: Routes[AuthLog, Nothing],
      hsts: Boolean,
  ): Routes[R & AccessGate & AuthLog, Nothing] =
    Routes.fromHandler {
      Handler.fromFunctionZIO { request =>
        WatchSecurity.screen(request) match
          case Some(refused) => ZIO.succeed(WatchSecurity.seal(refused, hsts))
          case None          =>
            if !hsts || WatchSecurity.mcp(request) then answer(routes, request, hsts)
            else
              ZIO
                .serviceWithZIO[AccessGate](_.allow(request.headers))
                .tapError { error =>
                  ZIO.serviceWithZIO[AuthLog](
                    _.record(AuthReason.from(error), request.path.render, AccessActor.of(request.headers))
                  )
                }
                .foldZIO(
                  _ => ZIO.succeed(WatchSecurity.seal(WatchSecurity.denied, hsts)),
                  _ =>
                    if WatchSecurity.watch(request) then watch(request).map(WatchSecurity.seal(_, hsts))
                    else answer(routes, request, hsts),
                )
      }
    }

  private def answer[R](routes: Routes[R, Response], request: Request, hsts: Boolean): ZIO[R, Nothing, Response] =
    routes(request).merge.map(WatchSecurity.seal(_, hsts))
end Ingress
