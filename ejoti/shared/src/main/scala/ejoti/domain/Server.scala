package ejoti.domain

import zio.ZIO

/** Represent an Http Server.
  *
  * It is essentially a wrapper around a (ZIO) function Req => Res
  */
trait Server[-R, Req, Res] {

  def handlerRequest(req: Req): ZIO[R, Nothing, Res]

  def attachMiddleware[R0 <: R, NewRes, NewReq](
      middleware: Middleware[R0, Req, Res, NewReq, NewRes]
  ): Server[R0, NewReq, NewRes] = middleware(this)

}

object Server {
  type RawServer[R] = Server[R, Request.RawRequest, Response]

  case class FromFunctionZIO[R, Req, Res](handler: Req => ZIO[R, Nothing, Res]) extends Server[R, Req, Res] {
    override def handlerRequest(req: Req): ZIO[R, Nothing, Res] = handler(req)
  }

  def fromFunctionZIO[R, Req, Res](handler: Req => ZIO[R, Nothing, Res]): Server[R, Req, Res] = FromFunctionZIO(handler)

  def fromFunction[R, Req, Res](handler: Req => Res): Server[R, Req, Res] =
    fromFunctionZIO(req => ZIO.succeed(handler(req)))

  def rawFromFunctionZIO[R](handler: Request.RawRequest => ZIO[R, Nothing, Response]): RawServer[R] = fromFunctionZIO(
    handler
  )

  case class MiddlewareApplied[R, Req, Res, NewRes, NewReq](
      server: Server[R, Req, Res],
      middleware: Middleware[R, Req, Res, NewReq, NewRes]
  ) extends Server[R, NewReq, NewRes] {
    override def handlerRequest(req: NewReq): ZIO[R, Nothing, NewRes] =
      middleware.transform(server.handlerRequest)(req)
  }

}
