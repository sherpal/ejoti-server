package ejoti.domain

import ejoti.domain.Request.RawRequest
import zio.ZIO
import zio.stream.ZStream

import java.nio.charset.StandardCharsets

/**
 * A [[Middleware]] is a global way to act on a [[Server]]. It allows you to tweak the handler of the [[Server]]
 * by doing something before or after (or both) the processing of a request.
 */
trait Middleware[-R, Req, Res, NewReq, NewRes] {

  def transform[R0 <: R](handler: Req => ZIO[R0, Nothing, Res]): NewReq => ZIO[R0, Nothing, NewRes]

  final def apply[R0 <: R](server: Server[R0, Req, Res]): Server[R0, NewReq, NewRes] =
    Server.MiddlewareApplied(server, this)

}

object Middleware {

  def identity[Req, Res]: Middleware[Any, Req, Res, Req, Res] = new Middleware[Any, Req, Res, Req, Res] {
    override def transform[R0 <: Any](handler: Req => ZIO[R0, Nothing, Res]): Req => ZIO[R0, Nothing, Res] = handler
  }

  case class ContramapMiddlewareZIO[R, OldReq, NewReq, Res](f: NewReq => ZIO[R, Nothing, OldReq])
      extends Middleware[R, OldReq, Res, NewReq, Res] {
    override def transform[R0 <: R](handler: OldReq => ZIO[R0, Nothing, Res]): NewReq => ZIO[R0, Nothing, Res] =
      f(_).flatMap(handler)
  }

  def decodeBody[R, Body, Res](
      decoder: ZStream[Any, Nothing, Byte] => ZIO[R, Nothing, Body]
  ): Middleware[R, Request[Body], Res, Request.RawRequest, Res] =
    ContramapMiddlewareZIO(_.mapBody(decoder))

  def decodeBodyAsString[Res]: Middleware[Any, Request[String], Res, RawRequest, Res] = decodeBody(
    _.runCollect.map(_.toArray).map(new String(_, StandardCharsets.UTF_8))
  )

}
