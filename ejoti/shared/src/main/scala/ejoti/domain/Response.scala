package ejoti.domain

import zio.Chunk
import zio.stream.ZStream
import zio.{Queue, ZIO}
import scala.concurrent.duration.Duration
import java.nio.charset.StandardCharsets
import ejoti.domain.Header.Headers

import scala.reflect.Typeable

/** @param status
  * @param headers
  * @param body
  * @param finalizer
  *   effect to be performed after the response has been sent and ended to the client. Guaranteed to happen. This is
  *   good to clean resources like temporary directories...
  */
final case class Response(
    status: Status[Int],
    headers: Headers,
    body: ZStream[Any, Nothing, Byte],
    finalizer: ZIO[Any, Nothing, Unit]
) {
  def withStatus(otherStatus: Status[Int]): Response = copy(status = otherStatus)

  def withBodyString(str: String): Response = Response.fromBodyString(status, headers, str)

  def withBody(newBody: ZStream[Any, Nothing, Byte]): Response =
    copy(headers = headers.removeHeaderOfType[Header.ContentLength], body = newBody)

  def addOrReplaceHeader[T <: Header](header: T)(using Typeable[T]): Response =
    copy(headers = headers.addOrReplaceHeader(header))

  def setCookie(cookie: HttpCookie): Response = copy(headers = headers addHeader cookie.setCookie)

  def deleteCookie(cookieName: String): Response = setCookie(
    HttpCookie(cookieName, "", maybeMaxAge = Some(Duration.Zero))
  )

  /** Returns a new [[Response]] that will perform the given side effect after the body has finished streaming to the
    * client.
    *
    * @param effect
    *   effect to run after the body has been sent.
    * @return
    *   a new [[Response]]
    */
  def ensuring(effect: => ZIO[Any, Nothing, Unit]): Response =
    copy(finalizer = finalizer *> effect)
}

object Response {

  def empty(status: Status[Int]): Response = empty(status, Headers.empty)

  def empty(status: Status[Int], headers: Headers): Response =
    Response(status, headers.addOrReplaceHeader(Header.ContentLength(0)), ZStream.empty, ZIO.unit)

  def fromBodyString(status: Status[Int], headers: Headers, body: String): Response = {
    val bytes = {
      import scala.language.unsafeNulls
      body.getBytes(StandardCharsets.UTF_8)
    }
    fromBodyChunk(status, headers, Chunk.fromArray(bytes))
  }

  def fromBodyChunk(status: Status[Int], headers: Headers, body: Chunk[Byte]): Response = Response(
    status,
    headers
      .addOrReplaceHeaders(Header.ContentType("text/plain"), Header.ContentLength(body.length)),
    ZStream.fromChunk(body),
    ZIO.unit
  )

  def Ok                            = empty(Status.Ok)
  def BadRequest                    = empty(Status.BadRequest)
  def Unauthorized                  = empty(Status.Unauthorized)
  def Forbidden                     = empty(Status.Forbidden)
  def NotModified                   = empty(Status.NotModified)
  def TemporaryRedirect(to: String) = empty(Status.TemporaryRedirect, Headers(Header.Location(to)))
  def NotFound                      = empty(Status.NotFound)
  def MethodNotAllowed              = empty(Status.MethodNotAllowed)

}
