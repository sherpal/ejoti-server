package ejoti.domain

import zio.Chunk
import zio.stream.ZStream
import zio.{Queue, ZIO}
import scala.concurrent.duration.Duration
import java.nio.charset.StandardCharsets

final case class Response(status: Status[Int], headers: List[Header], body: ZStream[Any, Nothing, Byte]) {
  def withStatus(otherStatus: Status[Int]): Response = copy(status = otherStatus)

  def withBodyString(str: String): Response = Response.fromBodyString(status, headers, str)

  def addOrReplaceHeader(header: Header): Response = {
    val newHeaders = header +: headers.filterNot(_.name == header.name)
    copy(headers = newHeaders)
  }

  def setCookie(cookie: HttpCookie): Response = copy(headers = cookie.setCookie +: headers)

  def deleteCookie(cookieName: String): Response = setCookie(
    HttpCookie(cookieName, "", maybeMaxAge = Some(Duration.Zero))
  )
}

object Response {

  def empty(status: Status[Int]): Response = empty(status, Nil)

  def empty(status: Status[Int], headers: List[Header]): Response = Response(status, headers, ZStream.empty)

  def fromBodyString(status: Status[Int], headers: List[Header], body: String): Response = {
    val bytes = {
      import scala.language.unsafeNulls
      body.getBytes(StandardCharsets.UTF_8)
    }
    Response(
      status,
      headers :+ Header.ContentType("text/plain"),
      ZStream.fromChunk(Chunk.fromArray(bytes))
    )
  }

  def Ok                            = empty(Status.Ok)
  def BadRequest                    = empty(Status.BadRequest)
  def Unauthorized                  = empty(Status.Unauthorized)
  def Forbidden                     = empty(Status.Forbidden)
  def TemporaryRedirect(to: String) = empty(Status.TemporaryRedirect, List(Header.Location(to)))
  def NotFound                      = empty(Status.NotFound)
  def MethodNotAllowed              = empty(Status.MethodNotAllowed)

}
