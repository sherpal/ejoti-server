package ejoti.domain

import zio.Chunk
import zio.stream.ZStream
import zio.{Queue, ZIO}

final case class Response(status: Status[Int], headers: List[Header], body: ZStream[Any, Nothing, Chunk[Byte]]) {
  def withStatus(otherStatus: Status[Int]): Response = copy(status = otherStatus)

  def addOrReplaceHeader(header: Header): Response = {
    val newHeaders = header +: headers.filterNot(_.name == header.name)
    copy(headers = newHeaders)
  }
}

object Response {

  def empty(status: Status[Int]): Response = empty(status, Nil)

  def empty(status: Status[Int], headers: List[Header]): Response = Response(status, headers, ZStream.empty)

  def fromBodyString(status: Status[Int], headers: List[Header], body: String): Response = {
    val bytes = {
      import scala.language.unsafeNulls
      body.getBytes()
    }
    Response(
      status,
      headers :+ Header.ContentType("text/plain"),
      ZStream.succeed(Chunk.fromArray(bytes))
    )
  }

  def Ok                            = empty(Status.Ok)
  def TemporaryRedirect(to: String) = empty(Status.TemporaryRedirect, List(Header.Location(to)))
  def NotFound                      = empty(Status.NotFound)
  def MethodNotAllowed              = empty(Status.MethodNotAllowed)

}
