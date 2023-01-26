package ejoti.domain

import zio.Chunk
import zio.stream.ZStream

final case class Response(status: Status[Int], headers: List[Header], body: ZStream[Any, Nothing, Chunk[Byte]])

object Response {

  def empty(status: Status[Int]): Response = Response(status, Nil, ZStream.empty)

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

  def Ok               = empty(Status.Ok)
  def NotFound         = empty(Status.NotFound)
  def MethodNotAllowed = empty(Status.MethodNotAllowed)

}
