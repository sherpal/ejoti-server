package ejoti.domain

import zio.{Chunk, Queue, ZIO}
import zio.stream.ZStream

sealed trait WebSocketResponse

object WebSocketResponse {
  sealed trait Message {
    type Data
    def content: Data
  }
  object Message {
    def apply(content: Chunk[Byte]): Message = BinaryMessage(content)
    def apply(content: String): Message      = StringMessage(content)
  }
  case class BinaryMessage(content: Chunk[Byte]) extends Message {
    type Data = Chunk[Byte]
  }
  case class StringMessage(content: String) extends Message {
    type Data = String
  }

  case class AcceptResponse(messageHandling: Queue[Message] => ZIO[Any, Nothing, ZStream[Any, Nothing, Message]])
      extends WebSocketResponse {
    def mapStream(f: ZStream[Any, Nothing, Message] => ZStream[Any, Nothing, Message]) = AcceptResponse(
      messageHandling(_).map(f)
    )
  }
  case class RejectResponse(status: Status[_], reason: String, headers: List[Header]) extends WebSocketResponse

  def empty: AcceptResponse = AcceptResponse(_ => ZIO.succeed(ZStream.empty))

  def echo: AcceptResponse = AcceptResponse(queue => ZIO.succeed(ZStream.fromQueue(queue)))
  def echoWithLog: AcceptResponse =
    echo.mapStream(_.tap(message => zio.Console.printLine(message).orDie))

}
