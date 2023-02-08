package facades.websocket

import scala.scalajs.js
import facades.http.Server
import facades.node.EventEmitter
import scala.scalajs.js.annotation.{JSImport, JSName}
import scala.scalajs.js.typedarray.Uint8Array
import zio.ZIO

@js.native
trait Connection extends EventEmitter {
  def sendBytes(bytes: Uint8Array): Unit = js.native
  def sendUTF(str: String): Unit         = js.native
}

object Connection {

  extension (conn: Connection) {
    def onMessage(handler: IBinaryMessage => Unit): Unit =
      conn.on[IBinaryMessage]("message", handler)

    def onEjotiMessage(handler: ejoti.domain.WebSocketResponse.Message => Unit): Unit =
      onMessage(message => handler(message.toEjotiMessage))

    def onClose(handler: (Int, String) => Unit): Unit =
      conn.on2[Int, String]("close", (reason: Int, description: String) => handler(reason, description))

    def sendMessage(message: ejoti.domain.WebSocketResponse.Message): Unit = message match {
      case ejoti.domain.WebSocketResponse.BinaryMessage(chunk) =>
        conn.sendBytes(
          facades.http.chunkToUint8Array(chunk)
        )
      case ejoti.domain.WebSocketResponse.StringMessage(str) => conn.sendUTF(str)
    }

    def sendMessageZIO(message: ejoti.domain.WebSocketResponse.Message): ZIO[Any, Nothing, Unit] =
      ZIO.succeed(sendMessage(message))
  }

}
