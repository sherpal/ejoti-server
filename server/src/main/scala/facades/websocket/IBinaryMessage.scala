package facades.websocket

import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js

trait IBinaryMessage extends js.Object {
  def `type`: String

  def binaryData: Uint8Array

  def utf8Data: String
}

object IBinaryMessage {

  extension (message: IBinaryMessage) {
    def toEjotiMessage: ejoti.domain.WebSocketResponse.Message =
      if message.`type` == "utf8" then ejoti.domain.WebSocketResponse.Message(message.utf8Data)
      else ejoti.domain.WebSocketResponse.Message(facades.http.uint8ArrayToChunk(message.binaryData))
  }

}
