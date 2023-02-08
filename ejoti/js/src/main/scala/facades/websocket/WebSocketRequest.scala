package facades.websocket

import scala.scalajs.js
import facades.http.Server
import facades.node.EventEmitter
import scala.scalajs.js.annotation.{JSImport, JSName}
import facades.http.Request
import ejoti.domain.Status

@js.native
trait WebSocketRequest extends EventEmitter {

  def httpRequest: Request = js.native

  def origin: String = js.native

  @JSName("reject")
  def rejectJS(status: Int, reason: String): Unit = js.native

  def accept(protocol: js.UndefOr[String], allowedOrigin: js.UndefOr[String]): Connection = js.native

}

object WebSocketRequest {

  extension (request: WebSocketRequest) {
    def reject(status: Status[_], reason: String): Unit = request.rejectJS(status.code, reason)
  }

}
