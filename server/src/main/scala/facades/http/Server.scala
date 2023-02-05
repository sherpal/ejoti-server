package facades.http

import zio.ZIO

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.typedarray.Uint8Array
import ejoti.domain.WebSocketServer
import zio.Unsafe
import zio.Chunk
import facades.node.EventEmitter

trait Server extends EventEmitter {

  @JSName("listen")
  def listenJS(port: Int, greeting: js.Function0[Unit]): Unit

}

object Server {

  extension (server: Server) {
    def listen(port: Int)(greeting: => Unit): Unit = server.listenJS(port, () => greeting)

    def listenZIO[R](port: Int)(greeting: => ZIO[R, Nothing, Unit]): ZIO[R, Nothing, Unit] =
      ZIO.asyncZIO(register => ZIO.succeed(server.listen(port)(register(greeting))))

    def onUpgradeJS(handler: js.Function3[Request, Socket, Uint8Array, Unit]): Unit = server.on3("upgrade", handler)
  }
}
