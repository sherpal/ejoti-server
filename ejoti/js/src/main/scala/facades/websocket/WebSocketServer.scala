package facades.websocket

import scala.scalajs.js
import facades.http.Server
import facades.node.EventEmitter
import scala.scalajs.js.annotation.{JSImport, JSName}
import zio.{Unsafe, ZIO}
import scala.util.Failure
import scala.util.Success
import ejoti.domain.Status

@js.native
@JSImport("websocket", "server")
final class WebSocketServer(options: WebSocketServer.Options) extends EventEmitter.Native {}

object WebSocketServer {

  trait Options extends js.Object {
    val httpServer: Server
    val autoAcceptConnections: Boolean
  }

  object Options {
    def apply(server: Server, autoAcceptConnections0: Boolean): Options = new Options {
      val httpServer: Server             = server
      val autoAcceptConnections: Boolean = autoAcceptConnections0
    }
  }

  def apply(server: Server, autoAcceptConnections: Boolean): WebSocketServer = {
    val options = Options(server, autoAcceptConnections)
    new WebSocketServer(options)
  }

  def fromEjotiWebSocketServer[R](
      server: Server,
      webSocketServer: ejoti.domain.WebSocketServer[R],
      autoAcceptConnections: Boolean
  ): ZIO[R, Nothing, WebSocketServer] =
    for {
      wsServer <- ZIO.succeed(apply(server, autoAcceptConnections))
      runtime  <- ZIO.runtime[R]
      _ <- ZIO.succeed {
        wsServer.onRequestJS { request =>
          import scala.concurrent.ExecutionContext.Implicits.global
          runtime.unsafe.runToFuture(for {
            ejotiRequest   <- request.httpRequest.toEjotiRequest
            serverResponse <- webSocketServer.handleRequest(ejotiRequest)
            _ <- serverResponse match {
              case response: ejoti.domain.WebSocketResponse.RejectResponse =>
                ZIO.succeed {
                  request.reject(response.status, response.reason)
                }
              case ejoti.domain.WebSocketResponse.AcceptResponse(messageHandling) =>
                for {
                  incomingQueue  <- zio.Queue.unbounded[ejoti.domain.WebSocketResponse.Message]
                  outgoingStream <- messageHandling(incomingQueue)
                  connection = request.accept((), request.origin)
                  streamFiber <- outgoingStream.foreach(connection.sendMessageZIO).fork
                  _ = connection.onEjotiMessage(message => runtime.unsafe.run(incomingQueue.offer(message)))
                  _ = connection.onClose((_, _) =>
                    runtime.unsafe.runToFuture(incomingQueue.isEmpty.repeatUntil(identity) *> incomingQueue.shutdown)
                  )
                  _ <- streamFiber.join
                } yield ()
            }
          } yield ()) onComplete {
            case Failure(exception) => exception.printStackTrace()
            case Success(_)         =>
          }
        }
      }
    } yield wsServer

  extension (server: WebSocketServer) {
    def onRequestJS(handler: js.Function1[WebSocketRequest, Unit]): Unit = server.on("request", handler)

    def onRequest(handler: WebSocketRequest => Unit): Unit = server.onRequestJS(handler)
  }

}
