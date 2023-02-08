package ejoti

import ejoti.domain.Server
import ejoti.domain.Request
import ejoti.domain.Response
import ejoti.domain.WebSocketServer

import zio.ZIO
import facades.http.Http.http

private def createServer[R](
    server: Server[R, Request.RawRequest, Response],
    serverOnEffect: ZIO[R, Nothing, Unit],
    maybeWebSocketServer: Option[WebSocketServer[R]],
    port: Int
): ZIO[R, Nothing, Unit] = for {
  nodeServer <- http.createServerZIO(server)
  _          <- nodeServer.listenZIO(port)(serverOnEffect)
  _ <- maybeWebSocketServer match {
    case None => ZIO.unit
    case Some(webSocketServer) =>
      facades.websocket.WebSocketServer.fromEjotiWebSocketServer(nodeServer, webSocketServer, false)
  }
  _ <- ZIO.never
} yield ()

def createServer[R](port: Int)(
    server: Server[R, Request.RawRequest, Response],
    serverOnEffect: ZIO[R, Nothing, Unit]
): ZIO[R, Nothing, Unit] = createServer(server, serverOnEffect, None, port)
def createServer[R](port: Int)(
    server: Server[R, Request.RawRequest, Response],
    serverOnEffect: ZIO[R, Nothing, Unit],
    webSocketServer: WebSocketServer[R]
): ZIO[R, Nothing, Unit] =
  createServer(server, serverOnEffect, Some(webSocketServer), port)
