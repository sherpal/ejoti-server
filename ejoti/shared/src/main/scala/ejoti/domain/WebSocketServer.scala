package ejoti.domain

import zio.ZIO

trait WebSocketServer[-R] {

  def handleRequest(req: Request.RawRequest): ZIO[R, Nothing, WebSocketResponse]

}
