package server

import ejoti.domain.{Header, Middleware, Request, Response, Server, Status}
import facades.http.Http.http
import facades.console.console
import ejoti.domain.Node
import ejoti.domain.Node.*
import ejoti.domain.WebSocketResponse
import urldsl.language.dummyErrorImpl.*
import ejoti.domain.CollectedInfo.given

import scala.scalajs.js
import zio.{Unsafe, ZIO}
import facades.websocket.{Connection, WebSocketRequest, WebSocketServer}
import fs2.io.file.Path

// Invoke-WebRequest -Uri http://localhost:3000/hello/stuff?hey=3 -Method POST -Body "hi"
// Example http server
@main def run(): Unit = {

  import ejoti.domain.Node.given

  val helloPath = navigation
    .pathPrefix(root / "hello")

  val step1 = Node.decodeBodyAsString
  val step2 = step1.fillOutlet[0](mappingNode((req: Request[String]) => s"You sent me: ${req.body}"))
  val step3 = step2.fillOutlet[0](printInfo[Request[String]])
  val step4 = step3.fillOutlet[0](printRequest)
  val step5 = step4.fillOutlet[0](Node.okString)

  val helloPathFilled = helloPath.fillOutlet[1](step5)

  val serverTree = helloPathFilled.fillOutlet[0](Node.sendFixedFile(Path("./test.txt")))

  val webSocketServer = Node.leaf((req: Request.RawRequest) => ZIO.succeed(WebSocketResponse.echoWithLog))

  val app = for {
    server <- http.createServerZIO(serverTree.asServer)
    _      <- server.listenZIO(3000)(ZIO.succeed(println("I'm on!")))
    _ <- WebSocketServer.fromEjotiWebSocketServer(
      server,
      webSocketServer.asWebSocketServer,
      autoAcceptConnections = false
    )
    _ <- ZIO.never
  } yield ()

  Unsafe.unsafe(implicit unsafe => zio.Runtime.default.unsafe.runToFuture(app))
  ()
}
