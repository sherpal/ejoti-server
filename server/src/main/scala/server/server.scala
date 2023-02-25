package server

import ejoti.domain.{Header, Middleware, Request, Response, Server, Status}
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
import fs2.io.file.Flags

// Invoke-WebRequest -Uri http://localhost:3000/hello/stuff?hey=3 -Method POST -Body "hi"
// Example http server
@main def run(): Unit = {

  import ejoti.domain.Node.given

  val helloPath = navigation.initialSegments
    .outlet[0]
    .attach(
      navigation
        .pathPrefix(root / "hello")
    )

  val step1 = Node.decodeBodyAsString
  val step2 = step1.outlet[0].attach(mappingNode((req: Request[String]) => s"You sent me: ${req.body}"))
  val step3 = step2.outlet[0].attach(printInfo[Request[String]])
  val step4 = step3.outlet[0].attach(printRequest)
  val step5 = step4.outlet[0].attach(Node.okString)

  val helloPathFilled = helloPath.outlet[1].attach(step5)

  val serverTree =
    helloPathFilled.outlet[0].attach(Node.serveStaticOrNotFound(Path("./demo-static-folder"), root / "static"))

  val webSocketServer = Node.leaf((req: Request.RawRequest) => ZIO.succeed(WebSocketResponse.echoWithLog))

  val app =
    ejoti.createServer(3000)(serverTree.asServer, ZIO.succeed(println("I'm on!")), webSocketServer.asWebSocketServer)

  Unsafe.unsafe(implicit unsafe => zio.Runtime.default.unsafe.runToFuture(app))
  ()
}
