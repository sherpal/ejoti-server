package server

import ejoti.domain.{Header, Middleware, Request, Response, Server, Status}
import facades.http.Http.http
import facades.console.console
import ejoti.domain.Node
import ejoti.domain.Node.*
import urldsl.language.dummyErrorImpl.*
import ejoti.domain.CollectedInfo.given

import scala.scalajs.js
import zio.{Unsafe, ZIO}

// Invoke-WebRequest -Uri http://localhost:3000/hello/stuff?hey=3 -Method POST -Body "hi"
// Example http server
@main def run(): Unit = {

  import ejoti.domain.Node.given

  val helloPath = navigation
    .pathPrefix(root / "hello")
    .fillOutlet[0](notFound)

  val step1 = Node.decodeBodyAsString
  val step2 = step1.fillOutlet[0](mappingNode((req: Request[String]) => s"You sent me: ${req.body}"))
  val step3 = step2.fillOutlet[0](printInfo[Request[String]])
  val step4 = step3.fillOutlet[0](printRequest)
  val step5 = step4.fillOutlet[0](Node.okString)

  val serverTree = step5

  val app = for {
    server <- http.createServerZIO(serverTree.asServer)
    _      <- server.listenZIO(3000)(ZIO.succeed(println("I'm on!")))
  } yield ()

  Unsafe.unsafe(implicit unsafe => zio.Runtime.default.unsafe.runToFuture(app))
  ()
}
