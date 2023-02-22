package ejoti.domain.nodeast

import ejoti.domain.Node
import ejoti.domain.Node.given

import zio.test._
import zio.ZIO
import ejoti.domain.*

object FromMirrorSpecs extends ZIOSpecDefault {

  sealed trait X
  case object A extends X
  case class B() extends X
  case class C(x: String) extends X

  val aLeaf = Node.leaf((a: A.type) => ZIO.succeed(Response.empty(Status.Ok)))
  val bLeaf = Node.leaf((b: B) => ZIO.succeed(Response.empty(Status.NotFound)))
  val cLeaf =
    Node.leaf((c: C) => ZIO.succeed(Response.empty(Status.TemporaryRedirect).addOrReplaceHeader(Header.Location(c.x))))

  val xNodeToLeaf = FromMirror[X].fillFirstOutlet(aLeaf).fillFirstOutlet(bLeaf).fillFirstOutlet(cLeaf)

  def spec =
    suite("FromMirrorSpecs")(
      test("Instances of X get properly dispatched")(for {
        request <- ZIO.succeed(TestRequest.fromBodyString(""))
        aResponse <- Node
          .fromValue[X](A)
          .fillFirstOutlet(xNodeToLeaf)
          .asServer
          .handleRequest(request)
          .flatMap(TestResponse.fromResponseZIO(_, _.asString))
        bResponse <- Node
          .fromValue[X](B())
          .fillFirstOutlet(xNodeToLeaf)
          .asServer
          .handleRequest(request)
          .flatMap(TestResponse.fromResponseZIO(_, _.asString))
        cResponse <- Node
          .fromValue[X](C("hello"))
          .fillFirstOutlet(xNodeToLeaf)
          .asServer
          .handleRequest(request)
          .flatMap(TestResponse.fromResponseZIO(_, _.asString))
      } yield assertTrue(aResponse.assertCode[200], bResponse.assertCode[404], cResponse.assertCode[307]))
    )

}
