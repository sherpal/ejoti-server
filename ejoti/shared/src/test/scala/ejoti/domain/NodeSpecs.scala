package ejoti.domain

import ejoti.domain.Node.*
import urldsl.language.dummyErrorImpl.*
import scala.collection.mutable

import urldsl.vocabulary.Segment

import zio.test._
import zio.ZIO

object NodeSpecs extends ZIOSpecDefault {

  import ejoti.domain.Node.given
  import CollectedInfo.given

  def spec =
    suite("NodeSpecs")(
      test("A simple server example") {
        val helloPath = navigation
          .pathPrefix(root / "hello")
          .fillOutlet[0](notFound)
        val serverTree = Node.decodeBodyAsString
          .fillOutlet[0](mappingNode((req: Request[String]) => s"You sent me: ${req.body}"))
          .fillOutlet[0](printInfo[Request[String]])
          .fillOutlet[0](printRequest)
          .fillOutlet[0](helloPath)
          .fillOutlet[0](Node.okString)

        val program = serverTree.asServer.handlerRequest(
          TestRequest.fromBodyString("body").withSegments(Segment("hello"))
        )

        for {
          response     <- program
          testResponse <- TestResponse.fromResponseZIO(response, _.asString)
        } yield assertTrue(testResponse.assertCode[200], testResponse.body == "You sent me: body")

      },
      test("Providing Int for AdditionalInfo") {
        val baseNode               = Node.fromValue("hello")
        val nodeWithAdditionalInfo = baseNode.provide[Int *: EmptyTuple]

        for {
          nodeOut <- nodeWithAdditionalInfo.outIfIndex[0](CollectedInfo.empty + 3 + "other string").someOrFailException
        } yield assertTrue(nodeOut.access[String] == "hello", nodeOut.access[Int] == 3)
      },
      test("Asking for Int proof") {

        val baseNode      = Node.fromValue("hello")
        val nodeWithProof = baseNode.withProofOf[Int]

        for {
          nodeOut <- nodeWithProof.outIfIndex[0](CollectedInfo.empty.add(3)).someOrFailException
        } yield assertTrue(nodeOut == "hello")
      },
      test("Retrieving Method from the request in Crud") {
        val node = Node.crudNode

        val testPostRequest    = TestRequest.fromMethod(HttpMethod.POST)
        val testGetRequest     = TestRequest.fromMethod(HttpMethod.GET)
        val testPatchRequest   = TestRequest.fromMethod(HttpMethod.PATCH)
        val testDeleteRequest  = TestRequest.fromMethod(HttpMethod.DELETE)
        val testOptionsRequest = TestRequest.fromMethod(HttpMethod.OPTIONS)

        for {
          nodePostOut    <- node.outIfIndex[0](CollectedInfo.empty + testPostRequest)
          nodeGetOut     <- node.outIfIndex[1](CollectedInfo.empty + testGetRequest)
          nodePatchOut   <- node.outIfIndex[2](CollectedInfo.empty + testPatchRequest)
          nodeDeleteOut  <- node.outIfIndex[3](CollectedInfo.empty + testDeleteRequest)
          nodeOptionsOut <- node.outIfIndex[0](CollectedInfo.empty + testOptionsRequest)
        } yield assertTrue(
          nodePostOut   == Some(HttpMethod.POST),
          nodeGetOut    == Some(HttpMethod.GET),
          nodePatchOut  == Some(HttpMethod.PATCH),
          nodeDeleteOut == Some(HttpMethod.DELETE),
          nodeOptionsOut match {
            case Some(response: Response) => response.status.code == 405
            case _                        => false
          }
        )
      },
      test("Filling a Crud node") {
        def itWasMethod[Method <: HttpMethod](using methodValue: ValueOf[Method]) =
          Node.fromValue(s"It was ${methodValue.value}!").withProofOf[Method].fillOutlet[0](Node.okString)

        val tree = Node.crudNode
          .fillFirstOutlet(itWasMethod[HttpMethod.POST])
          .fillFirstOutlet(itWasMethod[HttpMethod.GET])
          .fillFirstOutlet(itWasMethod[HttpMethod.PATCH])
          .fillFirstOutlet(itWasMethod[HttpMethod.DELETE])

        val server = tree.asServer

        for {
          postMessage <- server
            .handlerRequest(TestRequest.fromMethod(HttpMethod.POST))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          getMessage <- server
            .handlerRequest(TestRequest.fromMethod(HttpMethod.GET))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          patchMessage <- server
            .handlerRequest(TestRequest.fromMethod(HttpMethod.PATCH))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          deleteMessage <- server
            .handlerRequest(TestRequest.fromMethod(HttpMethod.DELETE))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          optionsMessage <- server
            .handlerRequest(TestRequest.fromMethod(HttpMethod.OPTIONS))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
        } yield assertTrue(
          postMessage    == "It was POST!",
          getMessage     == "It was GET!",
          patchMessage   == "It was PATCH!",
          deleteMessage  == "It was DELETE!",
          optionsMessage == ""
        )
      },
      test("Crud node with provided content") {
        val baseNode = Node.okString.withProofOf[HttpMethod.POST]

        val crudProvided = Node.crudNode
          .provide[Node.Singleton[String]]
          .fillFirstOutlet(baseNode)

        for {
          probablyResponse <- crudProvided
            .out(CollectedInfo.empty + TestRequest.fromMethod(HttpMethod.POST) + "hello")
            .asSome
            .map(_.collect { case response: Response => response })
            .someOrFailException
          responseBody <- probablyResponse.body.runCollect.map(_.flatten).map(_.asString)
        } yield assertTrue(responseBody == "hello")

      },
      test("Complete Crud node example") {
        import urldsl.language.dummyErrorImpl.*

        val path = root / "my-model" / segment[Long]

        // represent our database
        val data: mutable.Map[Long, String] = mutable.Map.empty

        val createOrPatchNodeSideEffect =
          Node.sideEffectNode((info: CollectedInfo[Long *: Request[String] *: EmptyTuple]) =>
            ZIO.succeed {
              val id    = info.access[Long]
              val value = info.access[Request[String]].body
              println((id, value))
              data += (id -> value)
            }
          )

        val createNode = Node.decodeBodyAsString
          .provide[Long *: EmptyTuple]
          .fillOutlet[0](createOrPatchNodeSideEffect)
          .fillOutlet[0](Node.ok)
          .withProofOf[HttpMethod.POST]

        val getNode = Node
          .failingEitherNode((id: Long) => ZIO.succeed(data.get(id).toRight(Response.NotFound)))
          .fillOutlet[0](Node.okString)
          .withProofOf[HttpMethod.GET]

        val patchNode = Node
          .failingEitherNode((id: Long) => ZIO.succeed(data.get(id).toRight(Response.NotFound)))
          .provide[Request.RawRequest *: EmptyTuple]
          .fillFirstOutlet(Node.decodeBodyAsString)
          .fillFirstOutlet(createOrPatchNodeSideEffect)
          .fillFirstOutlet(Node.ok)
          .withProofOf[HttpMethod.PATCH]

        val deleteNodeSideEffect = Node.sideEffectNode((in: CollectedInfo[Long *: EmptyTuple]) =>
          ZIO.succeed {
            val id = in.access[Long]
            data -= id
          }
        )

        val deleteNode = deleteNodeSideEffect.fillFirstOutlet(Node.ok).withProofOf[HttpMethod.DELETE]

        val crudWithPath =
          Node.navigation
            .path(path)
            .fillFirstOutlet(Node.notFound)
            .fillFirstOutlet(Node.crudNode)
            .fillFirstOutlet(createNode)
            .fillFirstOutlet(getNode)
            .fillFirstOutlet(patchNode)
            .fillFirstOutlet(deleteNode)

        val server = crudWithPath.asServer

        val baseRequest      = TestRequest.get.withPath(path)(0)
        val wrongBaseRequest = TestRequest.get.withPath(path)(1)

        for {
          createRequest  <- ZIO.succeed(baseRequest.withMethod(HttpMethod.POST).withStreamBodyAsString("my-data"))
          createResponse <- server.handlerRequest(createRequest)
          getRequest     <- ZIO.succeed(baseRequest)
          getResponse    <- server.handlerRequest(getRequest).flatMap(_.body.runCollect).map(_.flatten.asString)
          patchRequest  <- ZIO.succeed(baseRequest.withMethod(HttpMethod.PATCH).withStreamBodyAsString("my-other-data"))
          patchResponse <- server.handlerRequest(patchRequest)
          wrongPatchRequest  <- ZIO.succeed(wrongBaseRequest.withMethod(HttpMethod.PATCH).withStreamBodyAsString("meh"))
          wrongPatchResponse <- server.handlerRequest(wrongPatchRequest)
          getResponse2       <- server.handlerRequest(getRequest).flatMap(_.body.runCollect).map(_.flatten.asString)
          deleteRequest      <- ZIO.succeed(baseRequest.withMethod(HttpMethod.DELETE))
          deleteResponse     <- server.handlerRequest(deleteRequest)
          getResponse3       <- server.handlerRequest(getRequest)
        } yield assertTrue(
          createResponse.status.code     == 200,
          getResponse                    == "my-data",
          patchResponse.status.code      == 200,
          wrongPatchResponse.status.code == 404,
          getResponse2                   == "my-other-data",
          deleteResponse.status.code     == 200,
          getResponse3.status.code       == 404
        )
      }
    )

}
