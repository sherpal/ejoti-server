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
        val helloPath = navigation.initialSegments
          .outlet[0]
          .attach(
            navigation
              .pathPrefix(root / "hello")
          )
          .outlet[0]
          .attach(notFound)
        val serverTree = Node.decodeBodyAsString
          .outlet[0]
          .attach(mappingNode((req: Request[String]) => s"You sent me: ${req.body}"))
          .outlet[0]
          .attach(printInfo[Request[String]])
          .outlet[0]
          .attach(printRequest)
          .outlet[0]
          .attach(helloPath)
          .outlet[0]
          .attach(Node.okString)

        val program = serverTree.asServer.handleRequest(
          TestRequest.fromBodyString("body").withSegments(Segment("hello"))
        )

        for {
          response     <- program
          testResponse <- TestResponse.fromResponseZIO(response, _.asString)
        } yield assertTrue(testResponse.assertCode[200], testResponse.body == "You sent me: body")

      },
      test("Chaining the path prefixes") {
        val firstPath  = root / "first"
        val secondPath = root / "second"

        def ending(value: String): Header = Header.RawHeader("ending", value)

        val tree = navigation.initialSegments
          .outlet[0]
          .attach(
            navigation
              .pathPrefix(firstPath)
              .outlet[1]
              .attach(
                navigation
                  .pathPrefix(secondPath)
                  .outlet[0]
                  .attach(Node.notFound.mapResponse(_.addOrReplaceHeader(ending("one"))))
                  .outlet[0]
                  .attach(Node.ok.mapResponse(_.addOrReplaceHeader(ending("zero"))))
              )
              .outlet[0]
              .attach(Node.notFound.mapResponse(_.addOrReplaceHeader(ending("two"))))
          )

        val server = tree.asServer
        for {
          firstResponse      <- server.handleRequest(TestRequest.get.withSegments(Segment("first")))
          firstTestResponse  <- TestResponse.fromResponseZIO(firstResponse, _.asString)
          secondResponse     <- server.handleRequest(TestRequest.get.withSegments(Segment("first"), Segment("second")))
          secondTestResponse <- TestResponse.fromResponseZIO(secondResponse, _.asString)
          thirdResponse      <- server.handleRequest(TestRequest.get.withSegments(Segment("other")))
          thirdTestResponse  <- TestResponse.fromResponseZIO(thirdResponse, _.asString)
          fourthResponse <- server.handleRequest(
            TestRequest.get.withSegments(Segment("first"), Segment("second"), Segment("third"))
          )
          fourthTestResponse <- TestResponse.fromResponseZIO(fourthResponse, _.asString)
        } yield assertTrue(
          firstTestResponse.headerFromName("ending")  == Some(ending("one")),
          secondTestResponse.headerFromName("ending") == Some(ending("zero")),
          thirdTestResponse.headerFromName("ending")  == Some(ending("two")),
          fourthTestResponse.headerFromName("ending") == Some(ending("zero"))
        )
      },
      test("Mapping the response") {
        val baseNode       = Node.ok
        val responseMapped = baseNode.mapResponse(_.withStatus(Status.BadRequest))

        for {
          response     <- responseMapped.asServer.handleRequest(TestRequest.get)
          testResponse <- TestResponse.fromResponseZIO(response, _.asString)
        } yield assertTrue(testResponse.assertCode[400])
      },
      test("Providing Int for AdditionalInfo") {
        val baseNode               = Node.fromValue("hello")
        val nodeWithAdditionalInfo = baseNode.provide[Int *: EmptyTuple]

        for {
          nodeOut <- nodeWithAdditionalInfo
            .outIfIndex[0](CollectedInfo.empty + 3 + "other string")
            .someOrFailException
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
          Node.fromValue(s"It was ${methodValue.value}!").withProofOf[Method].outlet[0].attach(Node.okString)

        val tree = Node.crudNode
          .outlet[0]
          .attach(itWasMethod[HttpMethod.POST])
          .outlet[0]
          .attach(itWasMethod[HttpMethod.GET])
          .outlet[0]
          .attach(itWasMethod[HttpMethod.PATCH])
          .outlet[0]
          .attach(itWasMethod[HttpMethod.DELETE])

        val server = tree.asServer

        for {
          postMessage <- server
            .handleRequest(TestRequest.fromMethod(HttpMethod.POST))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          getMessage <- server
            .handleRequest(TestRequest.fromMethod(HttpMethod.GET))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          patchMessage <- server
            .handleRequest(TestRequest.fromMethod(HttpMethod.PATCH))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          deleteMessage <- server
            .handleRequest(TestRequest.fromMethod(HttpMethod.DELETE))
            .flatMap(_.body.runCollect)
            .map(_.flatten.asString)
          optionsMessage <- server
            .handleRequest(TestRequest.fromMethod(HttpMethod.OPTIONS))
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
          .outlet[0]
          .attach(baseNode)

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
          .outlet[0]
          .attach(createOrPatchNodeSideEffect)
          .outlet[0]
          .attach(Node.ok)
          .withProofOf[HttpMethod.POST]

        val getNode = Node
          .failingEitherNode((id: Long) => ZIO.succeed(data.get(id).toRight(Response.NotFound)))
          .outlet[0]
          .attach(Node.okString)
          .withProofOf[HttpMethod.GET]

        val patchNode = Node
          .failingEitherNode((id: Long) => ZIO.succeed(data.get(id).toRight(Response.NotFound)))
          .provide[Request.RawRequest *: EmptyTuple]
          .outlet[0]
          .attach(Node.decodeBodyAsString)
          .outlet[0]
          .attach(createOrPatchNodeSideEffect)
          .outlet[0]
          .attach(Node.ok)
          .withProofOf[HttpMethod.PATCH]

        val deleteNodeSideEffect = Node.sideEffectNode((in: CollectedInfo[Long *: EmptyTuple]) =>
          ZIO.succeed {
            val id = in.access[Long]
            data -= id
          }
        )

        val deleteNode = deleteNodeSideEffect.outlet[0].attach(Node.ok).withProofOf[HttpMethod.DELETE]

        val crudWithPath =
          Node.navigation
            .path(path)
            .outlet[0]
            .attach(Node.notFound)
            .outlet[0]
            .attach(Node.crudNode)
            .outlet[0]
            .attach(createNode)
            .outlet[0]
            .attach(getNode)
            .outlet[0]
            .attach(patchNode)
            .outlet[0]
            .attach(deleteNode)

        val server = crudWithPath.asServer

        val baseRequest      = TestRequest.get.withPath(path)(0)
        val wrongBaseRequest = TestRequest.get.withPath(path)(1)

        for {
          createRequest  <- ZIO.succeed(baseRequest.withMethod(HttpMethod.POST).withStreamBodyAsString("my-data"))
          createResponse <- server.handleRequest(createRequest)
          getRequest     <- ZIO.succeed(baseRequest)
          getResponse    <- server.handleRequest(getRequest).flatMap(_.body.runCollect).map(_.flatten.asString)
          patchRequest  <- ZIO.succeed(baseRequest.withMethod(HttpMethod.PATCH).withStreamBodyAsString("my-other-data"))
          patchResponse <- server.handleRequest(patchRequest)
          wrongPatchRequest  <- ZIO.succeed(wrongBaseRequest.withMethod(HttpMethod.PATCH).withStreamBodyAsString("meh"))
          wrongPatchResponse <- server.handleRequest(wrongPatchRequest)
          getResponse2       <- server.handleRequest(getRequest).flatMap(_.body.runCollect).map(_.flatten.asString)
          deleteRequest      <- ZIO.succeed(baseRequest.withMethod(HttpMethod.DELETE))
          deleteResponse     <- server.handleRequest(deleteRequest)
          getResponse3       <- server.handleRequest(getRequest)
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
