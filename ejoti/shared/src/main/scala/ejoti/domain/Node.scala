package ejoti.domain

import ejoti.domain.Node.ExitType
import zio.stream.ZStream

import scala.compiletime.ops.int.*
import scala.compiletime.erasedValue
import scala.reflect.{TypeTest, Typeable}
import Node.*
import ejoti.domain.Request.RawRequest
import io.circe.Decoder
import io.circe.parser.decode
import zio.ZIO
import urldsl.language.PathSegment
import urldsl.vocabulary.PathMatchOutput
import urldsl.errors.DummyError
import urldsl.url.UrlStringParserGenerator
import CollectedInfo.given
import ejoti.domain.HttpMethod.*
import urldsl.vocabulary.Segment
import java.net.http.HttpResponse.ResponseInfo
import ejoti.domain.nodeast.*
import fs2.io.file.Path

trait Node[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple] {
  self =>

  def numberOfOutlets(using value: ValueOf[Tuple.Size[X]]): Int = value.value

  type Outlet[Idx <: Int] = ElemOrNothing[X, Idx]

  import Node.given

  def asServer(using
      ev: (Choices[X] | Exit) =:= Response,
      inIsRawRequest: Conversion[CollectedInfo[Singleton[RawRequest]], CollectedInfo[IncomingInfo]]
  ): Server.RawServer[R] =
    Server.fromFunctionZIO[R, RawRequest, Response](request => out(CollectedInfo.empty + request).map(ev))

  def asWebSocketServer(using
      ev: (Choices[X] | Exit) <:< WebSocketResponse,
      inIsRawRequest: Conversion[CollectedInfo[Singleton[RawRequest]], CollectedInfo[IncomingInfo]]
  ): WebSocketServer[R] = request => out(CollectedInfo.empty + request).map(ev)

  def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R, Nothing, Choices[X] | Exit]

  def outIfIndex[Idx <: Int](collectedInfo: CollectedInfo[IncomingInfo])(using
      idxValue: ValueOf[Idx],
      t: Typeable[Exit]
  ): ZIO[R, Nothing, Option[ElemOrNothing[X, Idx] | Exit]] =
    out(collectedInfo)
      .map {
        case exit: Exit => Some(exit)
        case value: Choices[X] @unchecked =>
          value match {
            case typedValue: Value[ElemOrNothing[X, Idx], Idx] @unchecked =>
              Option.when(typedValue.idx == idxValue.value)(typedValue.value)
          }

      }

  class OutletFiller[Idx <: Int](using v: ValueOf[Idx]) {
    private val idx = summon[ValueOf[Idx]].value
    def apply[R0 <: R, Y <: Tuple, Exit1 <: ExitType](
        that: Node[R0, Y, Exit1, CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]]]
    )(using
        Typeable[Exit],
        Typeable[Exit1],
        ValueOf[Tuple.Size[Y]]
    ): Node[R0, Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
      ElemOrNothing[X, Idx]
    ]] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] = new OutletFilledNode(self, that)

    /** !DO NOT USE!
      *
      * This function allows you to use metals (or IJ when it will be ready) to discover in a practical way the missing
      * dependencies that `this` node has to be able to be filled by `that`.
      *
      * If you are confused by the error messages, you can put somewhere:
      * {{{
      *   val x = myNode.fillOutlet[SomeIdx].missingDependencies(myOtherNode)
      * }}}
      *
      * then put your mouse on x and you will see all missing dependencies, that must either be provided in the
      * IncomingInfo of myNode (you can use the provide method for that). Seeing EmptyTuple means that myOtherNode can
      * be used to fill the outlet SomeIdx of myNode.
      *
      * @param that
      * @return
      */
    def missingDependencies[R0 <: R, Y <: Tuple, Exit1 <: ExitType, IncomingInfo1 <: Tuple](
        that: Node[R0, Y, Exit1, IncomingInfo1]
    ): CollectedInfo.ElemsNotIn[
      IncomingInfo1,
      CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]]
    ] = ???
  }

  final def fillOutlet[Idx <: Int](using ValueOf[Idx]): OutletFiller[Idx] = new OutletFiller[Idx]()

  final def fillFirstOutlet: OutletFiller[0] = fillOutlet[0]

  final def withProofOf[Proof](using
      ValueOf[CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]],
      CollectedInfo.Elems[
        Proof *: IncomingInfo,
        CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]
      ] =:= IncomingInfo
  ): Node[R, X, Exit, Proof *: IncomingInfo] =
    NodeWithProof(this)

  final def provide[AdditionalInfo <: Tuple](using
      ValueOf[CollectedInfo.IndexesOf[IncomingInfo, AdditionalInfo ::: IncomingInfo]],
      ValueOf[CollectedInfo.IndexesOf[AdditionalInfo, AdditionalInfo ::: IncomingInfo]],
      CollectedInfo.Elems[
        AdditionalInfo ::: IncomingInfo,
        CollectedInfo.IndexesOf[IncomingInfo, AdditionalInfo ::: IncomingInfo]
      ] =:= IncomingInfo,
      CollectedInfo.Elems[
        AdditionalInfo ::: IncomingInfo,
        CollectedInfo.IndexesOf[AdditionalInfo, AdditionalInfo ::: IncomingInfo]
      ] =:= AdditionalInfo,
      ValueOf[Tuple.Size[X]],
      Typeable[Exit]
  ): Node[
    R,
    CollectedInfo.MappedCollectedInfo[X, AdditionalInfo],
    Exit,
    AdditionalInfo ::: IncomingInfo
  ] = ProvidedNode(this)

}

object Node {

  given askLess[R, X <: Tuple, Exit <: ExitType, Incoming1 <: Tuple, Incoming2 <: Tuple](using
      Conversion[CollectedInfo[Incoming2], CollectedInfo[Incoming1]]
  ): Conversion[Node[R, X, Exit, Incoming1], Node[R, X, Exit, Incoming2]] =
    (node: Node[R, X, Exit, Incoming1]) => (collectedInfo: CollectedInfo[Incoming2]) => node.out(collectedInfo)

  given outputMapping[R, X1 <: Tuple, X2 <: Tuple, Exit <: ExitType, Incoming <: Tuple](using
      PolymorphicFunction[X1, X2],
      Typeable[Exit]
  ): Conversion[Node[R, X1, Exit, Incoming], Node[R, X2, Exit, Incoming]] =
    new PolymorphicMappedNode(_, summon[PolymorphicFunction[X1, X2]])

  type :::[X <: Tuple, Y <: Tuple] = Tuple.Concat[X, Y]
  type Singleton[X]                = X *: EmptyTuple
  type Pair[X, Y]                  = X *: Y *: EmptyTuple

  type ExitType = Response | WebSocketResponse

  given Typeable[Nothing] = (_: Any) => None

  trait WithIdx {
    type Index <: Int
    type Type
    def idx: Index

    def value: Type
  }
  case class Value[T, Idx <: Int](value: T, idx: Idx) extends WithIdx {
    type Index = Idx
    type Type  = T
  }

  type ChoicesBuilder[X <: Tuple, Start <: Int] <: WithIdx = X match {
    case EmptyTuple => Nothing
    case x *: xs    => Value[x, Start] | ChoicesBuilder[xs, Start + 1]
  }

  type Choices[X <: Tuple] = ChoicesBuilder[X, 0]

  type ElemOrNothing[X <: Tuple, Idx <: Int] = X match {
    case EmptyTuple => Nothing
    case x *: xs    => Tuple.Elem[x *: xs, Idx]
  }

  def fromValue[Info](info: Info): Node[Any, Singleton[Info], Nothing, EmptyTuple] = new NodeFromValue(info)

  type Example1 = String *: Double *: EmptyTuple
  summon[Choices[Example1] =:= (Value[String, 0] | Value[Double, 1])]
  summon[Choices[EmptyTuple] =:= Nothing]

  def sideEffectNode[R, In <: Tuple](
      effect: CollectedInfo[In] => ZIO[R, Nothing, Unit]
  ): Node[R, Singleton[Unit], Nothing, In] = new SideEffectNode(effect)

  def eitherNode[Left, Right, In](
      f: In => Either[Left, Right]
  )(using
      ValueOf[CollectedInfo.IndexOf[RawRequest, In *: RawRequest *: EmptyTuple]]
  ): Node[Any, Left *: Right *: EmptyTuple, Nothing, In *: RawRequest *: EmptyTuple] = EitherNode((in: In, _) => f(in))

  def eitherNodeWithRequest[Left, Right, In](
      f: (In, RawRequest) => Either[Left, Right]
  )(using
      ValueOf[CollectedInfo.IndexOf[RawRequest, In *: RawRequest *: EmptyTuple]]
  ): Node[Any, Left *: Right *: EmptyTuple, Nothing, In *: RawRequest *: EmptyTuple] = EitherNode(f)

  def failingEitherNode[T, In, Exit <: ExitType](
      f: In => ZIO[Any, Nothing, Either[Exit, T]]
  ): Node[Any, T *: EmptyTuple, Exit, In *: EmptyTuple] = new FailingEitherNode(f)

  def leaf[R, In, Exit <: ExitType](f: In => ZIO[R, Nothing, Exit]): Node[R, EmptyTuple, Exit, Singleton[In]] =
    (in: CollectedInfo[Singleton[In]]) => f(in.access[In])

  def mappingNode[T, U](f: T => U): Node[Any, U *: EmptyTuple, Nothing, T *: EmptyTuple] =
    MappingNode(f)

  def identityNode[Incoming <: Tuple]: Node[Any, CollectedInfo[Incoming] *: EmptyTuple, Nothing, Incoming] =
    new IdentityNode[Incoming]

  def singletonIdentityNode[In]: Node[Any, CollectedInfo[Singleton[In]] *: EmptyTuple, Nothing, Singleton[In]] =
    identityNode[Singleton[In]]

  def addJsonNode[T](using Decoder[T]): Node[Any, Singleton[T], Response, Singleton[RawRequest]] =
    failingEitherNode { (request: RawRequest) =>
      request.bodyAsString
        .map(decode[T](_))
        .absolve
        .mapError(error =>
          Response.fromBodyString(
            Status.BadRequest,
            Nil,
            Option {
              import scala.language.unsafeNulls
              error.getMessage
            }.getOrElse(s"Unknown message.")
          )
        )
        .either

    }

  def printInfo[PieceOfInfo]: Node[Any, Singleton[CollectedInfo[EmptyTuple]], Nothing, Singleton[PieceOfInfo]] =
    (in: CollectedInfo[Singleton[PieceOfInfo]]) =>
      ZIO.succeed(println("printing the info")) *>
        ZIO.succeed(println(in.access[PieceOfInfo])) *> ZIO.succeed(Value(CollectedInfo.empty, 0))

  val printRequest: Node[Any, Singleton[CollectedInfo[EmptyTuple]], Nothing, Singleton[RawRequest]] =
    printInfo[RawRequest]

  val decodeBodyAsString: Node[Any, Singleton[Request[String]], Nothing, Singleton[RawRequest]] =
    (in: CollectedInfo[Singleton[RawRequest]]) => in.access[RawRequest].withBodyAsString.map(Value(_, 0))

  private def leafFromResponse(response: => Response): Node[Any, EmptyTuple, Response, Unit *: EmptyTuple] =
    leaf(_ => ZIO.succeed(response))

  val ok               = leafFromResponse(Response.Ok)
  val notFound         = leafFromResponse(Response.NotFound)
  val methodNotAllowed = leafFromResponse(Response.MethodNotAllowed)

  val okString = leaf((message: String) => ZIO.succeed(Response.fromBodyString(Status.Ok, Nil, message)))

  object navigation {
    def pathPrefix[T](
        segment: PathSegment[T, DummyError]
    ): Node[Any, Tuple2[CollectedInfo.Empty, T *: List[Segment] *: EmptyTuple], Nothing, Singleton[RawRequest]] =
      eitherNode((req: RawRequest) =>
        segment
          .matchSegments(req.segments)
          .map(output => output.output *: output.unusedSegments *: EmptyTuple)
          .left
          .map(_ => CollectedInfo.empty)
      )

    def path[T](
        segment: PathSegment[T, DummyError]
    ): Node[Any, Tuple2[CollectedInfo.Empty, T], Nothing, Singleton[RawRequest]] =
      eitherNode((req: RawRequest) =>
        segment.matchSegments(req.segments).map(output => output.output).left.map(_ => CollectedInfo.empty)
      )
  }

  def crudNode: Node[Any, HttpMethod.CRUD, Response, Singleton[RawRequest]] = CrudNode

  def sendFile(chunkSize: Int = 8 * 1024): FileDownloadNode = new FileDownloadNode(chunkSize)

  def sendFixedFile(path: Path, chunkSize: Int = 8 * 1024): Node[Any, EmptyTuple, Response, EmptyTuple] =
    fromValue(path).fillOutlet[0](sendFile(chunkSize))

}
