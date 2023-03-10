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
import urldsl.language.{PathSegment, QueryParameters}
import urldsl.vocabulary.PathMatchOutput
import urldsl.errors.DummyError
import urldsl.url.UrlStringParserGenerator
import CollectedInfo.given
import ejoti.domain.HttpMethod.*
import urldsl.vocabulary.Segment
import java.net.http.HttpResponse.ResponseInfo
import ejoti.domain.nodeast.*
import fs2.io.file.Path
import scala.deriving.Mirror
import scala.Tuple.Concat

trait Node[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple] {
  self =>

  type NumberOfOutlets = Tuple.Size[X]
  def numberOfOutlets(using value: ValueOf[NumberOfOutlets]): Int = value.value

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

  def mapResponse(f: Response => Response)(using ev: Exit =:= Response)(using
      Typeable[Exit]
  ): Node[R, X, Response, IncomingInfo] =
    new ResponseMappedNode(this, f compose ev)

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

  def squashFirstAndSecondOutlets(using Typeable[Exit]): Node[R, Unit *: Tuple.Drop[X, 2], Exit, IncomingInfo] =
    new Node[R, Unit *: Tuple.Drop[X, 2], Exit, IncomingInfo] {
      def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R, Nothing, Choices[Unit *: Tuple.Drop[X, 2]] | Exit] =
        self.out(collectedInfo).map {
          case exit: Exit => exit
          case other: Choices[X] @unchecked if other.idx <= 1 =>
            Value((), 0).asInstanceOf[Choices[Unit *: Tuple.Drop[X, 2]]]
          case other: Choices[X] @unchecked =>
            Value(other.value, other.idx - 1).asInstanceOf[Choices[Unit *: Tuple.Drop[X, 2]]]
        }
    }

  class OutletFiller[Idx <: Int](using v: ValueOf[Idx]) {
    private val idx = summon[ValueOf[Idx]].value

    def map[U](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => U
    )(using Typeable[Exit]) = mapZIO(f andThen ZIO.succeed)

    def mapZIO[R0 <: R, U](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => ZIO[R0, Nothing, U]
    )(using Typeable[Exit]) = attach(MappingCollectedInfoZIONode(f))

    def mapZIOOrExit[R0 <: R, Exit1 <: ExitType, U](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => ZIO[R0, Exit1, U]
    )(using Typeable[Exit], Typeable[Exit1]) = attach(MappingCollectedInfoZIOOrExitNode(f))

    def tapZIO[R0 <: R](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => ZIO[R0, Nothing, Unit]
    )(using Typeable[Exit]) = mapZIO(f)

    def exit[Exit1 <: ExitType](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => Exit1
    )(using Typeable[Exit], Typeable[Exit1]) = exitZIO(f andThen ZIO.succeed)

    def exitWith[Exit1 <: ExitType](e: Exit1)(using Typeable[Exit], Typeable[Exit1]) = exit(_ => e)

    def exitZIO[R0 <: R, Exit1 <: ExitType](
        f: CollectedInfo[
          CollectedInfo.FlattenedConcat[IncomingInfo, Node.ElemOrNothing[X, Idx]]
        ] => ZIO[R0, Nothing, Exit1]
    )(using Typeable[Exit], Typeable[Exit1]) = close(ExitZIONode(f))

    def attach[R0 <: R, Y <: Tuple, Exit1 <: ExitType](
        that: Node[R0, Y, Exit1, CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]]]
    )(using
        Typeable[Exit],
        Typeable[Exit1],
        ValueOf[Tuple.Size[Y]]
    ): Node[R0, Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
      ElemOrNothing[X, Idx]
    ]] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] = new OutletFilledNode(self, that)

    /** Sames as attach, but forces the attached [[Node]] to close the outlet by not producing any. */
    def close[R0 <: R, Exit1 <: ExitType](
        that: Node[R0, EmptyTuple, Exit1, CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]]]
    )(using
        Typeable[Exit],
        Typeable[Exit1]
    ): Node[R0, Tuple.Take[X, Idx] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] =
      new OutletFilledNode(self, that)

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

    def inDependencies: CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]] = ???

    def atIdx: ElemOrNothing[X, Idx] = ???
  }

  final def outlet[Idx <: Int](using ValueOf[Idx]): OutletFiller[Idx] = new OutletFiller[Idx]()

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
  def fromZIOValue[R, Info](info: ZIO[R, Nothing, Info]): Node[R, Singleton[Info], Nothing, EmptyTuple] =
    new NodeFromZIOValue(info)

  type Example1 = String *: Double *: EmptyTuple
  summon[Choices[Example1] =:= (Value[String, 0] | Value[Double, 1])]
  summon[Choices[EmptyTuple] =:= Nothing]

  def sideEffectNode[R, In <: Tuple](
      effect: CollectedInfo[In] => ZIO[R, Nothing, Unit]
  ): Node[R, Singleton[Unit], Nothing, In] = new SideEffectNode(effect)

  def eitherNode[Left, Right, In <: Tuple](
      f: CollectedInfo[In] => Either[Left, Right]
  ): Node[Any, Left *: Right *: EmptyTuple, Nothing, In] = EitherNode(f)

  def absolveNode[E, A] = eitherNode((in: CollectedInfo[Node.Singleton[Either[E, A]]]) => in.access[Either[E, A]])

  def failingEitherNode[T, In <: Tuple, Exit <: ExitType](
      f: CollectedInfo[In] => ZIO[Any, Nothing, Either[Exit, T]]
  ): Node[Any, T *: EmptyTuple, Exit, In] = new FailingEitherNode(f)

  def leaf[R, In, Exit <: ExitType](f: In => ZIO[R, Nothing, Exit]): Node[R, EmptyTuple, Exit, Singleton[In]] =
    (in: CollectedInfo[Singleton[In]]) => f(in.access[In])

  def leafFromZIOValue[R, Exit <: ExitType](effect: ZIO[R, Nothing, Exit]): Node[R, EmptyTuple, Exit, EmptyTuple] =
    (in: CollectedInfo.Empty) => effect

  def mappingNode[T, U](f: T => U): Node[Any, U *: EmptyTuple, Nothing, T *: EmptyTuple] =
    MappingNode(f)

  def mappingZIONode[R, T, U](f: T => ZIO[R, Nothing, U]): Node[R, Singleton[U], Nothing, Singleton[T]] =
    MappingZIONode(f)

  def mappingZIOOrExitNode[R, T, U, Exit <: ExitType](
      f: T => ZIO[R, Exit, U]
  ): Node[R, Singleton[U], Exit, Singleton[T]] =
    MappingZIOOrExitNode(f)

  def identityNode[Incoming <: Tuple]: Node[Any, CollectedInfo[Incoming] *: EmptyTuple, Nothing, Incoming] =
    new IdentityNode[Incoming]

  def singletonIdentityNode[In]: Node[Any, CollectedInfo[Singleton[In]] *: EmptyTuple, Nothing, Singleton[In]] =
    identityNode[Singleton[In]]

  def addJsonNode[T](using Decoder[T]): Node[Any, Singleton[T], Response, Singleton[RawRequest]] =
    failingEitherNode { (request: CollectedInfo[Singleton[RawRequest]]) =>
      request
        .access[RawRequest]
        .bodyAsString
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

  val ok                            = leafFromResponse(Response.Ok)
  def temporaryRedirect(to: String) = leafFromResponse(Response.TemporaryRedirect(to))
  val notFound                      = leafFromResponse(Response.NotFound)
  val methodNotAllowed              = leafFromResponse(Response.MethodNotAllowed)

  val okString = leaf((message: String) => ZIO.succeed(Response.fromBodyString(Status.Ok, Nil, message)))

  object navigation {
    sealed trait UnusedSegments {
      def segments: List[Segment]
    }
    private case class UnusedSegmentsImpl(segments: List[Segment]) extends UnusedSegments
    private def unusedSegments(segments: List[Segment]): UnusedSegments = UnusedSegmentsImpl(segments)

    def initialSegments: Node[Any, Singleton[UnusedSegments], Nothing, Singleton[Request.RawRequest]] =
      mappingNode((request: Request.RawRequest) => unusedSegments(request.segments))

    def pathPrefix[T](
        segment: PathSegment[T, DummyError]
    ) =
      eitherNode((segments: CollectedInfo[Singleton[UnusedSegments]]) =>
        segment
          .matchSegments(segments.access[UnusedSegments].segments)
          .map(output => CollectedInfo.empty.add(unusedSegments(output.unusedSegments)).add(output.output))
          .left
          .map(_ => segments)
      )

    def path[T](
        segment: PathSegment[T, DummyError]
    ): Node[Any, Tuple2[CollectedInfo.Empty, T], Nothing, Singleton[RawRequest]] =
      eitherNode((req: CollectedInfo[Singleton[RawRequest]]) =>
        segment
          .matchSegments(req.access[RawRequest].segments)
          .map(output => output.output)
          .left
          .map(_ => CollectedInfo.empty)
      )

    def pathUsingUnusedSegments[T](segment: PathSegment[T, DummyError]) = eitherNode {
      (segmentsInfo: CollectedInfo[Singleton[UnusedSegments]]) =>
        segment
          .matchSegments(segmentsInfo.access[UnusedSegments].segments)
          .map(output => output.output)
          .left
          .map(_ => CollectedInfo.empty)
    }

    def queryParamNode[Q](params: QueryParameters[Q, DummyError]) =
      Node.eitherNode((req: CollectedInfo[Singleton[RawRequest]]) =>
        params.matchParams(req.access[RawRequest].params).left.map(_ => CollectedInfo.empty).map(_.output)
      )
  }

  object body {

    def formField[Q](params: QueryParameters[Q, DummyError]) = Node.mappingZIOOrExitNode((req: RawRequest) =>
      for {
        _ <- ZIO.unless(req.headers.contains[Header](Header.ContentType.`application/x-www-form-urlencoded`))(
          ZIO.fail(
            Response.BadRequest.withBodyString(
              s"Missing header ${Header.ContentType.`application/x-www-form-urlencoded`.prettyPrint}"
            )
          )
        )
        stringBody <- req.bodyAsString
        q          <- ZIO.from(params.matchQueryString(stringBody)).orElseFail(Response.BadRequest)
      } yield q
    )

  }

  object requestmetadata {
    def cookie[E, C](cookieName: String, makeCookieData: String => Either[E, C]) =
      Node.eitherNode((req: CollectedInfo[Singleton[RawRequest]]) =>
        req.access[RawRequest].cookies.cookies.get(cookieName).map(makeCookieData) match {
          case None           => Left(Option.empty[E])
          case Some(Right(c)) => Right(c)
          case Some(Left(e))  => Left(Some(e))
        }
      )
  }

  object methods {
    def apply(methods: HttpMethod*) = eitherNode((in: CollectedInfo[Singleton[RawRequest]]) =>
      Either.cond(
        methods.contains[HttpMethod](in.access[RawRequest].method),
        in.access[RawRequest].method,
        CollectedInfo.empty
      )
    )

    private def isMethod[Method](using Typeable[Method]) = eitherNode((in: CollectedInfo[Singleton[RawRequest]]) =>
      in.access[RawRequest].method match {
        case m: Method => Right(m)
        case _         => Left(CollectedInfo.empty)
      }
    )

    def get     = isMethod[GET]
    def post    = isMethod[POST]
    def put     = isMethod[PUT]
    def patch   = isMethod[PATCH]
    def delete  = isMethod[DELETE]
    def options = isMethod[OPTIONS]
  }

  def crudNode: Node[Any, HttpMethod.CRUD, Response, Singleton[RawRequest]] = CrudNode

  def sendFile(chunkSize: Int = FileDownloadNode.defaultChunkSize): FileDownloadNode = new FileDownloadNode(chunkSize)

  def sendFixedFile(
      path: Path,
      chunkSize: Int = FileDownloadNode.defaultChunkSize
  ): Node[Any, EmptyTuple, Response, EmptyTuple] =
    fromValue(path).outlet[0].attach(sendFile(chunkSize))

  /** Serves files in the specified static folder. The path of the file will be the remaining segments after the prefix
    * has been matched.
    *
    * @example
    *   If
    * {{{
    *   val staticFolder = Path("./some-folder")
    *   val prefix = root / "static-prefix-route"
    *   val node = serveStatic(staticFolder, prefix)
    * }}}
    * This will serve, for example
    *   - "static-prefix-route/index.html" => some-folder/index.html
    *   - "static-prefix-route/public/index.css" => some-folder/public/index.css
    *   - "other-prefix/index.html" => will not match and continue to open outlet.
    *
    * @param staticFolder
    *   Path of the folder (relative to where the server runs) to the folder where static are found
    * @param prefix
    *   matching prefix segment
    * @param chunkSize
    *   control the chunk size to read the file
    * @return
    */
  def serveStatic(
      staticFolder: Path,
      prefix: PathSegment[Unit, DummyError],
      chunkSize: Int = FileDownloadNode.defaultChunkSize
  ): Node[Any, Singleton[CollectedInfo.Empty], Response, Singleton[RawRequest]] =
    FileDownloadNode.serveStatic(staticFolder, prefix, chunkSize)

  /** Same as serveStatic, but closes the open outlet with NotFound
    */
  def serveStaticOrNotFound(
      staticFolder: Path,
      prefix: PathSegment[Unit, DummyError],
      chunkSize: Int = FileDownloadNode.defaultChunkSize
  ): Node[Any, EmptyTuple, Response, Singleton[RawRequest]] =
    serveStatic(staticFolder, prefix, chunkSize).outlet[0].attach(notFound)

  def sealedDispatch[T](using mirror: Mirror.SumOf[T]): Node[Any, mirror.MirroredElemTypes, Nothing, Singleton[T]] =
    FromMirror[T]

}
