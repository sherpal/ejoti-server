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

trait Node[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple] {
  self =>

  def numberOfOutlets(using value: ValueOf[Tuple.Size[X]]): Int = value.value

  type Outlet[Idx <: Int] = ElemOrNothing[X, Idx]

  import Node.given

  def asServer(using
      ev: (Choices[X] | Exit) =:= ExitType,
      inIsRawRequest: Conversion[CollectedInfo[Singleton[RawRequest]], CollectedInfo[IncomingInfo]]
  ): Server.RawServer[R] =
    Server.fromFunctionZIO[R, RawRequest, Response](request => out(CollectedInfo.empty + request).map(ev))

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
    ]] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] =
      new Node[R0, Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
        ElemOrNothing[X, Idx]
      ]] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] {
        type Out =
          Choices[Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
            ElemOrNothing[X, Idx]
          ]] ::: Tuple.Drop[X, Idx + 1]]

        def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R0, Nothing, Out | Exit | Exit1] =
          self.out(collectedInfo).flatMap {
            case o: Exit => ZIO.succeed(o)
            case other: Choices[X] @unchecked =>
              other.idx match {
                case i if i == idx =>
                  val otherTyped = other.value.asInstanceOf[ElemOrNothing[X, Idx]]
                  that.out(collectedInfo.flattenedConcat(otherTyped)).map {
                    case o: Exit1 => o
                    case other: Choices[Y] @unchecked =>
                      val mappedOther =
                        CollectedInfo.liftToCollectedInfo(otherTyped).toPolymorphicFunction[Y].mapChoice(other)
                      Value(
                        mappedOther.value,
                        mappedOther.idx + idx
                      )
                        .asInstanceOf[Out]
                  }
                case i if i < idx =>
                  ZIO.succeed(Value(other.value, other.idx).asInstanceOf[Out])
                case _ =>
                  ZIO
                    .succeed(Value(other.value, other.idx + summon[ValueOf[Tuple.Size[Y]]].value - 1).asInstanceOf[Out])
              }
          }
      }
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
    Node.NodeWithProof(this)

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
  ] = Node.ProvidedNode(this)

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

  type ExitType = Response

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

  final class NodeFromValue[Info](val info: Info) extends Node[Any, Singleton[Info], Nothing, EmptyTuple] {
    def out(collectedInfo: CollectedInfo.Empty) = ZIO.succeed(Value[Info, 0](info, 0))
  }

  def fromValue[Info](info: Info): Node[Any, Singleton[Info], Nothing, EmptyTuple] = new NodeFromValue(info)

  final class NodeWithProof[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple, Proof](
      val node: Node[R, X, Exit, IncomingInfo]
  )(using
      ValueOf[CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]],
      CollectedInfo.Elems[
        Proof *: IncomingInfo,
        CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]
      ] =:= IncomingInfo
  ) extends Node[R, X, Exit, Proof *: IncomingInfo] {
    def out(collectedInfo: CollectedInfo[Proof *: IncomingInfo]) = node.out(collectedInfo)
  }

  final class ProvidedNode[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple, AdditionalInfo <: Tuple](
      val node: Node[R, X, Exit, IncomingInfo]
  )(using
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
  ) extends Node[
        R,
        CollectedInfo.MappedCollectedInfo[X, AdditionalInfo],
        Exit,
        AdditionalInfo ::: IncomingInfo
      ] {
    def out(
        collectedInfo: CollectedInfo[AdditionalInfo ::: IncomingInfo]
    ): ZIO[R, Nothing, Choices[CollectedInfo.MappedCollectedInfo[X, AdditionalInfo]] | Exit] =
      node.out(collectedInfo).map {
        case exit: Exit => exit
        case choice: Choices[X] @unchecked =>
          collectedInfo.subset[AdditionalInfo].toPolymorphicFunction[X].mapChoice(choice)
      }
  }

  final class PolymorphicMappedNode[-R, X1 <: Tuple, X2 <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple](
      val node: Node[R, X1, Exit, IncomingInfo],
      val f: PolymorphicFunction[X1, X2]
  )(using Typeable[Exit])
      extends Node[R, X2, Exit, IncomingInfo] {
    override def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R, Nothing, Choices[X2] | Exit] =
      node.out(collectedInfo).map {
        case exit: Exit                    => exit
        case value: Choices[X1] @unchecked => f.mapChoice(value)
      }
  }

  type Example1 = String *: Double *: EmptyTuple
  summon[Choices[Example1] =:= (Value[String, 0] | Value[Double, 1])]
  summon[Choices[EmptyTuple] =:= Nothing]

  final class SideEffectNode[-R, In <: Tuple](effect: CollectedInfo[In] => ZIO[R, Nothing, Unit])
      extends Node[R, Singleton[Unit], Nothing, In] {
    def out(in: CollectedInfo[In]): ZIO[R, Nothing, Value[Unit, 0]] = effect(in).map(Value(_, 0))
  }

  def sideEffectNode[R, In <: Tuple](
      effect: CollectedInfo[In] => ZIO[R, Nothing, Unit]
  ): Node[R, Singleton[Unit], Nothing, In] = new SideEffectNode(effect)

  final class EitherNode[Left, Right, In](f: (In, RawRequest) => Either[Left, Right])(using
      ValueOf[CollectedInfo.IndexOf[RawRequest, In *: RawRequest *: EmptyTuple]]
  ) extends Node[Any, Left *: Right *: EmptyTuple, Nothing, In *: RawRequest *: EmptyTuple] {
    def out(
        collectedInfo: CollectedInfo[In *: RawRequest *: EmptyTuple]
    ): ZIO[Any, Nothing, Value[Left, 0] | Value[Right, 1]] =
      ZIO.succeed(f(collectedInfo.access[In], collectedInfo.access[RawRequest]) match {
        case Left(left)   => Value[Left, 0](left, 0)
        case Right(right) => Value[Right, 1](right, 1)
      })
  }

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

  final class FailingEitherNode[T, In](f: In => ZIO[Any, Nothing, Either[ExitType, T]])
      extends Node[Any, T *: EmptyTuple, ExitType, In *: EmptyTuple] {
    def out(collectedInfo: CollectedInfo[In *: EmptyTuple]): ZIO[Any, Nothing, Value[T, 0] | ExitType] =
      f(collectedInfo.access[In]).map {
        case Left(value)  => value
        case Right(value) => Value[T, 0](value, 0)
      }
  }

  def failingEitherNode[T, In](
      f: In => ZIO[Any, Nothing, Either[ExitType, T]]
  ): Node[Any, T *: EmptyTuple, ExitType, In *: EmptyTuple] = new FailingEitherNode(f)

  def leaf[R, In](f: In => ZIO[R, Nothing, ExitType]): Node[R, EmptyTuple, ExitType, Singleton[In]] =
    (in: CollectedInfo[Singleton[In]]) => f(in.access[In])

  final class MappingNode[T, U](f: T => U) extends Node[Any, U *: EmptyTuple, Nothing, T *: EmptyTuple] {
    def out(in: CollectedInfo[T *: EmptyTuple]): ZIO[Any, Nothing, Value[U, 0]] =
      ZIO.succeed(Value(f(in.access[T]), 0))
  }

  def mappingNode[T, U](f: T => U): Node[Any, U *: EmptyTuple, Nothing, T *: EmptyTuple] =
    MappingNode(f)

  final class IdentityNode[Incoming <: Tuple]
      extends Node[Any, CollectedInfo[Incoming] *: EmptyTuple, Nothing, Incoming] {
    def out(in: CollectedInfo[Incoming]) = ZIO.succeed(Value[CollectedInfo[Incoming], 0](in, 0))
  }

  def identityNode[Incoming <: Tuple]: Node[Any, CollectedInfo[Incoming] *: EmptyTuple, Nothing, Incoming] =
    new IdentityNode[Incoming]

  def singletonIdentityNode[In]: Node[Any, CollectedInfo[Singleton[In]] *: EmptyTuple, Nothing, Singleton[In]] =
    identityNode[Singleton[In]]

  def addJsonNode[T](using Decoder[T]): Node[Any, Singleton[T], ExitType, Singleton[RawRequest]] =
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

  private def leafFromResponse(response: => Response): Node[Any, EmptyTuple, ExitType, Unit *: EmptyTuple] =
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

  object CrudNode extends Node[Any, HttpMethod.CRUD, ExitType, Singleton[RawRequest]] {
    def out(
        collectedInfo: CollectedInfo[Singleton[RawRequest]]
    ): ZIO[Any, Nothing, Choices[HttpMethod.CRUD] | ExitType] = ZIO.succeed {
      collectedInfo.access[RawRequest].method match {
        case POST   => Value[POST, 0](POST, 0)
        case GET    => Value[GET, 1](GET, 1)
        case PATCH  => Value[PATCH, 2](PATCH, 2)
        case DELETE => Value[DELETE, 3](DELETE, 3)
        case _      => Response.MethodNotAllowed
      }
    }
  }

  def crudNode: Node[Any, HttpMethod.CRUD, ExitType, Singleton[RawRequest]] = CrudNode

}
