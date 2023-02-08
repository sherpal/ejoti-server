package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO
import scala.reflect.Typeable
import scala.compiletime.ops.int.+
import scala.Tuple.Concat
import scala.Tuple.Drop
import ejoti.domain.CollectedInfo.LiftedToCollectedInfo
import ejoti.domain.CollectedInfo.MappedCollectedInfo
import scala.Tuple.Take

final class OutletFilledNode[
    R,
    X <: Tuple,
    Y <: Tuple,
    Idx <: Int,
    IncomingInfo <: Tuple,
    Exit <: ExitType,
    Exit1 <: ExitType
](
    left: Node[R, X, Exit, IncomingInfo],
    right: Node[R, Y, Exit1, CollectedInfo.FlattenedConcat[IncomingInfo, ElemOrNothing[X, Idx]]]
)(using
    Typeable[Exit],
    Typeable[Exit1],
    ValueOf[Tuple.Size[Y]],
    ValueOf[Idx]
) extends Node[R, Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
      ElemOrNothing[X, Idx]
    ]] ::: Tuple.Drop[X, Idx + 1], Exit | Exit1, IncomingInfo] {
  val idx = summon[ValueOf[Idx]].value
  type Out =
    Choices[Tuple.Take[X, Idx] ::: CollectedInfo.MappedCollectedInfo[Y, CollectedInfo.LiftedToCollectedInfo[
      ElemOrNothing[X, Idx]
    ]] ::: Tuple.Drop[X, Idx + 1]]

  def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R, Nothing, Out | Exit | Exit1] =
    left.out(collectedInfo).flatMap {
      case o: Exit => ZIO.succeed(o)
      case other: Choices[X] @unchecked =>
        other.idx match {
          case i if i == idx =>
            val otherTyped = other.value.asInstanceOf[ElemOrNothing[X, Idx]]
            right.out(collectedInfo.flattenedConcat(otherTyped)).map {
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
