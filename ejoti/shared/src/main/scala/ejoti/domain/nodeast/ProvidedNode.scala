package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

import scala.reflect.Typeable

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
