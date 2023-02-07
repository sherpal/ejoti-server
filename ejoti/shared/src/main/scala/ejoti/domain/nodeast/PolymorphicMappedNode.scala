package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

import scala.reflect.Typeable

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
