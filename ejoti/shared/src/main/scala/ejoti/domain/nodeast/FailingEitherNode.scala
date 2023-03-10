package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class FailingEitherNode[T, In <: Tuple, Exit <: ExitType](
    f: CollectedInfo[In] => ZIO[Any, Nothing, Either[Exit, T]]
) extends Node[Any, T *: EmptyTuple, Exit, In] {
  def out(collectedInfo: CollectedInfo[In]): ZIO[Any, Nothing, Value[T, 0] | Exit] =
    f(collectedInfo).map {
      case Left(value)  => value
      case Right(value) => Value[T, 0](value, 0)
    }
}
