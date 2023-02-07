package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class FailingEitherNode[T, In, Exit <: ExitType](f: In => ZIO[Any, Nothing, Either[Exit, T]])
    extends Node[Any, T *: EmptyTuple, Exit, In *: EmptyTuple] {
  def out(collectedInfo: CollectedInfo[In *: EmptyTuple]): ZIO[Any, Nothing, Value[T, 0] | Exit] =
    f(collectedInfo.access[In]).map {
      case Left(value)  => value
      case Right(value) => Value[T, 0](value, 0)
    }
}
