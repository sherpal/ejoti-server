package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class MappingZIOOrExitNode[R, T, U, Exit <: ExitType](f: T => ZIO[R, Exit, U])
    extends Node[R, U *: EmptyTuple, Exit, T *: EmptyTuple] {
  def out(in: CollectedInfo[T *: EmptyTuple]): ZIO[R, Nothing, Value[U, 0] | Exit] =
    f(in.access[T]).map(Value(_, 0): Value[U, 0] | Exit).fold(identity, identity)
}
