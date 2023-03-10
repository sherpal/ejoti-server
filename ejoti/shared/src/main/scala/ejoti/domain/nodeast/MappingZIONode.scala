package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class MappingZIONode[R, T, U](f: T => ZIO[R, Nothing, U])
    extends Node[R, U *: EmptyTuple, Nothing, T *: EmptyTuple] {
  def out(in: CollectedInfo[T *: EmptyTuple]): ZIO[R, Nothing, Value[U, 0]] = f(in.access[T]).map(Value(_, 0))
}
