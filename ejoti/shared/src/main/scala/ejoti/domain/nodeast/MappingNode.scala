package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class MappingNode[T, U](f: T => U) extends Node[Any, U *: EmptyTuple, Nothing, T *: EmptyTuple] {
  def out(in: CollectedInfo[T *: EmptyTuple]): ZIO[Any, Nothing, Value[U, 0]] =
    ZIO.succeed(Value(f(in.access[T]), 0))
}
