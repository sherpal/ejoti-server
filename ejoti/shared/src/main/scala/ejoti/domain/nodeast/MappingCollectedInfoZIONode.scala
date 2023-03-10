package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class MappingCollectedInfoZIONode[R, In <: Tuple, U](f: CollectedInfo[In] => ZIO[R, Nothing, U])
    extends Node[R, U *: EmptyTuple, Nothing, In] {
  def out(in: CollectedInfo[In]): ZIO[R, Nothing, Value[U, 0]] = f(in).map(Value(_, 0))
}
