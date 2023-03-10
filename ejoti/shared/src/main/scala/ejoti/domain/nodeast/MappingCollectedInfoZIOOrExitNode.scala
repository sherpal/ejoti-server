package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class MappingCollectedInfoZIOOrExitNode[R, In <: Tuple, Exit <: ExitType, U](
    f: CollectedInfo[In] => ZIO[R, Exit, U]
) extends Node[R, U *: EmptyTuple, Exit, In] {
  def out(in: CollectedInfo[In]): ZIO[R, Nothing, Value[U, 0] | Exit] =
    f(in).map(Value(_, 0): Value[U, 0] | Exit).fold(identity, identity)
}
