package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class ExitZIONode[R, In <: Tuple, Exit <: ExitType](
    f: CollectedInfo[In] => ZIO[R, Nothing, Exit]
) extends Node[R, EmptyTuple, Exit, In] {
  def out(in: CollectedInfo[In]): ZIO[R, Nothing, Exit] = f(in)
}
