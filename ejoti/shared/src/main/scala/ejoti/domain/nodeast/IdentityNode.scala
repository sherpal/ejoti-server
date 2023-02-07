package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class IdentityNode[Incoming <: Tuple]
    extends Node[Any, CollectedInfo[Incoming] *: EmptyTuple, Nothing, Incoming] {
  def out(in: CollectedInfo[Incoming]) = ZIO.succeed(Value[CollectedInfo[Incoming], 0](in, 0))
}
