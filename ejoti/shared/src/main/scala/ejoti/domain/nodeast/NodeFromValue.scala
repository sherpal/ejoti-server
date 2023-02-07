package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class NodeFromValue[Info](val info: Info) extends Node[Any, Singleton[Info], Nothing, EmptyTuple] {
  def out(collectedInfo: CollectedInfo.Empty) = ZIO.succeed(Value[Info, 0](info, 0))
}
