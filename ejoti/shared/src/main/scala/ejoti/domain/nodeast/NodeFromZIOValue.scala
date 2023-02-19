package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class NodeFromZIOValue[R, Info](val info: ZIO[R, Nothing, Info])
    extends Node[R, Singleton[Info], Nothing, EmptyTuple] {
  def out(collectedInfo: CollectedInfo.Empty) = info.map(Value[Info, 0](_, 0))
}
