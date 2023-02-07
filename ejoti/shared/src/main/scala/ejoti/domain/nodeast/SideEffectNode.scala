package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class SideEffectNode[-R, In <: Tuple](effect: CollectedInfo[In] => ZIO[R, Nothing, Unit])
    extends Node[R, Singleton[Unit], Nothing, In] {
  def out(in: CollectedInfo[In]): ZIO[R, Nothing, Value[Unit, 0]] = effect(in).map(Value(_, 0))
}
