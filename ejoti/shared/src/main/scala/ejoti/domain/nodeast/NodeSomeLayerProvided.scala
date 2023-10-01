package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.*

class NodeSomeLayerProvided[R0, R1, -R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple](
    layer: ZLayer[R0, Nothing, R1],
    from: Node[R, X, Exit, IncomingInfo]
)(using
    R0 with R1 <:< R,
    EnvironmentTag[R1],
    Trace
) extends Node[R0, X, Exit, IncomingInfo] {

  def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R0, Nothing, Choices[X] | Exit] =
    from.out(collectedInfo).provideSomeLayer[R0](layer)

}

object NodeSomeLayerProvided {

  class NodeSomeLayerProvider[R0, -R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple](
      from: Node[R, X, Exit, IncomingInfo]
  ) {
    def apply[R1](
        layer: => ZLayer[R0, Nothing, R1]
    )(using
        R0 with R1 <:< R,
        EnvironmentTag[R1],
        Trace
    ): NodeSomeLayerProvided[R0, R1, R, X, Exit, IncomingInfo] = NodeSomeLayerProvided(layer, from)
  }

}
