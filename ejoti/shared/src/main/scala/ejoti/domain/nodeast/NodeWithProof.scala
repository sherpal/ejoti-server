package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*

final class NodeWithProof[-R, X <: Tuple, Exit <: ExitType, IncomingInfo <: Tuple, Proof](
    val node: Node[R, X, Exit, IncomingInfo]
)(using
    ValueOf[CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]],
    CollectedInfo.Elems[
      Proof *: IncomingInfo,
      CollectedInfo.IndexesOf[IncomingInfo, Proof *: IncomingInfo]
    ] =:= IncomingInfo
) extends Node[R, X, Exit, Proof *: IncomingInfo] {
  def out(collectedInfo: CollectedInfo[Proof *: IncomingInfo]) = node.out(collectedInfo)
}
