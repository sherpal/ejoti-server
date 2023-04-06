package ejoti.domain.nodeast

import ejoti.domain.Node
import ejoti.domain.Response
import ejoti.domain.Node.Choices
import ejoti.domain.CollectedInfo
import zio.ZIO

final class ResponseMappedNode[R, X <: Tuple, Exit <: Node.ExitType, IncomingInfo <: Tuple](
    from: Node[R, X, Exit, IncomingInfo],
    map: (CollectedInfo[IncomingInfo], Exit) => Response
)(using scala.reflect.Typeable[Exit])
    extends Node[R, X, Response, IncomingInfo] {

  def out(collectedInfo: CollectedInfo[IncomingInfo]): ZIO[R, Nothing, Choices[X] | Response] =
    from.out(collectedInfo).map {
      case response: Exit                           => map(collectedInfo, response)
      case choice: Choices[X @unchecked] @unchecked => choice
    }

}
