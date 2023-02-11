package ejoti.domain.nodeast

import ejoti.domain.Request.RawRequest
import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class EitherNode[Left, Right, In <: Tuple](f: CollectedInfo[In] => Either[Left, Right])
    extends Node[Any, Left *: Right *: EmptyTuple, Nothing, In] {
  def out(
      collectedInfo: CollectedInfo[In]
  ): ZIO[Any, Nothing, Value[Left, 0] | Value[Right, 1]] =
    ZIO.succeed(f(collectedInfo) match {
      case Left(left)   => Value[Left, 0](left, 0)
      case Right(right) => Value[Right, 1](right, 1)
    })
}
