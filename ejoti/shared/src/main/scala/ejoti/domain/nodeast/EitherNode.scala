package ejoti.domain.nodeast

import ejoti.domain.Request.RawRequest
import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

final class EitherNode[Left, Right, In](f: (In, RawRequest) => Either[Left, Right])(using
    ValueOf[CollectedInfo.IndexOf[RawRequest, In *: RawRequest *: EmptyTuple]]
) extends Node[Any, Left *: Right *: EmptyTuple, Nothing, In *: RawRequest *: EmptyTuple] {
  def out(
      collectedInfo: CollectedInfo[In *: RawRequest *: EmptyTuple]
  ): ZIO[Any, Nothing, Value[Left, 0] | Value[Right, 1]] =
    ZIO.succeed(f(collectedInfo.access[In], collectedInfo.access[RawRequest]) match {
      case Left(left)   => Value[Left, 0](left, 0)
      case Right(right) => Value[Right, 1](right, 1)
    })
}
