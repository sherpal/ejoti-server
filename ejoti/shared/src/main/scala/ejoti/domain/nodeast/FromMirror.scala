package ejoti.domain.nodeast

import scala.deriving.Mirror
import ejoti.domain.Node
import ejoti.domain.Node.Choices
import ejoti.domain.CollectedInfo
import zio.ZIO

object FromMirror {

  def apply[T](using mirror: Mirror.SumOf[T]): Node[Any, mirror.MirroredElemTypes, Nothing, Node.Singleton[T]] =
    new Node[Any, mirror.MirroredElemTypes, Nothing, Node.Singleton[T]] {
      def out(collectedInfo: CollectedInfo[Node.Singleton[T]]): ZIO[Any, Nothing, Choices[mirror.MirroredElemTypes]] =
        ZIO.succeed {
          val t     = collectedInfo.access[T]
          val index = mirror.ordinal(t)
          Node.Value(t, index).asInstanceOf[Choices[mirror.MirroredElemTypes]]
        }
    }

}
