package ejoti.domain

import zio.{Chunk, ZIO}
import zio.stream.ZStream
import zio.stream.ZSink

case class MultipartFormData(parts: ZStream[Any, Nothing, MultipartFormData.BodyParts]) {

  def toInMemory(totalLimit: Int, limitPerParts: Int) = parts
    .mapZIO(_.toInMemory(limitPerParts))
    .run(
      MultipartFormData.sinkWithWeightedLimit[MultipartFormData.InMemoryBodyParts](_.flatMap(_.body).size)(totalLimit)
    )
    .map(_.toList)
    .map(MultipartFormData.InMemoryMultipartFormData(_))

}

object MultipartFormData {

  case class InMemoryMultipartFormData(parts: List[InMemoryBodyParts])

  sealed trait BodyParts {
    def headers: Vector[Header]

    /** Optionally returns the [[Header.ContentDisposition]] for this part. */
    def maybeContentDisposition: Option[Header.ContentDisposition] = headers.collectFirst {
      case cd: Header.ContentDisposition =>
        cd
    }

    /** Returns the Content Disposition param map for this part, or the empty map if there is no such header */
    def contentDispositionParams: Map[String, String] = maybeContentDisposition.fold(Map.empty)(_.params)

    def toInMemory(maxSize: Int): ZIO[Any, LimitExceeded, InMemoryBodyParts]

    def bodyStream: ZStream[Any, Nothing, Byte]
  }

  case class BodyPartsStreamed(headers: Vector[Header], body: ZStream[Any, Nothing, Byte]) extends BodyParts {
    def bodyStream: ZStream[Any, Nothing, Byte] = body

    def toInMemory(maxSize: Int): ZIO[Any, LimitExceeded, InMemoryBodyParts] =
      body.run(sinkWithLimit[Byte](maxSize)).map(InMemoryBodyParts(headers, _))

  }

  case class InMemoryBodyParts(headers: Vector[Header], body: Chunk[Byte]) extends BodyParts {

    def toInMemory(maxSize: Int): ZIO[Any, LimitExceeded, InMemoryBodyParts] =
      ZIO.ifZIO(ZIO.succeed(body.size <= maxSize))(ZIO.succeed(this), ZIO.fail(LimitExceeded(maxSize)))

    def bodyStream: ZStream[Any, Nothing, Byte] = ZStream.fromChunk(body)

  }

  object BodyParts {

    def findSliceIndex[T](chunk: Chunk[T], slice: Chunk[T]): Int = {
      val firstT      = slice(0)
      val sliceLength = slice.length
      val chunkLength = chunk.length
      chunk.zipWithIndex.find((t, idx) => t == firstT && chunk.slice(idx, idx + sliceLength) == slice).fold(-1)(_._2)
    }

    import scala.language.unsafeNulls
    def fromInMemoryChunk(chunk: Chunk[Byte]): ZIO[Any, Throwable, BodyParts] = {
      val separator = Chunk.fromArray("\r\n".bytes)

      val (headersChunk, bodyChunkWithSep) = chunk.splitAt(findSliceIndex(chunk, separator ++ separator))
      val bodyChunk                        = bodyChunkWithSep.drop(separator.length * 2).dropRight(separator.length)

      for {
        headerLinesChunks <- ZStream.fromChunk(headersChunk).splitOnChunk(separator).runCollect.map(_.tail)
        headerLines = headerLinesChunks.map(_.asString).toVector
        headers <- ZIO.foreach(headerLines) { headerInfo =>
          val (key, value) = headerInfo.splitAt(headerInfo.indexOf(":"))
          Header.fromKeyValuePairZIO(key, value.tail.trim())
        }
      } yield BodyPartsStreamed(headers, ZStream.fromChunk(bodyChunk))

    }
  }

  class LimitExceeded(val limit: Int) extends Throwable(s"Exceeded limit of $limit.")
  def sinkWithLimit[T](limit: Int) =
    sinkWithWeightedLimit((_: Chunk[T]).size)(limit)

  def sinkWithWeightedLimit[T](weight: Chunk[T] => Int)(limit: Int) = ZSink
    .foldLeftChunksZIO[Any, LimitExceeded, T, (Chunk[T], Int)]((Chunk.empty[T], 0)) {
      case ((accChunk: Chunk[T], accSize: Int), next: Chunk[T]) =>
        val nextSize = accSize + weight(next)
        ZIO.ifZIO(ZIO.succeed(nextSize > limit))(
          ZIO.fail(LimitExceeded(limit)),
          ZIO.succeed((accChunk ++ next, nextSize))
        )
    }
    .map(_._1)
}
