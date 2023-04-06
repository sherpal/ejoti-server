package ejoti.domain.nodeast

import ejoti.domain.*
import zio.{Chunk, ZIO}

import ejoti.domain.*
import ejoti.domain.Node.*
import zio.ZIO

class MultipartFormDataBodyNode
    extends Node[Any, Unit *: MultipartFormData *: EmptyTuple, Nothing, Request.RawRequest *: EmptyTuple] {

  def out(info: CollectedInfo[Request.RawRequest *: EmptyTuple]) = {
    val request = info.access[Request.RawRequest]

    request.maybeHeader[Header.ContentType].flatMap(_.maybeMultipartFormDataBoundary) match {
      case None => ZIO.succeed(Node.Value[Unit, 0]((), 0))
      case Some(boundaryData) =>
        val boundaryByteChunk = Chunk.fromArray(("--" ++ boundaryData.boundary).bytes)
        val end               = "--".bytes

        // todo: fixme: this puts everything in memory!
        val output = MultipartFormData(
          request.body
            .splitOnChunk(boundaryByteChunk)
            .filterNot(_.isEmpty)
            .takeWhile(!_.startsWith(end))
            .mapZIO(MultipartFormData.BodyParts.fromInMemoryChunk)
            .orDie
        )
        ZIO.succeed(Node.Value[MultipartFormData, 1](output, 1))
    }

  }

}

object MultipartFormDataBodyNode {
  import ejoti.domain.Node.given

  def multipartOrFail = new MultipartFormDataBodyNode().outlet[0].exitWith(Response.BadRequest)

}
