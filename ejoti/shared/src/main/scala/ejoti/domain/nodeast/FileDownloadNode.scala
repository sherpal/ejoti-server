package ejoti.domain.nodeast

import fs2.{text, Stream}
import fs2.io.file.{Files, Path}
import zio.interop.catz._
import zio.stream.interop.fs2z._
import ejoti.domain.*
import ejoti.domain.Node.*
import zio.{Task, ZIO}
import fs2.io.file.Flags

final class FileDownloadNode(chunkSize: Int = 8 * 1024) extends Node[Any, EmptyTuple, Response, Singleton[Path]] {

  type IO[+A] = Task[A]

  lazy val files = Files[IO]

  def out(collectedInfo: CollectedInfo[Singleton[Path]]): ZIO[Any, Nothing, Response] = {
    val path = collectedInfo.access[Path]

    val filesBytes: Stream[IO, fs2.Chunk[Byte]] = files.readAll(path, chunkSize, Flags.Read).chunks
    val zFilesBytes                             = filesBytes.toZStream()

    ZIO.ifZIO(files.exists(path).orDie)(
      ZIO.succeed(
        Response(
          Status.Ok,
          List(FileDownloadNode.contentTypeFromExt(path)),
          zFilesBytes.map(chunk => zio.Chunk.fromArray(chunk.toArray)).orDie
        )
      ),
      ZIO.succeed(Response.NotFound)
    )

  }
}

object FileDownloadNode {

  // todo, see https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
  def contentTypeFromExt(path: Path): Header.ContentType = Header.ContentType(path.extName match {
    case ".css"  => "text/css"
    case ".csv"  => "text/csv"
    case ".html" => "text/html"
    case ".htm"  => "text/html"
    case ".js"   => "text/javascript"
    case ".json" => "application/json"
    case ".mjs"  => "text/javascript"
    case ".txt"  => "text/plain"
    case _       => "application/octet-stream"
  })

}
