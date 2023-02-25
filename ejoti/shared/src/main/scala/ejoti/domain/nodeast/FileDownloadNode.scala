package ejoti.domain.nodeast

import fs2.{text, Stream}
import fs2.io.file.{Files, Path}
import zio.interop.catz._
import zio.stream.interop.fs2z._
import ejoti.domain.*
import ejoti.domain.Node.*
import zio.{Task, ZIO}
import fs2.io.file.Flags
import urldsl.language.dummyErrorImpl.*
import urldsl.language.*
import urldsl.vocabulary.Segment
import urldsl.errors.DummyError
import ejoti.domain.Node.given
import ejoti.domain.CollectedInfo.given
import ejoti.domain.Node.*

final class FileDownloadNode(chunkSize: Int = FileDownloadNode.defaultChunkSize)
    extends Node[Any, EmptyTuple, Response, Singleton[Path]] {

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

  def serveStatic(staticFolder: Path, prefix: PathSegment[Unit, DummyError], chunkSize: Int = defaultChunkSize) = {
    val fileDownloadLeaf =
      Node
        .mappingNode((segments: Node.navigation.UnusedSegments) =>
          staticFolder / Path(segments.segments.map(_.content).mkString("/"))
        )
        .outlet[0]
        .attach(Node.sendFile(chunkSize))

    val navigationNode = Node.navigation.initialSegments.outlet[0].attach(Node.navigation.pathPrefix(prefix))

    navigationNode.outlet[1].attach(fileDownloadLeaf)
  }

  // todo, see https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
  def contentTypeFromExt(path: Path): Header.ContentType = path.extName.toLowerCase() match {
    case ".css"  => Header.ContentType.`text/css`
    case ".csv"  => Header.ContentType.`text/csv`
    case ".html" => Header.ContentType.`text/html`
    case ".htm"  => Header.ContentType.`text/html`
    case ".jpg"  => Header.ContentType.`image/jpeg`
    case ".js"   => Header.ContentType.`text/javascript`
    case ".json" => Header.ContentType.`application/json`
    case ".mjs"  => Header.ContentType.`text/javascript`
    case ".pdf"  => Header.ContentType.`application/pdf`
    case ".svg"  => Header.ContentType.`image/svg+xml`
    case ".txt"  => Header.ContentType.`text/plain`
    case _       => Header.ContentType.`application/octet-stream`
  }

  val defaultChunkSize = 64 * 1024

}
