package ejoti.domain.nodeast

import fs2.{text, Stream}
import fs2.io.file.{Files, Path}
import zio.interop.catz._
import zio.Unsafe
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
import zio.stream.ZStream
import fs2.io.file.FileKey
import fs2.io.file.BasicFileAttributes
import ejoti.domain.Header.Headers
import ejoti.domain.Request.RawRequest
import ejoti.domain.Header.StrongETag
import ejoti.domain.Header.WeakETag
import ejoti.domain.nodeast.filedownload.*

final class FileDownloadNode(chunkSize: Int = FileDownloadNode.defaultChunkSize)
    extends Node[FileDownloadSharedResource, Unit *: EmptyTuple, Response, Path *: Headers *: EmptyTuple] {

  type IO[+A] = Task[A]

  lazy val files = Files[IO]

  def out(
      collectedInfo: CollectedInfo[Path *: Headers *: EmptyTuple]
  ): ZIO[FileDownloadSharedResource, Nothing, Response | Value[Unit, 0]] = {
    val path             = collectedInfo.access[Path]
    val headers          = collectedInfo.access[Headers]
    val maybeIfNoneMatch = headers.maybeHeaderOfType[Header.IfNoneMatch]

    lazy val filesBytes: Stream[IO, fs2.Chunk[Byte]] = files.readAll(path, chunkSize, Flags.Read).chunks
    lazy val zFilesBytes                             = filesBytes.toZStream()

    ZIO.ifZIO(files.exists(path).orDie)(
      for {
        maybeBasicFileAttributes <- files
          .getBasicFileAttributes(path)
          .map(Some(_))
          .orElse(ZIO.succeed(Option.empty[BasicFileAttributes]))
        etag = maybeBasicFileAttributes
          .flatMap(_.fileKey)
          .fold[Header.ETag](Header.ETag.weak(path.toString.hashCode().toString))((fileKey: FileKey) =>
            Header.ETag.strong(fileKey.hashCode().toString)
          )
        ifNoneMatchMatched = maybeIfNoneMatch.map(_.value).contains[String](etag.value)
        headers = Headers(
          List(FileDownloadNode.contentTypeFromExt(path), etag, Header.CacheControl.noCache) ++ maybeBasicFileAttributes
            .map(attributes => Header.ContentLength(attributes.size))
        )
        response <-
          if ifNoneMatchMatched then ZIO.succeed(Response.NotModified)
          else
            for {
              fileDownloadResourceService <- ZIO.service[FileDownloadSharedResource]
              _                           <- fileDownloadResourceService.acquire
              stream = zFilesBytes.map(chunk => zio.Chunk.fromArray(chunk.toArray)).flatMap(ZStream.fromChunk).orDie
            } yield Response(Status.Ok, headers, stream, fileDownloadResourceService.release)
      } yield response,
      ZIO.succeed(Value[Unit, 0]((), 0))
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
        .provide[Node.Singleton[RawRequest]]
        .outlet[0]
        .attach(Node.sendFile(chunkSize))

    val navigationNode = Node.navigation.initialSegments
      .outlet[0]
      .map(info => info.access[Request.RawRequest].maybeHeader[Header.ETag])
      .outlet[0]
      .attach(Node.navigation.pathPrefix(prefix))

    navigationNode.outlet[1].attach(fileDownloadLeaf)
  }

  def fileDownloadNodeOrNotFound(chunkSize: Int = FileDownloadNode.defaultChunkSize) =
    FileDownloadNode(chunkSize).outlet[0].attach(notFound)

  def fileDownloadNode(
      chunkSize: Int = FileDownloadNode.defaultChunkSize
  ): Node[FileDownloadSharedResource, Unit *: EmptyTuple, Response, Path *: Headers *: EmptyTuple] =
    new FileDownloadNode(chunkSize)

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
    case ".png"  => Header.ContentType.`image/png`
    case _       => Header.ContentType.`application/octet-stream`
  }

  val defaultChunkSize = 64 * 1024

}
