package facades.http

import ejoti.domain.Header
import urldsl.url.UrlStringParserGenerator
import zio.stream.ZStream
import zio.{Chunk, ZIO}

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.annotation.JSName
import facades.node.EventEmitter

@js.native
trait Request extends EventEmitter {

  def method: String = js.native

  def url: String = js.native

  def host: String | Unit = js.native

  def protocol: String | Unit = js.native

  def rawHeaders: js.Array[String] = js.native

  def writableEnded: Boolean = js.native

}

object Request {

  private val urlStringParserGenerator: UrlStringParserGenerator =
    UrlStringParserGenerator.defaultUrlStringParserGenerator

  extension (req: Request) {
    def onChunk(handler: Chunk[Byte] => Unit): Unit = req.on[Uint8Array](
      "data",
      (chunk: Uint8Array) => handler(uint8ArrayToChunk(chunk))
    )

    def onEnd(body: => Unit): Unit = req.on[js.Any]("end", (_: js.Any) => body)

    def onUpgrade(handler: (Response, Socket, Uint8Array) => Unit): Unit =
      req.on3[Response, Socket, Uint8Array]("upgrade", handler)

    def bodyStream: ZIO[Any, Nothing, ZStream[Any, Nothing, Byte]] = for {
      queue   <- zio.Queue.unbounded[Chunk[Byte]]
      runtime <- ZIO.runtime[Any]
      _       <- ZIO.succeed(req.onChunk(chunk => runtime.unsafe.run(queue.offer(chunk))))
      _ <- ZIO.succeed(req.onEnd(runtime.unsafe.runToFuture(queue.isEmpty.repeatUntil(identity) *> queue.shutdown)))
    } yield zio.stream.ZStream.fromChunkQueue(queue)

    def toEjotiRequest: ZIO[Any, Nothing, ejoti.domain.Request.RawRequest] = for {
      body   <- bodyStream
      method <- ejoti.domain.HttpMethod.fromStringZIO(req.method)
      headers <- ZIO.foreach(
        req.rawHeaders
          .grouped(2)
          .map(_.toList)
          .collect { case name :: value :: Nil =>
            (name, value)
          }
          .toVector
      )(Header.fromKeyValuePairZIO)
      host <- ZIO
        .from(headers.collectFirst { case Header.Host(host) => host })
        .orElseFail(new IllegalStateException(s"No host header"))
        .orDie
      urlStringParser <- ZIO.succeed(urlStringParserGenerator.parser(s"http://$host${req.url}"))
    } yield ejoti.domain.Request(method, headers, urlStringParser.segments, urlStringParser.params, host, body)
  }

}
