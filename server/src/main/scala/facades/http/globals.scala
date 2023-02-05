package facades.http

import zio.{Chunk, ZIO}
import scala.scalajs.js.typedarray.Uint8Array
import scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.typedarray.TypedArrayBuffer

def chunkToUint8Array(chunk: Chunk[Byte]): Uint8Array =
  Uint8Array.from(chunk.toList.toJSArray.map(_.toShort))

def uint8ArrayToChunk(uint8Array: Uint8Array): Chunk[Byte] =
  Chunk.fromByteBuffer(TypedArrayBuffer.wrap(uint8Array.buffer))

private[http] def handleNodeResponseFromEjotiResponse(
    jsResponse: Response,
    response: ejoti.domain.Response
): ZIO[Any, Nothing, Unit] = for {
  _ <- ZIO.succeed(jsResponse.writeHead(response.status, response.headers))
  _ <- response.body.runForeach(chunk => ZIO.succeed(jsResponse.write(chunk)))
  _ <- ZIO.succeed(jsResponse.end())
} yield ()
