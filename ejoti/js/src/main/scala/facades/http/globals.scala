package facades.http

import zio.{Chunk, ZIO}
import scala.scalajs.js.typedarray.Uint8Array
import scalajs.js.JSConverters.JSRichIterableOnce
import scala.scalajs.js.typedarray.TypedArrayBuffer
import java.nio.{ByteBuffer => JByteBuffer}
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBuffer
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

def chunkToUint8Array(chunk: Chunk[Byte]): Uint8Array = {
  import scala.language.unsafeNulls
  val javaByteBuffer = JByteBuffer.wrap(chunk.toArray)
  val arrayBuffer =
    if javaByteBuffer.hasArrayBuffer() then javaByteBuffer.arrayBuffer()
    else {
      val ab = new ArrayBuffer(javaByteBuffer.remaining())
      TypedArrayBuffer.wrap(ab).put(javaByteBuffer)
      ab
    }
  new Uint8Array(arrayBuffer, 0, arrayBuffer.byteLength)
}
def uint8ArrayToChunk(uint8Array: Uint8Array): Chunk[Byte] =
  Chunk.fromByteBuffer(TypedArrayBuffer.wrap(uint8Array.buffer))

private[http] def handleNodeResponseFromEjotiResponse(
    jsResponse: Response,
    response: ejoti.domain.Response
): ZIO[Any, Nothing, Unit] = (for {
  _ <- ZIO.succeed(jsResponse.writeHead(response.status, response.headers.toList))
  _ <- response.body.chunks.runForeach(chunk => ZIO.succeed(jsResponse.write(chunk)))
  _ <- ZIO.succeed(jsResponse.end())
} yield ()).ensuring(response.finalizer)
