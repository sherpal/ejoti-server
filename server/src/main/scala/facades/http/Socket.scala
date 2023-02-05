package facades.http

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.annotation.JSName
import zio.{Chunk, ZIO}
import scalajs.js.JSConverters.JSRichIterableOnce
import java.nio.charset.StandardCharsets
import zio.Unsafe

@js.native
trait Socket extends js.Object {

  def on[Data <: js.Any](name: String, handler: js.Function1[Data, Unit]): Unit = js.native

  @JSName("write")
  def writeJS(buffer: Uint8Array): Unit = js.native

  @JSName("write")
  def writeJS(str: String): Unit = js.native

  def end(): Unit = js.native

}

object Socket {

  extension (socket: Socket) {
    def write(chunk: Chunk[Byte]): Unit = socket.writeJS(chunkToUint8Array(chunk))

    def write(str: String): Unit = write(Chunk.fromArray {
      import scala.language.unsafeNulls
      str.getBytes(StandardCharsets.UTF_8)
    })

    def writeZIO(chunk: Chunk[Byte]): ZIO[Any, Nothing, Unit] = ZIO.succeed(write(chunk))

    def writeZIO(str: String): ZIO[Any, Nothing, Unit] = ZIO.succeed(write(str))

    def writeZIO(message: ejoti.domain.WebSocketResponse.Message): ZIO[Any, Nothing, Unit] = message match {
      case ejoti.domain.WebSocketResponse.BinaryMessage(chunk) => writeZIO(chunk)
      case ejoti.domain.WebSocketResponse.StringMessage(str)   => writeZIO(str)
    }

    def onDataJS(handler: js.Function1[Uint8Array, Unit]): Unit = socket.on("data", handler)

    def onDataZIO[R](handler: Chunk[Byte] => ZIO[R, Nothing, Unit]): ZIO[R, Nothing, Unit] = for {
      runtime <- ZIO.runtime[R]
      _ <- ZIO.succeed { implicit unsafe: Unsafe =>
        socket.onDataJS { uint8Array =>
          val chunk  = uint8ArrayToChunk(uint8Array)
          val effect = handler(chunk)
          runtime.unsafe.runToFuture(effect)
        }
      }
    } yield ()
  }

}
