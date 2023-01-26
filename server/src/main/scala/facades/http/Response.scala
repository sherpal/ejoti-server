package facades.http

import ejoti.domain.{Header, Status}

import scala.scalajs.js.JSConverters.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.typedarray.{TypedArrayBuffer, Uint8Array}

@js.native
trait Response extends js.Object {

  @JSName("writeHead")
  def writeHeadJS(status: Int, headers: js.Dictionary[String]): this.type = js.native

  @JSName("write")
  def writeJS(chunk: Uint8Array): Unit = js.native

  def end(body: String): Unit = js.native

  def end(): Unit = js.native
  
  def writableEnded: Boolean = js.native

}

object Response {
  extension (res: Response) {
    @inline def writeHead(status: Status[Int], headers: List[Header]): res.type =
      res.writeHeadJS(status.code, headers.map(_.keyValue).toMap.toJSDictionary)

    @inline def write(chunk: zio.Chunk[Byte]): Unit = res.writeJS(Uint8Array.from(chunk.toList.toJSArray.map(_.toShort)))
  }
}
