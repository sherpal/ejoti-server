package ejoti.domain

import zio.stream.*
import zio.Chunk

object TestRequest {

  def get = fromMethod(HttpMethod.GET)

  def fromMethod(method: HttpMethod): Request.RawRequest =
    Request(method, Vector.empty, Nil, Map.empty, "", ZStream.empty)

  def fromBodyString(body: String): Request.RawRequest = get.withStreamBodyAsString(body)

}
