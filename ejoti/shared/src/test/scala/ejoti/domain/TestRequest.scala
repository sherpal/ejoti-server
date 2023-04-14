package ejoti.domain

import zio.stream.*
import zio.Chunk
import ejoti.domain.Header.Headers

object TestRequest {

  def get = fromMethod(HttpMethod.GET)

  def fromMethod(method: HttpMethod): Request.RawRequest =
    Request(method, Headers.empty, Nil, Map.empty, "", ZStream.empty)

  def fromBodyString(body: String): Request.RawRequest = get.withStreamBodyAsString(body)

}
