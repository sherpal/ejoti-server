package ejoti.domain

import zio.ZIO
import zio.Chunk

final case class TestResponse[Body](status: Status[_], headers: List[Header], body: Body) {
  inline def assertCode[Code <: Int](using value: ValueOf[Code]): Boolean = status.code == value.value
}

object TestResponse {

  def fromResponseZIO[Body](response: Response, makeBody: Chunk[Byte] => Body): ZIO[Any, Nothing, TestResponse[Body]] =
    for {
      body <- response.body.runCollect.map(_.flatten).map(makeBody)
    } yield TestResponse(response.status, response.headers, body)

}
