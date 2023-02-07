package ejoti.domain.nodeast

import ejoti.domain.*
import ejoti.domain.Node.*
import ejoti.domain.HttpMethod.*
import ejoti.domain.Request.RawRequest
import zio.ZIO

object CrudNode extends Node[Any, HttpMethod.CRUD, Response, Singleton[RawRequest]] {
  def out(
      collectedInfo: CollectedInfo[Singleton[RawRequest]]
  ): ZIO[Any, Nothing, Choices[HttpMethod.CRUD] | Response] = ZIO.succeed {
    collectedInfo.access[RawRequest].method match {
      case POST   => Value[POST, 0](POST, 0)
      case GET    => Value[GET, 1](GET, 1)
      case PATCH  => Value[PATCH, 2](PATCH, 2)
      case DELETE => Value[DELETE, 3](DELETE, 3)
      case _      => Response.MethodNotAllowed
    }
  }
}
