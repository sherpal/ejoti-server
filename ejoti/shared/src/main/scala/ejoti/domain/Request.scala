package ejoti.domain

import urldsl.errors.DummyError
import urldsl.language.QueryParameters
import urldsl.vocabulary.{Param, ParamMatchOutput, Segment, UrlMatching}
import zio.{Chunk, ZIO}
import zio.stream.ZStream
import urldsl.language.PathSegment

/** Represents an incoming Request from a client.
  *
  * @param method
  *   http method used in the request
  * @param headers
  *   all headers from the request
  * @param segments
  *   path segments from the request, already uri-decoded
  * @param params
  *   query params from the request, already uri-decoded
  * @param host
  *   incoming host
  * @param body
  *   body of the request
  */
final case class Request[Body](
    method: HttpMethod,
    headers: Vector[Header],
    segments: List[Segment],
    params: Map[String, Param],
    host: String,
    body: Body
) {

  lazy val completeUrl =
    s"http://$host/${Request.pathAndQuery.createPart(UrlMatching(segments.map(_.content), params))}"

  def copyChangeBody[Body0](newBody: Body0): Request[Body0] = Request(method, headers, segments, params, host, newBody)

  def bodyAsString(using ev: Body <:< ZStream[Any, Nothing, Byte]): ZIO[Any, Nothing, String] =
    ev(body).runCollect.map(_.toArray).map(new String(_))

  def withBodyAsString(using Body <:< ZStream[Any, Nothing, Byte]): ZIO[Any, Nothing, Request[String]] =
    mapBody(_ => bodyAsString)

  def mapBody[R, Body0](f: Body => ZIO[R, Nothing, Body0]): ZIO[R, Nothing, Request[Body0]] =
    f(body).map(copyChangeBody[Body0])

  // methods below are intended to be used in tests only

  private[domain] def withBody[Body0](newBody: Body0): Request[Body0] = copyChangeBody(newBody)

  private[domain] def withMethod(method: HttpMethod): Request[Body] = copy(method = method)

  private[domain] def withSegments(segments: Segment*): Request[Body] = copy(segments = segments.toList)

  private[domain] def withPath[T](path: PathSegment[T, Any])(t: T): Request[Body] = withSegments(
    path.createSegments(t): _*
  )

  private[domain] def withStreamBodyAsString(body: String): Request.RawRequest =
    withBody {
      val bytes = {
        import scala.language.unsafeNulls
        body.getBytes()
      }
      ZStream.fromIterable(Chunk.fromArray(bytes))
    }

}

object Request {

  type RawRequest = Request[ZStream[Any, Nothing, Byte]]

  private val pathAndQuery = {
    import urldsl.language.dummyErrorImpl._
    val query = new QueryParameters[Map[String, Param], DummyError] {
      override def createParams(q: Map[String, Param]): Map[String, Param] = q

      override def matchParams(params: Map[String, Param]): Either[DummyError, ParamMatchOutput[Map[String, Param]]] =
        Right(ParamMatchOutput(params, Map.empty))
    }
    (root / remainingSegments) ? query
  }

}
