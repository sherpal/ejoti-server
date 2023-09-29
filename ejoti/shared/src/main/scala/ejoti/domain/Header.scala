package ejoti.domain

import zio.ZIO
import scala.concurrent.duration.*
import scala.reflect.Typeable

/** Represents a Http header */
sealed trait Header(val name: String, val value: String) {
  def keyValue: (String, String) = name -> value

  def prettyPrint: String = s"$name: $value"
}

object Header {

  opaque type Headers <: List[Header] = List[Header]

  object Headers {
    inline def apply(headers: Seq[Header]): Headers = headers.toList

    inline def apply(headers: Header*)(using DummyImplicit): Headers = apply(headers)

    def empty: Headers = Nil

    private def _removeHeaderOfType[T <: Header](headers: List[Header])(using Typeable[T]): List[Header] =
      headers.filter {
        case _: T => false
        case _    => true
      }

    private def _removeHeaderOfType2[T1 <: Header, T2 <: Header](
        headers: List[Header]
    )(using Typeable[T1], Typeable[T2]): List[Header] =
      headers.filter {
        case _: T1 => false
        case _: T2 => false
        case _     => true
      }

    private def _removeHeaderOfType3[T1 <: Header, T2 <: Header, T3 <: Header](
        headers: List[Header]
    )(using Typeable[T1], Typeable[T2], Typeable[T3]): List[Header] =
      headers.filter {
        case _: T1 => false
        case _: T2 => false
        case _: T3 => false
        case _     => true
      }

    private def _removeHeaderOfName(name: String, headers: List[Header]): List[Header] =
      headers.filterNot(_.name == name)

    extension (headers: Headers) {
      def removeHeaderOfType[T <: Header](using Typeable[T]): Headers = _removeHeaderOfType[T](headers)

      def removeHeaderOfName(name: String): Headers = _removeHeaderOfName(name, headers)

      def addOrReplaceHeader[T <: Header](t: T)(using Typeable[T]): Headers = t +: _removeHeaderOfType[T](headers)

      def addOrReplaceHeaders[T1 <: Header, T2 <: Header](t1: T1, t2: T2)(using Typeable[T1], Typeable[T2]): Headers =
        t1 +: t2 +: _removeHeaderOfType2[T1, T2](headers)

      def addOrReplaceHeaders[T1 <: Header, T2 <: Header, T3 <: Header](t1: T1, t2: T2, t3: T3)(using
          Typeable[T1],
          Typeable[T2],
          Typeable[T3]
      ): Headers =
        t1 +: t2 +: t3 +: _removeHeaderOfType3[T1, T2, T3](headers)

      def addOrReplaceHeaderByName(header: Header): Headers = header +: _removeHeaderOfName(header.name, headers)

      def addHeader(header: Header): Headers = header +: headers

      def maybeHeaderOfType[T <: Header](using Typeable[T]): Option[T] = headers.collectFirst { case t: T =>
        t
      }

      def hasHeader(header: Header): Boolean = headers.contains[Header](header)
    }
  }

  case class MultipartFormDataBoundary(boundary: String)

  case class ContentType(tpe: String) extends Header("Content-Type", tpe) {

    /** Returns the boundary defined by this content type when it's a multipart/form-data
      *
      * @return
      */
    def maybeMultipartFormDataBoundary: Option[MultipartFormDataBoundary] = Option
      .when(tpe.startsWith("multipart/form-data"))(
        """boundary=([^;]+)""".r.findFirstIn(tpe).map(_.drop("boundary=".length)).map(MultipartFormDataBoundary.apply)
      )
      .flatten
  }

  object ContentType {
    lazy val `application/octet-stream`          = ContentType("application/octet-stream")
    lazy val `text/css`                          = Header.ContentType("text/css")
    lazy val `text/csv`                          = Header.ContentType("text/csv")
    lazy val `text/html`                         = Header.ContentType("text/html")
    lazy val `image/jpeg`                        = Header.ContentType("image/jpeg")
    lazy val `text/javascript`                   = Header.ContentType("text/javascript")
    lazy val `application/json`                  = Header.ContentType("application/json")
    lazy val `application/pdf`                   = Header.ContentType("application/pdf")
    lazy val `image/svg+xml`                     = Header.ContentType("image/svg+xml")
    lazy val `text/plain`                        = Header.ContentType("text/plain")
    lazy val `image/png`                         = Header.ContentType("image/png")
    lazy val `application/x-www-form-urlencoded` = Header.ContentType("application/x-www-form-urlencoded")
  }

  case class ContentEncoding(encoding: String) extends Header("Content-Encoding", encoding)
  case class Host(host: String) extends Header("Host", host)
  case class Connection(tpe: String) extends Header("Connection", tpe)
  case class ContentLength(length: Long) extends Header("Content-Length", length.toString)
  case class Location(uri: String) extends Header("Location", uri)
  case class Origin(override val value: String) extends Header("Origin", value)
  case class Upgrade(override val value: String) extends Header("Upgrade", value)
  case class UserAgent(override val name: String) extends Header("User-Agent", name)
  case class Cookie(cookiesString: String) extends Header("Cookie", cookiesString) {
    import scala.language.unsafeNulls
    lazy val cookies: Map[String, String] = cookiesString
      .split(";")
      .map(_.trim)
      .map(_.split("=").toList)
      .collect { case cookieName :: cookieValue :: Nil =>
        cookieName -> cookieValue
      }
      .toMap
  }
  case class SetCookie(cookie: HttpCookie) extends Header("Set-Cookie", cookie.setCookieValue)
  case class CacheControl(rawValue: String) extends Header("Cache-Control", rawValue)
  object CacheControl {
    val oneWeek = Some(7.days)
    val oneDay  = Some(1.day)
    def apply(maxAge: Option[FiniteDuration] = None): CacheControl = CacheControl(
      List(
        maxAge.map(age => s"max-age=${age.toSeconds}")
      ).flatten.mkString(", ")
    )

    lazy val noCache: CacheControl = CacheControl("no-cache")
  }
  sealed trait ETag(override val value: String) extends Header {
    def actualValue: String
  }
  case class StrongETag(actualValue: String)
      extends ETag(s""""$actualValue"""")
      with Header("ETag", s""""$actualValue"""")
  case class WeakETag(actualValue: String)
      extends ETag(s"""W/"$actualValue"""")
      with Header("ETag", s"""W/"$actualValue"""")
  object ETag {
    def weak(value: String): ETag   = WeakETag(value)
    def strong(value: String): ETag = StrongETag(value)

    def fromRawValue(rawValue: String): ETag = {
      import scala.language.unsafeNulls
      val isWeak = rawValue.startsWith("W")
      if isWeak then weak(rawValue.substring(3, rawValue.length - 1))
      else strong(rawValue.substring(1, rawValue.length - 1))
    }
  }

  private val contentDispositionParamsRegex = """([a-zA-Z]+)="(.+?)"""".r
  case class ContentDisposition(rawValue: String) extends Header("Content-Disposition", rawValue) {
    def params: Map[String, String] =
      contentDispositionParamsRegex.findAllIn(rawValue).map { case s"""$name="$value"""" => name -> value }.toMap
  }

  case class IfNoneMatch(override val value: String) extends Header("If-None-Match", value)

  case class RawHeader(override val name: String, override val value: String) extends Header(name, value)

  private val nameToDomain: Map[String, String => Header] = Map(
    "connection"          -> Connection.apply,
    "content-type"        -> ContentType.apply,
    "host"                -> Host.apply,
    "content-length"      -> ContentLength.apply.compose(_.toLong),
    "user-agent"          -> UserAgent.apply,
    "upgrade"             -> Upgrade.apply,
    "origin"              -> Origin.apply,
    "location"            -> Location.apply,
    "cookie"              -> Cookie.apply,
    "cache-control"       -> CacheControl.apply,
    "etag"                -> ETag.fromRawValue,
    "content-disposition" -> ContentDisposition.apply,
    "if-none-match"       -> IfNoneMatch.apply
  )

  def fromKeyValuePairZIO(name: String, value: String): ZIO[Any, Nothing, Header] =
    ZIO.succeed {
      nameToDomain.getOrElse(
        {
          import scala.language.unsafeNulls
          name.toLowerCase
        },
        RawHeader(name, _)
      )(value)
    }

}
