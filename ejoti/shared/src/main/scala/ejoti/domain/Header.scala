package ejoti.domain

import zio.ZIO
import scala.concurrent.duration.*

/** Represents a Http header */
sealed trait Header(val name: String, val value: String) {
  def keyValue: (String, String) = name -> value

  def prettyPrint: String = s"$name: $value"
}

object Header {

  case class ContentType(tpe: String) extends Header("Content-Type", tpe)

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

  case class RawHeader(override val name: String, override val value: String) extends Header(name, value)

  private val nameToDomain: Map[String, String => Header] = Map(
    "connection"     -> Connection.apply,
    "content-type"   -> ContentType.apply,
    "host"           -> Host.apply,
    "content-length" -> ContentLength.apply.compose(_.toLong),
    "user-agent"     -> UserAgent.apply,
    "upgrade"        -> Upgrade.apply,
    "origin"         -> Origin.apply,
    "location"       -> Location.apply,
    "cookie"         -> Cookie.apply,
    "cache-control"  -> CacheControl.apply,
    "etag"           -> ETag.fromRawValue
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
