package ejoti.domain

import zio.ZIO

/** Represents a Http header */
sealed trait Header(name: String, value: String) {
  def keyValue: (String, String) = name -> value
}

object Header {

  case class ContentType(tpe: String) extends Header("Content-Type", tpe)
  case class Host(host: String) extends Header("Host", host)
  case class Connection(tpe: String) extends Header("Connection", tpe)
  case class ContentLength(length: Long) extends Header("Content-Length", length.toString)
  case class UserAgent(name: String) extends Header("User-Agent", name)

  case class RawHeader(name: String, value: String) extends Header(name, value)

  private val nameToDomain: Map[String, String => Header] = Map(
    "connection"     -> Connection.apply,
    "content-type"   -> ContentType.apply,
    "host"           -> Host.apply,
    "content-length" -> ContentLength.apply.compose(_.toLong),
    "user-agent"     -> UserAgent.apply
  )

  def fromKeyValuePairZIO(name: String, value: String): ZIO[Any, Nothing, Header] =
    ZIO.succeed(nameToDomain.getOrElse({
      import scala.language.unsafeNulls
      name.toLowerCase
    }, RawHeader(name, _))(value))

}
