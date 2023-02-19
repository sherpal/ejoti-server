package ejoti.domain

import zio.ZIO

/** Represents a Http header */
sealed trait Header(val name: String, val value: String) {
  def keyValue: (String, String) = name -> value
}

object Header {

  case class ContentType(tpe: String) extends Header("Content-Type", tpe)
  case class ContentEncoding(encoding: String) extends Header("Content-Encoding", encoding)
  case class Host(host: String) extends Header("Host", host)
  case class Connection(tpe: String) extends Header("Connection", tpe)
  case class ContentLength(length: Long) extends Header("Content-Length", length.toString)
  case class Origin(override val value: String) extends Header("Origin", value)
  case class Upgrade(override val value: String) extends Header("Upgrade", value)
  case class UserAgent(override val name: String) extends Header("User-Agent", name)

  case class RawHeader(override val name: String, override val value: String) extends Header(name, value)

  private val nameToDomain: Map[String, String => Header] = Map(
    "connection"     -> Connection.apply,
    "content-type"   -> ContentType.apply,
    "host"           -> Host.apply,
    "content-length" -> ContentLength.apply.compose(_.toLong),
    "user-agent"     -> UserAgent.apply,
    "upgrade"        -> Upgrade.apply,
    "origin"         -> Origin.apply
  )

  def fromKeyValuePairZIO(name: String, value: String): ZIO[Any, Nothing, Header] =
    ZIO.succeed(
      nameToDomain.getOrElse(
        {
          import scala.language.unsafeNulls
          name.toLowerCase
        },
        RawHeader(name, _)
      )(value)
    )

}
