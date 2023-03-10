package ejoti.domain

import java.time.LocalDateTime
import scala.concurrent.duration.FiniteDuration
import scala.language.unsafeNulls
import scala.concurrent.duration.Duration

final case class HttpCookie(
    name: String,
    value: String,
    maybeDomain: Option[String] = None,
    maybeExpires: Option[LocalDateTime] = None,
    maybeHttpOnly: Option[Boolean] = None,
    maybeMaxAge: Option[FiniteDuration] = None,
    maybePartitioned: Option[Boolean] = None,
    maybePath: Option[String] = None,
    maybeSameSite: Option[HttpCookie.SameSite] = None,
    maybeSecure: Option[Boolean] = None
) {

  def setCookie: Header.SetCookie = Header.SetCookie(this)

  def setCookieValue: String = List(
    Some(s"$name=$value"),
    maybeDomain.map("Domain=" ++ _),
    maybeExpires.map(HttpCookie.expiresFormatter.format).map("Expires=" ++ _ ++ " GMT"),
    maybeHttpOnly.filter(identity).map(_ => "HttpOnly"),
    maybeMaxAge.map(_.toSeconds.toString).map("Max-Age=" ++ _),
    maybePartitioned.filter(identity).map(_ => "Partitioned"),
    maybePath.map("Path=" ++ _),
    maybeSameSite.map("SameSite=" ++ _.value),
    maybeSecure.filter(identity).map(_ => "Secure")
  ).flatten.mkString("; ")

  def expiresNow: HttpCookie = copy(maybeMaxAge = Some(Duration.Zero))
}

object HttpCookie {

  private val expiresFormatter = java.time.format.DateTimeFormatter.ofPattern(
    "EEE, dd LLL YYYY HH:mm:ss"
  )

  sealed trait SameSite {
    def value: String = this.toString
  }
  object SameSite {
    case object Strict extends SameSite
    case object Lax extends SameSite
    case object None extends SameSite
  }

}
