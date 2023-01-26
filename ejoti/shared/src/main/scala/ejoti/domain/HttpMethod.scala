package ejoti.domain

import zio.ZIO

/** Enum of existing Http methods
  */
sealed trait HttpMethod

object HttpMethod {

  sealed trait GET extends HttpMethod
  case object GET extends GET {
    given ValueOf[GET] = ValueOf(this)
  }

  sealed trait POST extends HttpMethod
  case object POST extends POST {
    given ValueOf[POST] = ValueOf(this)
  }

  sealed trait PUT extends HttpMethod
  case object PUT extends PUT {
    given ValueOf[PUT] = ValueOf(this)
  }

  sealed trait PATCH extends HttpMethod
  case object PATCH extends PATCH {
    given ValueOf[PATCH] = ValueOf(this)
  }

  sealed trait DELETE extends HttpMethod
  case object DELETE extends DELETE {
    given ValueOf[DELETE] = ValueOf(this)
  }

  sealed trait OPTIONS extends HttpMethod
  case object OPTIONS extends OPTIONS {
    given ValueOf[OPTIONS] = ValueOf(this)
  }

  type CRUD = POST *: GET *: PATCH *: DELETE *: EmptyTuple

  def fromStringZIO(str: String): ZIO[Any, Nothing, HttpMethod] = str match
    case "GET"     => ZIO.succeed(GET)
    case "POST"    => ZIO.succeed(POST)
    case "PUT"     => ZIO.succeed(PUT)
    case "PATCH"   => ZIO.succeed(PATCH)
    case "DELETE"  => ZIO.succeed(DELETE)
    case "OPTIONS" => ZIO.succeed(OPTIONS)
    case other     => ZIO.die(new IllegalArgumentException(s"$other is not a valid http method!"))
}
