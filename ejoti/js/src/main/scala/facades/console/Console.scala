package facades.console

import scala.scalajs.js

@js.native
trait Console extends js.Object {

  def log(any: Any*): Unit   = js.native
  def warn(any: Any*): Unit  = js.native
  def error(any: Any*): Unit = js.native

}
