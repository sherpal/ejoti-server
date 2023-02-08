package facades.console

import scala.scalajs.js

val console: Console = js.Dynamic.global.console.asInstanceOf[Console]
