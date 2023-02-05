package facades.node

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

trait EventEmitter extends js.Object {

  def on[Data](event: String, handler: js.Function1[Data, Unit]): Unit

  @JSName("on")
  def on1[Data](event: String, handler: js.Function1[Data, Unit]): Unit

  @JSName("on")
  def on2[Data1, Data2](event: String, handler: js.Function2[Data1, Data2, Unit]): Unit

  @JSName("on")
  def on3[Data1, Data2, Data3](
      event: String,
      handler: js.Function3[Data1, Data2, Data3, Unit]
  ): Unit

}

object EventEmitter {
  @js.native
  trait Native extends EventEmitter {
    def on[Data](event: String, handler: js.Function1[Data, Unit]): Unit = js.native

    @JSName("on")
    def on1[Data](event: String, handler: js.Function1[Data, Unit]): Unit = js.native

    @JSName("on")
    def on2[Data1, Data2](event: String, handler: js.Function2[Data1, Data2, Unit]): Unit =
      js.native

    @JSName("on")
    def on3[Data1, Data2, Data3](
        event: String,
        handler: js.Function3[Data1, Data2, Data3, Unit]
    ): Unit = js.native
  }
}
