package facades.http

import ejoti.domain.Status
import facades.console.console
import zio.{Cause, Unsafe, ZIO}

import scala.scalajs.js.annotation.{JSImport, JSName}
import scala.scalajs.{js, LinkingInfo}
import scala.util.{Failure, Success}

@js.native
trait Http extends js.Object {

  @JSName("createServer")
  def createServerJS(handler: js.Function2[Request, Response, Unit]): Server = js.native

}

object Http {

  @JSImport("node:http", JSImport.Namespace)
  @js.native
  val http: Http = js.native

  extension (h: Http) {
    def createServer(handler: (Request, Response) => Unit): Server = h.createServerJS(handler)

    def createServerZIO[R](
        handler: ejoti.domain.Request.RawRequest => ZIO[R, Nothing, ejoti.domain.Response]
    ): ZIO[R, Nothing, Server] = for {
      runtime <- ZIO.runtime[R]
    } yield h.createServer { (jsRequest, jsResponse) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      Unsafe.unsafe { implicit unsafe =>
        runtime.unsafe.runToFuture((for {
          request  <- jsRequest.toEjotiRequest
          response <- handler(request)
          _        <- handleNodeResponseFromEjotiResponse(jsResponse, response)
        } yield ()).catchAllCause { (cause: Cause[Throwable]) =>
          ZIO.succeed {
            val message = cause.prettyPrint
            console.error(message)
            if !jsResponse.writableEnded then
              jsResponse.writeHead(Status.Internal, Nil)
              jsResponse.end(if LinkingInfo.developmentMode then message else "Internal error.")
          }
        })
      } onComplete {
        case Failure(exception) =>
          // should not happen
          exception.printStackTrace()
        case Success(_) => ()
      }
      ()
    }

    def createServerZIO[R](server: ejoti.domain.Server.RawServer[R]): ZIO[R, Nothing, Server] = createServerZIO(
      server.handleRequest
    )
  }

}
