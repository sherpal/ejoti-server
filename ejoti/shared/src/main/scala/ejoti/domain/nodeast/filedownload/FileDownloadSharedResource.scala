package ejoti.domain.nodeast.filedownload

import zio.*

trait FileDownloadSharedResource {

  def acquire: ZIO[Any, Nothing, Unit]

  def release: ZIO[Any, Nothing, Unit]

}

object FileDownloadSharedResource {

  def fromPermits(numberOfPermits: Long): ZLayer[Any, Nothing, FileDownloadSharedResource] = ZLayer.fromZIO(for {
    theRef    <- Ref.make[Long](numberOfPermits)
    semaphore <- Semaphore.make(1)
  } yield new FileDownloadSharedResource {
    def acquire: ZIO[Any, Nothing, Unit] = semaphore.withPermit(for {
      _ <- theRef.get.map(_ > 0).repeatUntil(identity)
      _ <- theRef.update(_ - 1)
    } yield ())

    def release: ZIO[Any, Nothing, Unit] = theRef.update(_ + 1)
  })

  def unlimited: ZLayer[Any, Nothing, FileDownloadSharedResource] = ZLayer.succeed(new FileDownloadSharedResource {
    def acquire: ZIO[Any, Nothing, Unit] = ZIO.unit
    def release: ZIO[Any, Nothing, Unit] = ZIO.unit
  })

}
