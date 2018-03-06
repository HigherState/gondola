package gondola.std

import cats.{Monad, MonadError}

import scala.concurrent.Future

trait MonadAsync[F[_]] extends Monad[F] {

  def liftFuture[A](f:Future[A]):F[A]
}

trait MonadFuture[F[_]] extends MonadAsync[F] with MonadError[F, Throwable]