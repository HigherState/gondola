package gondola

import scalaz._
import scala.concurrent.Future

package object std {

  type Id[+T] = T

  type ValidE[+E] = Valid[E, _]

  type Valid[+E, +T] = ValidationNel[E, T]

  type ReaderValid[F, +E, +T] = Reader[F, Valid[E, T]]

  type EitherValid[+E, +T] = scalaz.\/[NonEmptyList[E], T]

  type FutureValid[+E, +T] = Future[ValidationNel[E, T]]

  type ReaderFuture[F, +T] = Reader[F, Future[T]]

  type ReaderFutureValid[F, +E, +T] = Reader[F, FutureValid[E, T]]

  type FutureEitherValid[+E, +T] = Future[EitherValid[E, T]]

}
