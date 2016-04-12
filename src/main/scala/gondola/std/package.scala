package gondola

import cats.data.{Writer, Xor, NonEmptyList}
import scala.concurrent.Future

package object std {

  type Id[T] = T

  type Valid[E, T] = Xor[NonEmptyList[E], T]

  type Writer[L, T] = cats.data.WriterT[Id, L, T]

  type IOValid[E, T] = IO[Valid[E, T]]

  type IOWriter[L, T] = IO[Writer[L, T]]

  type IOValidWriter[E, L, T] = IO[Valid[E, Writer[L, T]]]

  type ReaderValid[F, E, T] = Reader[F, Valid[E, T]]

  type ValidWriter[E, L, T] = Valid[E, Writer[L, T]]

  type ReaderWriter[F, L, T] = Reader[F, Writer[L, T]]

  type ReaderValidWriter[F, E, L, T] = Reader[F, Valid[E, Writer[L, T]]]


  type FutureValid[E, T] = Future[Valid[E, T]]

  type FutureWriter[L, T] = Future[Writer[L, T]]

  type ReaderFuture[F, T] = Reader[F, Future[T]]

  type ReaderFutureValid[F, E, T] = Reader[F, FutureValid[E, T]]

  type FutureValidWriter[E, L, T] = Future[Valid[E, Writer[L, T]]]

}
