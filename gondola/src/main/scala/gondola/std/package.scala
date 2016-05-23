package gondola

import cats.data.{ReaderT, WriterT, XorT}

import scala.concurrent.Future

package object std {

  type Id[A] = A

  type Valid[E, A] = XorT[Id, E, A]

  type State[S, A] = cats.data.State[S, A]

  type StateValid[S, E, A] = cats.data.StateT[Valid[E, ?], S, A]

  type Writer[W, A] = WriterT[Id, W, A]

  type WriterValid[W, E, A] = WriterT[Valid[E, ?], W, A]

  type WriterState[W, S, A] = WriterT[State[S, ?], W, A]

  type WriterStateValid[W, S, E, A] = WriterT[StateValid[S, E, ?], W, A]

  type Reader[R, A] = cats.data.Reader[R, A]

  type ReaderValid[R, E, A] = ReaderT[Valid[E, ?], R, A]

  type ReaderWriter[R, W, A] = ReaderT[Writer[W, ?], R, A]

  type ReaderWriterValid[R, W, E, A] = ReaderT[WriterT[Valid[E, ?], W, ?], R, A]

  type IO[A] = cats.data.Reader[Unit, A]

  type IOT[M[_], A] = ReaderT[M, Unit, A]

  type IOValid[E, A] = ReaderValid[Unit, E, A]

  type IOWriter[W, A] = ReaderWriter[Unit, W, A]

  type IOWriterValid[W, E, A] = ReaderWriterValid[Unit, W, E, A]

  type FutureValid[E, A] = FutureT[Valid[E, ?], A]

  type FutureWriter[W, A] = FutureT[Writer[W, ?], A]

  type FutureWriterValid[W, E, A] = FutureT[WriterValid[W, E, ?], A]

  type ReaderFuture[R, A] = ReaderT[Future, R, A]

  type ReaderFutureValid[R, E, A] = ReaderT[FutureValid[E, ?], R, A]



}
