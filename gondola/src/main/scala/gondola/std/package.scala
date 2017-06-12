package gondola

import cats.data.{ReaderT, WriterT}

import scala.concurrent.Future

package object std {

  type Id[A] = A

  type Error[E, A] = Either[E, A]

  type State[S, A] = cats.data.State[S, A]

  type StateError[S, E, A] = cats.data.StateT[Error[E, ?], S, A]

  type Writer[W, A] = WriterT[Id, W, A]

  type WriterError[W, E, A] = WriterT[Error[E, ?], W, A]

  type WriterState[W, S, A] = WriterT[State[S, ?], W, A]

  type WriterStateError[W, S, E, A] = WriterT[StateError[S, E, ?], W, A]

  type Reader[R, A] = cats.data.Reader[R, A]

  type ReaderError[R, E, A] = ReaderT[Error[E, ?], R, A]

  type ReaderWriter[R, W, A] = ReaderT[Writer[W, ?], R, A]

  type ReaderWriterError[R, W, E, A] = ReaderT[WriterT[Error[E, ?], W, ?], R, A]

  type IO[A] = cats.data.Reader[Unit, A]

  type IOT[M[_], A] = ReaderT[M, Unit, A]

  type IOError[E, A] = ReaderError[Unit, E, A]

  type IOWriter[W, A] = ReaderWriter[Unit, W, A]

  type IOWriterError[W, E, A] = ReaderWriterError[Unit, W, E, A]

  type FutureError[E, A] = FutureT[Error[E, ?], A]

  type FutureWriter[W, A] = FutureT[Writer[W, ?], A]

  type FutureWriterError[W, E, A] = FutureT[WriterError[W, E, ?], A]

  type ReaderFuture[R, A] = ReaderT[Future, R, A]

  type ReaderFutureError[R, E, A] = ReaderT[FutureError[E, ?], R, A]

  type ReaderFutureWriter[R, W, A] = ReaderT[FutureWriter[W, ?], R, A]

  type ReaderFutureWriterError[R, W, E, A] = ReaderT[FutureWriterError[W, E, ?], R, A]



}
