package gondola

import cats.data.{WriterT, ReaderT, XorT}
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





  type FutureValid[E, A] = Future[Valid[E, A]]

  type FutureWriter[W, A] = Future[Writer[W, A]]

  type ReaderFuture[R, A] = ReaderT[Future[?], R, A]

  type ReaderFutureValid[R, E, A] = ReaderT[FutureValid[E, ?], R, A]

  type FutureValidWriter[E, W, A] = Future[Valid[E, Writer[W, A]]]

}
