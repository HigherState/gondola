package gondola.std

import cats._
import cats.data._
import gondola.{StateTransformation, ~>}

trait StateMonad {
  implicit def stateMonad[S]:MonadState[State[S, ?], S] =
    cats.data.StateT.catsDataMonadStateForStateT[Eval, S]
}

object StateMonads
  extends StateMonad
  with IdMonad

trait StateTransformationOps {
  def toStateTransform[M[_], N[_], S](implicit T:M ~> N, M:Monad[N]): M ~> StateT[N, S, ?] =
    new (M ~> StateT[N, S, ?]) {
      def apply[A](fa: M[A]): StateT[N, S, A] =
        StateT[N, S, A](s => M.map(T(fa))(s -> _))
    }

  def fromStateTransform[M[_], N[_], S](implicit T:M ~> N, M:Monad[M], N:Monad[N]): StateT[M, S, ?] ~> StateT[N, S, ?] =
    new (StateT[M, S, ?] ~> StateT[N, S, ?]) {
      def apply[A](fa: StateT[M, S, A]): StateT[N, S, A] =
        fa.transformF[N, A](T.apply)(M, N)
    }
}

object StateTransformationOps extends StateTransformationOps

trait StateTransformations {

  implicit def id2State[S](implicit M:Monad[State[S, ?]]): Id ~> State[S, ?] =
    IdTransformationOps.fromIdentity[State[S, ?]](M)

  implicit def state2State[S]: State[S, ?] ~> State[S, ?] =
    IdTransformationOps.identity[State[S, ?]]

  implicit def state2Id[S]: StateTransformation[State[S, ?], Id, S] =
    new StateTransformation[State[S, ?], Id, S] {
      def apply[A](fa: State[S, A], s: S): Id[(S, A)] =
        fa.run(s).value
    }
}

object StateTransformations
  extends StateTransformations
  with IdTransformations
  with StateMonad
  with IdMonad


trait StateErrorMonad  {

  implicit def stateErrorMonad[S, E](implicit M:MonadError[Error[E, ?], E]):MonadState[StateError[S, E, ?], S] with MonadError[StateError[S, E, ?], E] =
    new MonadState[StateError[S, E, ?], S] with MonadError[StateError[S, E, ?], E] {

      private val inst = StateT.catsDataMonadForStateT[Error[E, ?], S]

      def get: StateError[S, E, S] =
        StateT[Error[E, ?], S, S](s => M.pure(s -> s))

      def set(s: S): StateError[S, E, Unit] =
        StateT[Error[E, ?], S, Unit](_ => M.pure((s, ())))

      def handleErrorWith[A](fa: StateError[S, E, A])(f: (E) => StateError[S, E, A]): StateError[S, E, A] =
        StateT[Error[E, ?], S, A](s => M.handleErrorWith(fa.run(s))(e => f(e).run(s)))

      def raiseError[A](e: E): StateError[S, E, A] =
        StateT[Error[E, ?], S, A](_ => M.raiseError[(S, A)](e))

      def pure[A](x: A): StateError[S, E, A] =
        StateT.pure[Error[E, ?], S, A](x)(M)

      def flatMap[A, B](fa: StateError[S, E, A])(f: (A) => StateError[S, E, B]): StateError[S, E, B] =
        fa.flatMap(f)(M)

      def tailRecM[A, B](a: A)(f: (A) => StateError[S, E, Either[A, B]]): StateError[S, E, B] =
        inst.tailRecM(a)(f)
    }
}

object StateErrorMonads
  extends StateErrorMonad
  with StateMonad
  with ErrorMonad
  with IdMonad

trait StateErrorTransformations {

  implicit def id2StateError[S,E](implicit M:Monad[StateError[S, E, ?]]):Id ~> StateError[S, E, ?] =
    IdTransformationOps.fromIdentity[StateError[S, E, ?]](M)

  implicit def stateError2StateError[S, E]:StateError[S, E, ?] ~> StateError[S, E, ?] =
    IdTransformationOps.identity[StateError[S, E, ?]]

  implicit def state2StateError[S, E](implicit T:Eval ~> Error[E, ?], M:Monad[Eval], N:Monad[Error[E, ?]]):State[S, ?] ~> StateError[S, E, ?] =
    StateTransformationOps.fromStateTransform[Eval, Error[E, ?], S](T, M, N)

  implicit def error2StateError[S, E](implicit T:Error[E, ?] ~> Error[E, ?], N:Monad[Error[E, ?]]):Error[E, ?] ~> StateError[S, E, ?] =
    StateTransformationOps.toStateTransform[Error[E, ?], Error[E, ?], S](T, N)

  implicit def stateError2Error[S, E](implicit M:Monad[Error[E, ?]]): StateTransformation[StateError[S, E, ?], Error[E, ?], S] =
    new StateTransformation[StateError[S, E, ?], Error[E, ?], S] {
      def apply[A](fa: StateError[S, E, A], s: S): Error[E, (S, A)] =
        fa.run(s)(M)
    }
}

object StateErrorTransformations
  extends StateErrorTransformations
  with ErrorTransformations
  with StateTransformations
  with IdTransformations
  with StateErrorMonad
  with StateMonad
  with ErrorMonad
  with IdMonad


trait ReaderStateTransformations {

  implicit def reader2state[S]:Reader[S, ?] ~> State[S, ?] =
    new (Reader[S, ?] ~> State[S, ?]) {
     def apply[A](fa: Reader[S, A]): State[S, A] =
       State[S, A](s =>
        s -> fa.run(s)
       )
    }

  implicit def readerError2stateError[S, E](implicit A:Applicative[Error[E, ?]]):ReaderError[S, E, ?] ~> StateError[S, E, ?] =
    new (ReaderError[S, E, ?] ~> StateError[S, E, ?]) {

      def apply[A](fa: ReaderError[S, E, A]): StateError[S, E, A] =
        StateT[Error[E, ?], S, A](s =>
          fa.run(s).map(s -> _)
        )(A)
    }

  implicit def readerWriter2writerState[S, W]:ReaderWriter[S, W, ?] ~> WriterState[W, S, ?] =
    new (ReaderWriter[S, W, ?] ~> WriterState[W, S, ?]) {
      def apply[A](fa: ReaderWriter[S, W, A]): WriterState[W, S, A] =
        WriterT[State[S, ?], W, A]{
          State[S, (W, A)](s => s -> fa.run(s).run)
        }
    }

  implicit def readerWriterError2writerStateError[S, W, E](implicit A:Applicative[Error[E, ?]]):ReaderWriterError[S, W, E, ?] ~> WriterStateError[W, S, E, ?] =
    new (ReaderWriterError[S, W, E, ?] ~> WriterStateError[W, S, E, ?]) {
      def apply[A](fa: ReaderWriterError[S, W, E, A]): WriterStateError[W, S, E, A] = {
        WriterT[StateError[S, E, ?], W, A]{
          StateT[Error[E, ?], S, (W, A)](s => fa.run(s).run.map(s -> _))(A)
        }
      }
    }
}

object ReaderStateTransformations
  extends ReaderStateTransformations
