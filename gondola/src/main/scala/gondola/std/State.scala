package gondola.std

import cats._
import cats.data._
import gondola.{StateTransformation, ~>}

trait StateMonad {
  implicit def stateMonad[S]:MonadState[State[S, ?], S] =
    cats.data.StateT.stateTMonadState[Eval, S]
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


trait StateValidMonad  {

  implicit def stateValidMonad[S, E](implicit M:MonadError[Valid[E, ?], E]):MonadState[StateValid[S, E, ?], S] with MonadError[StateValid[S, E, ?], E] =
    new MonadState[StateValid[S, E, ?], S] with MonadError[StateValid[S, E, ?], E] {
      def get: StateValid[S, E, S] =
        StateT[Valid[E, ?], S, S](s => M.pure(s -> s))

      def set(s: S): StateValid[S, E, Unit] =
        StateT[Valid[E, ?], S, Unit](_ => M.pure((s, ())))

      def handleErrorWith[A](fa: StateValid[S, E, A])(f: (E) => StateValid[S, E, A]): StateValid[S, E, A] =
        StateT[Valid[E, ?], S, A](s => M.handleErrorWith(fa.run(s))(e => f(e).run(s)))

      def raiseError[A](e: E): StateValid[S, E, A] =
        StateT[Valid[E, ?], S, A](_ => M.raiseError[(S, A)](e))

      def pure[A](x: A): StateValid[S, E, A] =
        StateT.pure[Valid[E, ?], S, A](x)(M)

      def flatMap[A, B](fa: StateValid[S, E, A])(f: (A) => StateValid[S, E, B]): StateValid[S, E, B] =
        fa.flatMap(f)(M)
    }
}

object StateValidMonads
  extends StateValidMonad
  with StateMonad
  with ValidMonad
  with IdMonad

trait StateValidTransformations {

  implicit def id2StateValid[S,E](implicit M:Monad[StateValid[S, E, ?]]):Id ~> StateValid[S, E, ?] =
    IdTransformationOps.fromIdentity[StateValid[S, E, ?]](M)

  implicit def stateValid2StateValid[S, E]:StateValid[S, E, ?] ~> StateValid[S, E, ?] =
    IdTransformationOps.identity[StateValid[S, E, ?]]

  implicit def state2StateValid[S, E](implicit T:Eval ~> Valid[E, ?], M:Monad[Eval], N:Monad[Valid[E, ?]]):State[S, ?] ~> StateValid[S, E, ?] =
    StateTransformationOps.fromStateTransform[Eval, Valid[E, ?], S](T, M, N)

  implicit def valid2StateValid[S, E](implicit T:Valid[E, ?] ~> Valid[E, ?], N:Monad[Valid[E, ?]]):Valid[E, ?] ~> StateValid[S, E, ?] =
    StateTransformationOps.toStateTransform[Valid[E, ?], Valid[E, ?], S](T, N)

  implicit def state2Valid[S, E](implicit M:Monad[Valid[E, ?]]): StateTransformation[StateValid[S, E, ?], Valid[E, ?], S] =
    new StateTransformation[StateValid[S, E, ?], Valid[E, ?], S] {
      def apply[A](fa: StateValid[S, E, A], s: S): Valid[E, (S, A)] =
        fa.run(s)(M)
    }
}

object StateValidTransformations
  extends StateValidTransformations
  with ValidTransformations
  with StateTransformations
  with IdTransformations
  with StateValidMonad
  with StateMonad
  with ValidMonad
  with IdMonad
