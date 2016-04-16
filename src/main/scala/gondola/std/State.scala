package gondola.std

import cats._
import cats.data._


trait StateMonads[S] {
  implicit val stateMonad:MonadState[State[S, ?], S] =
    cats.data.StateT.stateTMonadState[Eval, S]
}

trait StateTransforms[S] extends StateMonads[S] with IdTransforms {

  def toStateTransform[M[_], N[_]](implicit transform:M ~> N, monad:Monad[N]): M ~> StateT[N, S, ?] =
    new (M ~> StateT[N, S, ?]) {
      def apply[A](fa: M[A]): StateT[N, S, A] =
        StateT[N, S, A](s => monad.map(transform(fa))(s -> _))
    }

  def fromStateTransform[M[_], N[_]](implicit transform:M ~> N, monadM:Monad[M], monadN:Monad[N]): StateT[M, S, ?] ~> StateT[N, S, ?] =
    new (StateT[M, S, ?] ~> StateT[N, S, ?]) {
      def apply[A](fa: StateT[M, S, A]): StateT[N, S, A] =
        fa.transformF[N, A](transform.apply)
    }

  implicit val id2State: Id ~> State[S, ?] =
    fromIdentity[State[S, ?]]

  implicit val state2State: Reader[S, ?] ~> Reader[S, ?] =
    identity[Reader[S, ?]]
}



trait StateValidMonads[S, E] extends StateMonads[S] with ValidMonads[E] {

  implicit val stateValidMonad:MonadState[StateValid[S, E, ?], S] with MonadError[StateValid[S, E, ?], E] =
    new MonadState[StateValid[S, E, ?], S] with MonadError[StateValid[S, E, ?], E] {
      def get: StateValid[S, E, S] =
        StateT[Valid[E, ?], S, S](s => validMonad.pure(s -> s))

      def set(s: S): StateValid[S, E, Unit] =
        StateT[Valid[E, ?], S, Unit](_ => validMonad.pure((s, ())))

      def handleErrorWith[A](fa: StateValid[S, E, A])(f: (E) => StateValid[S, E, A]): StateValid[S, E, A] =
        StateT[Valid[E, ?], S, A](s => validMonad.handleErrorWith(fa.run(s))(e => f(e).run(s)))

      def raiseError[A](e: E): StateValid[S, E, A] =
        StateT[Valid[E, ?], S, A](_ => validMonad.raiseError[(S, A)](e))

      def pure[A](x: A): StateValid[S, E, A] =
        StateT.pure[Valid[E, ?], S, A](x)

      def flatMap[A, B](fa: StateValid[S, E, A])(f: (A) => StateValid[S, E, B]): StateValid[S, E, B] =
        fa.flatMap(f)
    }
}

trait StateValidTransforms[S, E] extends StateTransforms[S] with StateValidMonads[S, E] with ValidTransforms[E] {

  implicit val id2StateValid:Id ~> StateValid[S, E, ?] =
    fromIdentity[StateValid[S, E, ?]]

  implicit val stateValid2StateValid:StateValid[S, E, ?] ~> StateValid[S, E, ?] =
    identity[StateValid[S, E, ?]]

  implicit val state2StateValid:State[S, ?] ~> StateValid[S, E, ?] =
    fromStateTransform[Eval, Valid[E, ?]]

  implicit val valid2StateValid:Valid[E, ?] ~> StateValid[S, E, ?] =
    toStateTransform[Valid[E, ?], Valid[E, ?]]

}


//trait StateWriterMonads[S, W] extends StateMonads[S] with WriterMonads[W] {
//
//  implicit val stateWriterMonad:MonadState[StateWriter[S, W, ?], S] with MonadWriter[StateWriter[S, W, ?], W] =
//    new MonadState[StateWriter[S, W, ?], S] with MonadWriter[StateWriter[S, W, ?], W] {
//      def get: StateWriter[S, W, S] =
//        StateT(a => F.pure((a, a)))
//
//      def set(s: S): StateWriter[S, W, Unit] = ???
//
//      def writer[A](aw: (W, A)): StateWriter[S, W, A] =
//        StateT[Writer[W, ?], S, A](_ => writerMonad.writer(aw))
//
//      def listen[A](fa: StateWriter[S, W, A]): StateWriter[S, W, (W, A)] = ???
//
//      def pass[A](fa: StateWriter[S, W, ((W) => W, A)]): StateWriter[S, W, A] = ???
//
//      def pure[A](x: A): StateWriter[S, W, A] =
//        StateT.pure(x)
//
//      def flatMap[A, B](fa: StateWriter[S, W, A])(f: (A) => StateWriter[S, W, B]): StateWriter[S, W, B] =
//        fa.flatMap(f)
//    }
//}
