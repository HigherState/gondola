package gondola.std

import cats._
import cats.data.{WriterT, Xor}
import gondola.{StateTransformation, WriterTransformation, ~>}

object Writer {
  def apply[W, A](a:A)(implicit monoid:Monoid[W]) =
    cats.data.Writer(monoid.empty, a)
  def apply[W, A](l:W, a:A) =
    cats.data.Writer(l, a)
}

trait WriterMonad {

  implicit def writerMonad[W:Monoid] = new MonadWriter[Writer[W, ?], W] {

    def flatMap[A, B](fa: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] =
      fa.flatMap(f)

    def writer[A](aw: (W, A)): Writer[W, A] =
      cats.data.WriterT[Id, W, A](aw)

    def listen[A](fa: Writer[W, A]): Writer[W, (W, A)] =
      pure(fa.run)

    def pass[A](fa: Writer[W, ((W) => W, A)]): Writer[W, A] = {
      val (l, (f,a)) = fa.run
      writer(f(l) -> a)
    }

    def pure[A](x: A): Writer[W, A] =
      WriterT.value[Id,W,A](x)
  }

  implicit def writerTraverse[W:Monoid]:Traverse[Writer[W, ?]] = new Traverse[Writer[W, ?]] {
    def traverse[G[_], A, B](fa: Writer[W, A])(f: (A) => G[B])(implicit A: Applicative[G]): G[Writer[W, B]] = {
      val (w, a) = fa.run
      A.map(f(a))(a => writerMonad.writer(w -> a))
    }

    def foldLeft[A, B](fa: Writer[W, A], b: B)(f: (B, A) => B): B =
      f(b, fa.run._2)

    def foldRight[A, B](fa: Writer[W, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.run._2, lb)
  }
}

object WriterMonads
  extends WriterMonad
  with IdMonad

trait WriterTransformationOps {
  def toWriterTransformation[M[_], N[_], W](implicit N:Monad[N], W:Monoid[W], transform:M ~> N): M ~> WriterT[N, W, ?] =
    new (M ~> WriterT[N, W, ?]) {
      def apply[A](fa: M[A]): WriterT[N, W, A] =
        cats.data.WriterT[N, W, A](N.map(transform(fa))(W.empty -> _))
    }

  def fromWriterTransformation[M[_], N[_], W](implicit T:M ~> N): WriterT[M, W, ?] ~> WriterT[N, W, ?] =
    new (WriterT[M, W, ?] ~> WriterT[N, W, ?]) {
      def apply[A](fa: WriterT[M, W, A]): WriterT[N, W, A] =
        cats.data.WriterT[N,W,A](T(fa.run))
    }

  def dropWriter[M[_], W]:WriterTransformation[WriterT[M, W, ?], M, W] =
    new WriterTransformation[WriterT[M, W, ?], M, W] {
      def apply[A](fa: WriterT[M, W, A]): M[(W, A)] =
        fa.run
    }
}

object WriterTransformationOps extends WriterTransformationOps

trait WriterTransformations extends IdTransformations {

  implicit def id2Writer[W](implicit M:Monad[Writer[W, ?]]): Id ~> Writer[W, ?] =
    IdTransformationOps.fromIdentity[Writer[W, ?]](M)

  implicit def writer2Writer[W]: Writer[W, ?] ~> Writer[W, ?] =
    IdTransformationOps.identity[Writer[W, ?]]

  implicit def writer2id[W]: WriterTransformation[Writer[W, ?], Id, W] =
    WriterTransformationOps.dropWriter[Id, W]
}

object WriterTransformations
  extends WriterTransformations
  with IdTransformations
  with WriterMonad
  with IdMonad



trait WriterErrorMonad {

  implicit def writerErrorMonad[W, E](implicit ME:MonadError[Error[E, ?], E], W:Monoid[W]) =
    new MonadError[WriterError[W, E, ?], E] with MonadWriter[WriterError[W, E, ?], W]{

      def raiseError[A](e: E): WriterError[W, E, A] =
        cats.data.WriterT[Error[E,?],W,A](ME.raiseError[(W,A)](e))

      def handleErrorWith[A](fa: WriterError[W, E, A])(f: (E) => WriterError[W, E, A]): WriterError[W, E, A] =
        cats.data.WriterT[Error[E,?],W,A](ME.handleErrorWith(fa.run)(e => f(e).run))

      def pure[A](x: A): WriterError[W, E, A] =
        WriterT.value[Error[E, ?],W,A](x)

      def flatMap[A, B](fa: WriterError[W, E, A])(f: (A) => WriterError[W, E, B]): WriterError[W, E, B] =
        fa.flatMap(f)

      def writer[A](aw: (W, A)): WriterError[W, E, A] =
        cats.data.WriterT[Error[E,?],W,A](ME.pure(aw))

      def listen[A](fa: WriterError[W, E, A]): WriterError[W, E, (W, A)] =
        cats.data.WriterT[Error[E,?],W,(W, A)](fa.run.map(W.empty -> _))

      def pass[A](fa: WriterError[W, E, ((W) => W, A)]): WriterError[W, E, A] =
        cats.data.WriterT[Error[E,?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})
    }

  implicit def writerErrorTraverse[W, E](implicit MW:MonadWriter[WriterError[W, E, ?], W]):Traverse[WriterError[W, E, ?]] =
    new Traverse[WriterError[W, E, ?]] {

      def traverse[G[_], A, B](fa: WriterError[W, E, A])(f: (A) => G[B])(implicit A: Applicative[G]): G[WriterError[W, E, B]] =
        fa.run match {
          case Xor.Left(_) =>
            A.pure(fa.asInstanceOf[WriterError[W, E, B]])
          case Xor.Right((w, a)) =>
            A.map(f(a))(r => MW.writer(w -> r))
        }

      def foldLeft[A, B](fa: WriterError[W, E, A], b: B)(f: (B, A) => B): B =
        fa.run.foldLeft(b)( (c, p) => f(c, p._2))

      def foldRight[A, B](fa: WriterError[W, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.run.foldRight(lb)( (c, p) => f(c._2, p))
    }
}

object WriterErrorMonads
  extends WriterErrorMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad



trait WriterErrorTransformations  {

  implicit def id2WriterError[W, E](implicit M:Monad[WriterError[W, E, ?]]): Id ~> WriterError[W, E, ?] =
    IdTransformationOps.fromIdentity[WriterError[W, E, ?]](M)

  implicit def writerError2WriterError[W,E]:WriterError[W, E, ?] ~> WriterError[W, E, ?] =
    IdTransformationOps.identity[WriterError[W, E, ?]]

  implicit def writer2writerError[W, E](implicit T:Id ~> Error[E, ?]):Writer[W, ?] ~> WriterError[W, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Id, Error[E, ?], W](T)

  implicit def error2writerError[W, E](implicit N:Monad[Error[E, ?]], W:Monoid[W], T:Error[E, ?] ~> Error[E, ?]):Error[E, ?] ~> WriterError[W, E, ?] =
    WriterTransformationOps.toWriterTransformation[Error[E, ?], Error[E, ?], W](N, W, T)

  implicit def writerError2Error[W, E]: WriterTransformation[WriterError[W, E, ?], Error[E, ?], W] =
    WriterTransformationOps.dropWriter[Error[E, ?], W]
}

object WriterErrorTransformations
  extends WriterErrorTransformations
  with WriterTransformations
  with ErrorTransformations
  with IdTransformations
  with WriterErrorMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad

trait WriterStateMonad {

  implicit def writerStateMonad[W, S](implicit MS:MonadState[State[S, ?], S], W:Monoid[W]):MonadState[WriterState[W, S, ?], S] with MonadWriter[WriterState[W, S, ?], W] =
    new MonadState[WriterState[W, S, ?], S] with MonadWriter[WriterState[W, S, ?], W] {

      def writer[A](aw: (W, A)): WriterState[W, S, A] =
        cats.data.WriterT[State[S,?],W,A](MS.pure(aw))

      def listen[A](fa: WriterState[W, S, A]): WriterState[W, S, (W, A)] =
        cats.data.WriterT[State[S,?],W,(W, A)](fa.run.map(W.empty -> _))

      def pass[A](fa: WriterState[W, S, ((W) => W, A)]): WriterState[W, S, A] =
        cats.data.WriterT[State[S,?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})

      def set(s: S): WriterState[W, S, Unit] =
        cats.data.WriterT[State[S, ?], W, Unit](MS.set(s).map(W.empty -> _))

      def get: WriterState[W, S, S] =
        cats.data.WriterT[State[S, ?],W,S](MS.get.map(W.empty -> _))

      def pure[A](x: A): WriterState[W, S, A] =
        WriterT.value[State[S, ?], W, A](x)

      def flatMap[A, B](fa: WriterState[W, S, A])(f: (A) => WriterState[W, S, B]): WriterState[W, S, B] =
        fa.flatMap(f)
    }
}

object WriterStateMonads
  extends WriterStateMonad
  with WriterMonad
  with StateMonad
  with IdMonad

trait WriterStateTransformations  {

  implicit def id2StateWriter[W, S](implicit M:Monad[WriterState[W, S, ?]]): Id ~> WriterState[W, S, ?] =
    IdTransformationOps.fromIdentity[WriterState[W, S, ?]]

  implicit def writerState2WriterState[W, S]:WriterState[W, S, ?] ~> WriterState[W, S, ?] =
    IdTransformationOps.identity[WriterState[W, S, ?]]

  implicit def writer2writerState[W, S](implicit T: Id ~> State[S, ?]):Writer[W, ?] ~> WriterState[W, S, ?] =
    WriterTransformationOps.fromWriterTransformation[Id, State[S, ?], W](T)

  implicit def error2WriterState[W, S](implicit N:Monad[State[S, ?]], W:Monoid[W], T: State[S, ?] ~> State[S, ?]):State[S, ?] ~> WriterState[W, S, ?] =
    WriterTransformationOps.toWriterTransformation[State[S, ?], State[S, ?], W](N, W, T)

  implicit def writerState2Writer[W, S](implicit MW:MonadWriter[Writer[W, ?], W]): StateTransformation[WriterState[W, S, ?], Writer[W, ?], S] =
    new StateTransformation[WriterState[W, S, ?], Writer[W, ?], S] {
      def apply[A](fa: WriterState[W, S, A], s: S): Writer[W, (S, A)] = {
        val (ns, (w, a)) = fa.run.run(s).value
        MW.writer(w -> (ns -> a))
      }
    }

  implicit def writerState2State[W, S]:WriterTransformation[WriterState[W, S, ?], State[S, ?], W] =
    WriterTransformationOps.dropWriter[State[S, ?], W]
}

object WriterStateTransformations
  extends WriterStateTransformations
  with WriterTransformations
  with StateTransformations
  with IdTransformations
  with WriterStateMonad
  with WriterMonad
  with StateMonad

trait WriterStateErrorMonad {

  implicit def writerStateErrorMonads[W, S, E](implicit MSE:MonadState[StateError[S, E, ?], S] with MonadError[StateError[S, E, ?], E], W:Monoid[W], M:Monad[Error[E, ?]])
    :MonadState[WriterStateError[W, S, E, ?], S] with MonadWriter[WriterStateError[W, S, E, ?], W] with MonadError[WriterStateError[W, S, E, ?], E] =
    new MonadState[WriterStateError[W, S, E, ?], S] with MonadWriter[WriterStateError[W, S, E, ?], W] with MonadError[WriterStateError[W, S, E, ?], E] {

      def get: WriterStateError[W, S, E, S] =
        cats.data.WriterT[StateError[S, E, ?],W,S](MSE.get.map(W.empty -> _)(M))

      def set(s: S): WriterStateError[W, S, E, Unit] =
        cats.data.WriterT[StateError[S, E, ?], W, Unit](MSE.set(s).map(W.empty -> _)(M))

      def listen[A](fa: WriterStateError[W, S, E, A]): WriterStateError[W, S, E, (W, A)] =
        cats.data.WriterT[StateError[S, E, ?],W,(W, A)](fa.run.map(W.empty -> _)(M))

      def writer[A](aw: (W, A)): WriterStateError[W, S, E, A] =
        cats.data.WriterT[StateError[S, E, ?],W,A](MSE.pure(aw))

      def pass[A](fa: WriterStateError[W, S, E, ((W) => W, A)]): WriterStateError[W, S, E, A] =
        cats.data.WriterT[StateError[S, E, ?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a}(M))

      def handleErrorWith[A](fa: WriterStateError[W, S, E, A])(f: (E) => WriterStateError[W, S, E, A]): WriterStateError[W, S, E, A] =
        cats.data.WriterT[StateError[S, E,?],W,A](MSE.handleErrorWith(fa.run)(e => f(e).run))

      def raiseError[A](e: E): WriterStateError[W, S, E, A] =
        cats.data.WriterT[StateError[S, E, ?],W,A](MSE.raiseError[(W,A)](e))

      def flatMap[A, B](fa: WriterStateError[W, S, E, A])(f: (A) => WriterStateError[W, S, E, B]): WriterStateError[W, S, E, B] =
        fa.flatMap(f)

      def pure[A](x: A): WriterStateError[W, S, E, A] =
        WriterT.value[StateError[S, E, ?],W,A](x)
    }
}

object WriterStateErrorMonads
  extends WriterStateErrorMonad
  with WriterStateMonad
  with WriterErrorMonad
  with StateErrorMonad
  with WriterMonad
  with StateMonad
  with ErrorMonad
  with IdMonad

trait WriterStateErrorTransformations {

  implicit def id2WriterStateError[W, S, E](implicit N:Monad[StateError[S, E, ?]], W:Monoid[W], T:Id ~> StateError[S, E, ?]):Id ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[Id, StateError[S, E, ?], W](N, W, T)

  implicit def writer2WriterStateError[W, S, E](implicit T:Id ~> StateError[S, E, ?]):Writer[W, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Id, StateError[S, E, ?], W](T)

  implicit def stateError2WriterStateError[W, S, E](implicit N:Monad[StateError[S, E, ?]], W:Monoid[W], T:StateError[S, E, ?] ~> StateError[S, E, ?]):StateError[S, E, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[StateError[S, E, ?], StateError[S, E, ?], W](N, W, T)

  implicit def state2WriterStateError[W, S, E](implicit N:Monad[StateError[S, E, ?]], W:Monoid[W], T:State[S, ?] ~> StateError[S, E, ?]):State[S, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[State[S, ?], StateError[S, E, ?], W](N, W, T)

  implicit def error2WriterStateError[W, S, E](implicit N:Monad[StateError[S, E, ?]], W:Monoid[W], T:Error[E, ?] ~> StateError[S, E, ?]):Error[E, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[Error[E, ?], StateError[S, E, ?], W](N, W, T)

  implicit def writerError2WriterStateError[W, S, E](implicit T:Error[E, ?] ~> StateError[S, E, ?]):WriterError[W, E, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Error[E, ?], StateError[S, E, ?], W](T)

  implicit def writerState2WriterStateError[W, S, E](implicit T:State[S, ?] ~> StateError[S, E, ?]):WriterState[W, S, ?] ~> WriterStateError[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[State[S, ?], StateError[S, E, ?], W](T)

  implicit def writerStateError2WriterStateError[W, S, E]:WriterStateError[W, S, E, ?] ~> WriterStateError[W, S, E, ?] =
    IdTransformationOps.identity[WriterStateError[W, S, E, ?]]

  implicit def writerStateError2StateError[W,S,E](implicit M:Monad[Error[E, ?]]): StateTransformation[WriterStateError[W, S, E, ?], WriterError[W, E, ?], S] =
    new StateTransformation[WriterStateError[W, S, E, ?], WriterError[W, E, ?], S] {
      def apply[A](fa: WriterStateError[W, S, E, A], s: S): WriterError[W, E, (S, A)] =
        WriterT[Error[E, ?], W, (S, A)](fa.run.run(s)(M).map(f => f._2._1 -> (f._1 -> f._2._2)))
    }

  implicit def writerErrorState2ErrorState[W,S,E]:WriterTransformation[WriterStateError[W, S, E, ?], StateError[S, E, ?], W] =
    WriterTransformationOps.dropWriter[StateError[S, E, ?], W]

}

object WriterStateErrorTransformations
  extends WriterStateErrorTransformations
  with WriterStateTransformations
  with WriterErrorTransformations
  with StateErrorTransformations
  with WriterTransformations
  with StateTransformations
  with ErrorTransformations
  with IdTransformations
  with WriterStateErrorMonad
  with WriterStateMonad
  with WriterErrorMonad
  with StateErrorMonad
  with WriterMonad
  with StateMonad
  with ErrorMonad
  with IdMonad
