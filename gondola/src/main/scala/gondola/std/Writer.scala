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
  def toWriterTransformation[M[_], N[_], W](N:Monad[N], W:Monoid[W], transform:M ~> N): M ~> WriterT[N, W, ?] =
    new (M ~> WriterT[N, W, ?]) {
      def apply[A](fa: M[A]): WriterT[N, W, A] =
        cats.data.WriterT[N, W, A](N.map(transform(fa))(W.empty -> _))
    }

  def fromWriterTransformation[M[_], N[_], W](T:M ~> N): WriterT[M, W, ?] ~> WriterT[N, W, ?] =
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



trait WriterValidMonad {

  implicit def writerValidMonad[W, E](implicit ME:MonadError[Valid[E, ?], E], W:Monoid[W]) =
    new MonadError[WriterValid[W, E, ?], E] with MonadWriter[WriterValid[W, E, ?], W]{

      def raiseError[A](e: E): WriterValid[W, E, A] =
        cats.data.WriterT[Valid[E,?],W,A](ME.raiseError[(W,A)](e))

      def handleErrorWith[A](fa: WriterValid[W, E, A])(f: (E) => WriterValid[W, E, A]): WriterValid[W, E, A] =
        cats.data.WriterT[Valid[E,?],W,A](ME.handleErrorWith(fa.run)(e => f(e).run))

      def pure[A](x: A): WriterValid[W, E, A] =
        WriterT.value[Valid[E, ?],W,A](x)

      def flatMap[A, B](fa: WriterValid[W, E, A])(f: (A) => WriterValid[W, E, B]): WriterValid[W, E, B] =
        fa.flatMap(f)

      def writer[A](aw: (W, A)): WriterValid[W, E, A] =
        cats.data.WriterT[Valid[E,?],W,A](ME.pure(aw))

      def listen[A](fa: WriterValid[W, E, A]): WriterValid[W, E, (W, A)] =
        cats.data.WriterT[Valid[E,?],W,(W, A)](fa.run.map(W.empty -> _))

      def pass[A](fa: WriterValid[W, E, ((W) => W, A)]): WriterValid[W, E, A] =
        cats.data.WriterT[Valid[E,?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})
    }

  implicit def writerValidTraverse[W, E](implicit MW:MonadWriter[WriterValid[W, E, ?], W]):Traverse[WriterValid[W, E, ?]] =
    new Traverse[WriterValid[W, E, ?]] {

      def traverse[G[_], A, B](fa: WriterValid[W, E, A])(f: (A) => G[B])(implicit A: Applicative[G]): G[WriterValid[W, E, B]] =
        fa.run.value match {
          case Xor.Left(_) =>
            A.pure(fa.asInstanceOf[WriterValid[W, E, B]])
          case Xor.Right((w, a)) =>
            A.map(f(a))(r => MW.writer(w -> r))
        }

      def foldLeft[A, B](fa: WriterValid[W, E, A], b: B)(f: (B, A) => B): B =
        fa.run.foldLeft(b)( (c, p) => f(c, p._2))

      def foldRight[A, B](fa: WriterValid[W, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.run.foldRight(lb)( (c, p) => f(c._2, p))
    }
}

object WriterValidMonads
  extends WriterValidMonad
  with WriterMonad
  with ValidMonad
  with IdMonad



trait WriterValidTransformations  {

  implicit def id2WriterValid[W, E](M:Monad[WriterValid[W, E, ?]]): Id ~> WriterValid[W, E, ?] =
    IdTransformationOps.fromIdentity[WriterValid[W, E, ?]](M)

  implicit def writerValid2WriterValid[W,E]:WriterValid[W, E, ?] ~> WriterValid[W, E, ?] =
    IdTransformationOps.identity[WriterValid[W, E, ?]]

  implicit def writer2writerValid[W, E](implicit T:Id ~> Valid[E, ?]):Writer[W, ?] ~> WriterValid[W, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Id, Valid[E, ?], W](T)

  implicit def valid2writerValid[W, E](implicit N:Monad[Valid[E, ?]], W:Monoid[W], T:Valid[E, ?] ~> Valid[E, ?]):Valid[E, ?] ~> WriterValid[W, E, ?] =
    WriterTransformationOps.toWriterTransformation[Valid[E, ?], Valid[E, ?], W](N, W, T)

  implicit def writerValid2Valid[W, E]: WriterTransformation[WriterValid[W, E, ?], Valid[E, ?], W] =
    WriterTransformationOps.dropWriter[Valid[E, ?], W]
}

object WriterValidTransformations
  extends WriterValidTransformations
  with WriterTransformations
  with ValidTransformations
  with IdTransformations
  with WriterValidMonad
  with WriterMonad
  with ValidMonad
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

  implicit def valid2WriterState[W, S](implicit N:Monad[State[S, ?]], W:Monoid[W], T: State[S, ?] ~> State[S, ?]):State[S, ?] ~> WriterState[W, S, ?] =
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

trait WriterStateValidMonad {

  implicit def writerStateValidMonads[W, S, E](implicit MSE:MonadState[StateValid[S, E, ?], S] with MonadError[StateValid[S, E, ?], E], W:Monoid[W], M:Monad[Valid[E, ?]])
    :MonadState[WriterStateValid[W, S, E, ?], S] with MonadWriter[WriterStateValid[W, S, E, ?], W] with MonadError[WriterStateValid[W, S, E, ?], E] =
    new MonadState[WriterStateValid[W, S, E, ?], S] with MonadWriter[WriterStateValid[W, S, E, ?], W] with MonadError[WriterStateValid[W, S, E, ?], E] {

      def get: WriterStateValid[W, S, E, S] =
        cats.data.WriterT[StateValid[S, E, ?],W,S](MSE.get.map(W.empty -> _)(M))

      def set(s: S): WriterStateValid[W, S, E, Unit] =
        cats.data.WriterT[StateValid[S, E, ?], W, Unit](MSE.set(s).map(W.empty -> _)(M))

      def listen[A](fa: WriterStateValid[W, S, E, A]): WriterStateValid[W, S, E, (W, A)] =
        cats.data.WriterT[StateValid[S, E, ?],W,(W, A)](fa.run.map(W.empty -> _)(M))

      def writer[A](aw: (W, A)): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W,A](MSE.pure(aw))

      def pass[A](fa: WriterStateValid[W, S, E, ((W) => W, A)]): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a}(M))

      def handleErrorWith[A](fa: WriterStateValid[W, S, E, A])(f: (E) => WriterStateValid[W, S, E, A]): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E,?],W,A](MSE.handleErrorWith(fa.run)(e => f(e).run))

      def raiseError[A](e: E): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W,A](MSE.raiseError[(W,A)](e))

      def flatMap[A, B](fa: WriterStateValid[W, S, E, A])(f: (A) => WriterStateValid[W, S, E, B]): WriterStateValid[W, S, E, B] =
        fa.flatMap(f)

      def pure[A](x: A): WriterStateValid[W, S, E, A] =
        WriterT.value[StateValid[S, E, ?],W,A](x)
    }
}

object WriterStateValidMonads
  extends WriterStateValidMonad
  with WriterStateMonad
  with WriterValidMonad
  with StateValidMonad
  with WriterMonad
  with StateMonad
  with ValidMonad
  with IdMonad

trait WriterStateValidTransformations {

  implicit def id2WriterStateValid[W, S, E](N:Monad[StateValid[S, E, ?]], W:Monoid[W], T:Id ~> StateValid[S, E, ?]):Id ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[Id, StateValid[S, E, ?], W](N, W, T)

  implicit def writer2WriterStateValid[W, S, E](T:Id ~> StateValid[S, E, ?]):Writer[W, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Id, StateValid[S, E, ?], W](T)

  implicit def stateValid2WriterStateValid[W, S, E](N:Monad[StateValid[S, E, ?]], W:Monoid[W], T:StateValid[S, E, ?] ~> StateValid[S, E, ?]):StateValid[S, E, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[StateValid[S, E, ?], StateValid[S, E, ?], W](N, W, T)

  implicit def state2WriterStateValid[W, S, E](N:Monad[StateValid[S, E, ?]], W:Monoid[W], T:State[S, ?] ~> StateValid[S, E, ?]):State[S, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[State[S, ?], StateValid[S, E, ?], W](N, W, T)

  implicit def valid2WriterStateValid[W, S, E](N:Monad[StateValid[S, E, ?]], W:Monoid[W], T:Valid[E, ?] ~> StateValid[S, E, ?]):Valid[E, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.toWriterTransformation[Valid[E, ?], StateValid[S, E, ?], W](N, W, T)

  implicit def writerValid2WriterStateValid[W, S, E](T:Valid[E, ?] ~> StateValid[S, E, ?]):WriterValid[W, E, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[Valid[E, ?], StateValid[S, E, ?], W](T)

  implicit def writerState2WriterStateValid[W, S, E](T:State[S, ?] ~> StateValid[S, E, ?]):WriterState[W, S, ?] ~> WriterStateValid[W, S, E, ?] =
    WriterTransformationOps.fromWriterTransformation[State[S, ?], StateValid[S, E, ?], W](T)

  implicit def writerStateValid2WriterStateValid[W, S, E]:WriterStateValid[W, S, E, ?] ~> WriterStateValid[W, S, E, ?] =
    IdTransformationOps.identity[WriterStateValid[W, S, E, ?]]

  implicit def writerStateValid2StateValid[W,S,E](M:Monad[Valid[E, ?]]): StateTransformation[WriterStateValid[W, S, E, ?], WriterValid[W, E, ?], S] =
    new StateTransformation[WriterStateValid[W, S, E, ?], WriterValid[W, E, ?], S] {
      def apply[A](fa: WriterStateValid[W, S, E, A], s: S): WriterValid[W, E, (S, A)] =
        WriterT[Valid[E, ?], W, (S, A)](fa.run.run(s)(M).map(f => f._2._1 -> (f._1 -> f._2._2))(IdMonads.idMonad))
    }

  implicit def writerValidState2ValidState[W,S,E]:WriterTransformation[WriterStateValid[W, S, E, ?], StateValid[S, E, ?], W] =
    WriterTransformationOps.dropWriter[StateValid[S, E, ?], W]

}

object WriterStateValidTransformations
  extends WriterStateValidTransformations
  with WriterStateTransformations
  with WriterValidTransformations
  with StateValidTransformations
  with WriterTransformations
  with StateTransformations
  with ValidTransformations
  with IdTransformations
  with WriterStateValidMonad
  with WriterStateMonad
  with WriterValidMonad
  with StateValidMonad
  with WriterMonad
  with StateMonad
  with ValidMonad
  with IdMonad
