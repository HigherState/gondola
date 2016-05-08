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

trait MonadWriter[F[_], W] extends Monad[F] {

 def writer[A](aw: (W, A)): F[A]

 def listen[A](fa: F[A]): F[(W, A)]

 def pass[A](fa: F[(W => W, A)]): F[A]

 def tell(w: W): F[Unit] = writer((w, ()))

 def listens[A, B](fa: F[A])(f: W => B): F[(B, A)] =
   map(listen(fa)) { case (w, a) => (f(w), a) }

  def censor[A](fa: F[A])(f: W => W): F[A] =
    flatMap(listen(fa)) { case (w, a) => writer((f(w), a)) }
}

trait WriterMonads[W] {
  implicit def writerMonoid: Monoid[W]

  def empty = writerMonoid.empty

  implicit val writerMonad = new MonadWriter[Writer[W, ?], W] {

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

  implicit val writerTraverse:Traverse[Writer[W, ?]] = new Traverse[Writer[W, ?]] {
    def traverse[G[_], A, B](fa: Writer[W, A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[Writer[W, B]] = {
      val (w, a) = fa.run
      evidence$1.map(f(a))(a => writerMonad.writer(w -> a))
    }

    def foldLeft[A, B](fa: Writer[W, A], b: B)(f: (B, A) => B): B =
      f(b, fa.run._2)

    def foldRight[A, B](fa: Writer[W, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.run._2, lb)
  }
}

trait WriterTransforms[W] extends WriterMonads[W] with IdTransforms {

  protected def toWriterTransform[M[_], N[_]](implicit monad:Monad[N], transform:M ~> N): M ~> WriterT[N, W, ?] =
    new (M ~> WriterT[N, W, ?]) {
      def apply[A](fa: M[A]): WriterT[N, W, A] =
        cats.data.WriterT[N, W, A](monad.map(transform(fa))(empty -> _))
    }

  protected def fromWriterTransform[M[_], N[_]](implicit monad:MonadWriter[WriterT[N, W, ?], W], transform:M ~> N): WriterT[M, W, ?] ~> WriterT[N, W, ?] =
    new (WriterT[M, W, ?] ~> WriterT[N, W, ?]) {
      def apply[A](fa: WriterT[M, W, A]): WriterT[N, W, A] =
        cats.data.WriterT[N,W,A](transform(fa.run))
    }

  protected def dropWriter[M[_]]:WriterTransformation[WriterT[M, W, ?], M, W] =
    new WriterTransformation[WriterT[M, W, ?], M, W] {
      def apply[A](fa: WriterT[M, W, A]): M[(W, A)] =
        fa.run
    }

  implicit val id2Writer: Id ~> Writer[W, ?] =
    fromIdentity[Writer[W, ?]]

  implicit val writer2Writer: Writer[W, ?] ~> Writer[W, ?] =
    identity[Writer[W, ?]]

  implicit val writer2id: WriterTransformation[Writer[W, ?], Id, W] =
    dropWriter[Id]
}



trait WriterValidMonads[W, E] extends WriterMonads[W] with ValidMonads[E] {

  implicit val writerValidMonad = new Monad[WriterValid[W, E, ?]] {

    def pure[A](x: A): WriterValid[W, E, A] =
      WriterT.value[Valid[E, ?],W,A](x)

    def flatMap[A, B](fa: WriterValid[W, E, A])(f: (A) => WriterValid[W, E, B]): WriterValid[W, E, B] =
      fa.flatMap(f)
  }

  implicit val writerValidTraverse:Traverse[WriterValid[W, E, ?]] = new Traverse[WriterValid[W, E, ?]] {
    def traverse[G[_], A, B](fa: WriterValid[W, E, A])(f: (A) => G[B])(implicit A: Applicative[G]): G[WriterValid[W, E, B]] =
      fa.run.value match {
        case Xor.Left(_) =>
          A.pure(fa.asInstanceOf[WriterValid[W, E, B]])
        case Xor.Right((w, a)) =>
          A.map(f(a))(r => writerValidMonad.writer(w -> r))
      }

    def foldLeft[A, B](fa: WriterValid[W, E, A], b: B)(f: (B, A) => B): B =
      fa.run.foldLeft(b)( (c, p) => f(c, p._2))

    def foldRight[A, B](fa: WriterValid[W, E, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.run.foldRight(lb)( (c, p) => f(c._2, p))
  }

}

trait WriterValidTransforms[W, E] extends WriterTransforms[W] with ValidTransforms[E] with WriterValidMonads[W, E] {

  implicit val id2ValidWriter: Id ~> WriterValid[W, E, ?] =
    fromIdentity[WriterValid[W, E, ?]]

  implicit val validWriter2ValidWriter:WriterValid[W, E, ?] ~> WriterValid[W, E, ?] =
    identity[WriterValid[W, E, ?]]

  implicit val writer2writerValid:Writer[W, ?] ~> WriterValid[W, E, ?] =
    fromWriterTransform[Id, Valid[E, ?]]

  implicit val valid2writerValid:Valid[E, ?] ~> WriterValid[W, E, ?] =
    toWriterTransform[Valid[E, ?], Valid[E, ?]]

  implicit val writerValid2Valid: WriterTransformation[WriterValid[W, E, ?], Valid[E, ?], W] =
    dropWriter[Valid[E, ?]]
}

trait WriterStateMonads[W, S] extends WriterMonads[W] with StateMonads[S] {

  implicit val writerStateMonad:MonadState[WriterState[W, S, ?], S] with MonadWriter[WriterState[W, S, ?], W] =
    new MonadState[WriterState[W, S, ?], S] with MonadWriter[WriterState[W, S, ?], W] {

      def writer[A](aw: (W, A)): WriterState[W, S, A] =
        cats.data.WriterT[State[S,?],W,A](stateMonad.pure(aw))

      def listen[A](fa: WriterState[W, S, A]): WriterState[W, S, (W, A)] =
        cats.data.WriterT[State[S,?],W,(W, A)](fa.run.map(empty -> _))

      def pass[A](fa: WriterState[W, S, ((W) => W, A)]): WriterState[W, S, A] =
        cats.data.WriterT[State[S,?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})

      def set(s: S): WriterState[W, S, Unit] =
        cats.data.WriterT[State[S, ?], W, Unit](stateMonad.set(s).map(empty -> _))

      def get: WriterState[W, S, S] =
        cats.data.WriterT[State[S, ?],W,S](stateMonad.get.map(empty -> _))

      def pure[A](x: A): WriterState[W, S, A] =
        WriterT.value[State[S, ?], W, A](x)

      def flatMap[A, B](fa: WriterState[W, S, A])(f: (A) => WriterState[W, S, B]): WriterState[W, S, B] =
        fa.flatMap(f)
    }

}

trait WriterStateTransforms[W, S] extends WriterStateMonads[W, S] with WriterTransforms[W] with StateTransforms[S] {

  implicit val id2StateWriter: Id ~> WriterState[W, S, ?] =
    fromIdentity[WriterState[W, S, ?]]

  implicit val writerState2WriterState:WriterState[W, S, ?] ~> WriterState[W, S, ?] =
    identity[WriterState[W, S, ?]]

  implicit val writer2writerState:Writer[W, ?] ~> WriterState[W, S, ?] =
    fromWriterTransform[Id, State[S, ?]]

  implicit val valid2WriterState:State[S, ?] ~> WriterState[W, S, ?] =
    toWriterTransform[State[S, ?], State[S, ?]]

  implicit val writerState2Writer: StateTransformation[WriterState[W, S, ?], Writer[W, ?], S] =
    new StateTransformation[WriterState[W, S, ?], Writer[W, ?], S] {
      def apply[A](fa: WriterState[W, S, A], s: S): Writer[W, (S, A)] = {
        val (ns, (w, a)) = fa.run.run(s).value
        writerMonad.writer(w -> (ns -> a))
      }
    }

  implicit val writerState2State:WriterTransformation[WriterState[W, S, ?], State[S, ?], W] =
    dropWriter[State[S, ?]]
}

trait WriterStateValidMonads[W, S, E] extends WriterStateMonads[W, S] with WriterValidMonads[W, E] with StateValidMonads[S, E] {

  implicit val writerStateValidMonads:MonadState[WriterStateValid[W, S, E, ?], S] with MonadWriter[WriterStateValid[W, S, E, ?], W] with MonadError[WriterStateValid[W, S, E, ?], E] =
    new MonadState[WriterStateValid[W, S, E, ?], S] with MonadWriter[WriterStateValid[W, S, E, ?], W] with MonadError[WriterStateValid[W, S, E, ?], E] {

      def get: WriterStateValid[W, S, E, S] =
        cats.data.WriterT[StateValid[S, E, ?],W,S](stateValidMonad.get.map(empty -> _))

      def set(s: S): WriterStateValid[W, S, E, Unit] =
        cats.data.WriterT[StateValid[S, E, ?], W, Unit](stateValidMonad.set(s).map(empty -> _))

      def listen[A](fa: WriterStateValid[W, S, E, A]): WriterStateValid[W, S, E, (W, A)] =
        cats.data.WriterT[StateValid[S, E, ?],W,(W, A)](fa.run.map(empty -> _))

      def writer[A](aw: (W, A)): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W,A](stateValidMonad.pure(aw))

      def pass[A](fa: WriterStateValid[W, S, E, ((W) => W, A)]): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})

      def handleErrorWith[A](fa: WriterStateValid[W, S, E, A])(f: (E) => WriterStateValid[W, S, E, A]): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E,?],W,A](stateValidMonad.handleErrorWith(fa.run)(e => f(e).run))

      def raiseError[A](e: E): WriterStateValid[W, S, E, A] =
        cats.data.WriterT[StateValid[S, E, ?],W,A](stateValidMonad.raiseError[(W,A)](e))

      def flatMap[A, B](fa: WriterStateValid[W, S, E, A])(f: (A) => WriterStateValid[W, S, E, B]): WriterStateValid[W, S, E, B] =
        fa.flatMap(f)

      def pure[A](x: A): WriterStateValid[W, S, E, A] =
        WriterT.value[StateValid[S, E, ?],W,A](x)
    }
}

trait WriterStateValidTransforms[W, S, E]
  extends WriterStateValidMonads[W, S, E]
    with WriterStateTransforms[W, S]
    with WriterValidTransforms[W, E]
    with StateValidTransforms[S, E] {

  implicit val id2WriterStateValid:Id ~> WriterStateValid[W, S, E, ?] =
    toWriterTransform[Id, StateValid[S, E, ?]]

  implicit val writer2WriterStateValid:Writer[W, ?] ~> WriterStateValid[W, S, E, ?] =
    fromWriterTransform[Id, StateValid[S, E, ?]]

  implicit val stateValid2WriterStateValid:StateValid[S, E, ?] ~> WriterStateValid[W, S, E, ?] =
    toWriterTransform[StateValid[S, E, ?], StateValid[S, E, ?]]

  implicit val state2WriterStateValid:State[S, ?] ~> WriterStateValid[W, S, E, ?] =
    toWriterTransform[State[S, ?], StateValid[S, E, ?]]

  implicit val valid2WriterStateValid:Valid[E, ?] ~> WriterStateValid[W, S, E, ?] =
    valid2StateValid.andThen[WriterStateValid[W, S, E, ?]](stateValid2WriterStateValid)

  implicit val writerValid2WriterStateValid:WriterValid[W, E, ?] ~> WriterStateValid[W, S, E, ?] =
    fromWriterTransform[Valid[E, ?], StateValid[S, E, ?]]

  implicit val writerState2WriterStateValid:WriterState[W, S, ?] ~> WriterStateValid[W, S, E, ?] =
    fromWriterTransform[State[S, ?], StateValid[S, E, ?]]

  implicit val writerStateValid2WriterStateValid:WriterStateValid[W, S, E, ?] ~> WriterStateValid[W, S, E, ?] =
    identity[WriterStateValid[W, S, E, ?]]

  implicit val writerStateValid2StateValid: StateTransformation[WriterStateValid[W, S, E, ?], WriterValid[W, E, ?], S] =
    new StateTransformation[WriterStateValid[W, S, E, ?], WriterValid[W, E, ?], S] {
      def apply[A](fa: WriterStateValid[W, S, E, A], s: S): WriterValid[W, E, (S, A)] =
        WriterT[Valid[E, ?], W, (S, A)](fa.run.run(s).map(f => f._2._1 -> (f._1 -> f._2._2)))
    }

  implicit val writerValidState2ValidState:WriterTransformation[WriterStateValid[W, S, E, ?], StateValid[S, E, ?], W] =
    dropWriter[StateValid[S, E, ?]]

}
