package gondola.std

import cats._
import cats.data.{WriterT, XorT}

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
    private val monadT = data.WriterT.writerTIdMonad[W]

    def flatMap[A, B](fa: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] =
      monadT.flatMap(fa)(f)

    def writer[A](aw: (W, A)): Writer[W, A] =
      cats.data.WriterT[Id, W, A](aw)

    def listen[A](fa: Writer[W, A]): Writer[W, (W, A)] =
      pure(fa.run)

    def pass[A](fa: Writer[W, ((W) => W, A)]): Writer[W, A] = {
      val (l, (f,a)) = fa.run
      writer(f(l) -> a)
    }

    def pure[A](x: A): Writer[W, A] =
      monadT.pure(x)
  }
}

trait WriterTransforms[W] extends WriterMonads[W] with IdTransforms {

  def toWriter[M[_]](implicit monad:Monad[M]): M ~> WriterT[M, W, ?] =
    new (M ~> WriterT[M, W, ?]) {
      def apply[A](fa: M[A]): WriterT[M, W, A] =
        cats.data.WriterT[M, W, A](monad.map(fa)(empty -> _))
    }

  def fromWriter[M[_]](implicit monad:MonadWriter[WriterT[M, W, ?], W]): WriterT[Id, W, ?] ~> WriterT[M, W, ?] =
    new (WriterT[Id, W, ?] ~> WriterT[M, W, ?]) {
      def apply[A](fa: WriterT[Id, W, A]): WriterT[M, W, A] =
        monad.writer(fa.run)
    }

  implicit val id2Writer: Id ~> Writer[W, ?] =
    fromIdentity[Writer[W, ?]]

  implicit val writer2Writer: Writer[W, ?] ~> Writer[W, ?] =
    identity[Writer[W, ?]]
}



trait WriterValidMonads[W, E] extends WriterMonads[W] with ValidMonads[E] {

  implicit val writerValidMonad = new MonadError[WriterValid[W, E, ?], E] with MonadWriter[WriterValid[W, E, ?], W]{
    private val monadT = data.WriterT.writerTMonad[XorT[Id, E, ?], W](validMonad, writerMonoid)

    def raiseError[A](e: E): WriterValid[W, E, A] =
      cats.data.WriterT[Valid[E,?],W,A](validMonad.raiseError[(W,A)](e))

    def handleErrorWith[A](fa: WriterValid[W, E, A])(f: (E) => WriterValid[W, E, A]): WriterValid[W, E, A] =
      cats.data.WriterT[Valid[E,?],W,A](validMonad.handleErrorWith(fa.run)(e => f(e).run))

    def pure[A](x: A): WriterValid[W, E, A] =
      monadT.pure(x)

    def flatMap[A, B](fa: WriterValid[W, E, A])(f: (A) => WriterValid[W, E, B]): WriterValid[W, E, B] =
      monadT.flatMap(fa)(f)

    def writer[A](aw: (W, A)): WriterValid[W, E, A] =
      cats.data.WriterT[Valid[E,?],W,A](validMonad.pure(aw))

    def listen[A](fa: WriterValid[W, E, A]): WriterValid[W, E, (W, A)] =
      cats.data.WriterT[Valid[E,?],W,(W, A)](fa.run.map(empty -> _))

    def pass[A](fa: WriterValid[W, E, ((W) => W, A)]): WriterValid[W, E, A] =
      cats.data.WriterT[Valid[E,?],W, A](fa.run.map{ case (l, (f, a)) => f(l) -> a})
  }
}

trait WriterValidTransforms[W, E] extends WriterTransforms[W] with ValidTransforms[E] with WriterValidMonads[W, E] {

  implicit val id2ValidWriter: Id ~> WriterValid[W, E, ?] =
    fromIdentity[WriterValid[W, E, ?]]

  implicit val validWriter2ValidWriter:WriterValid[W, E, ?] ~> WriterValid[W, E, ?] =
    identity[WriterValid[W, E, ?]]

  implicit val writer2writerValid:Writer[W, ?] ~> WriterValid[W, E, ?] =
    fromWriter[Valid[E, ?]]

  implicit val valid2writerValid:Valid[E, ?] ~> WriterValid[W, E, ?] =
    toWriter[Valid[E, ?]]
}
