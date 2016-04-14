package gondola.std

import cats._
import cats.data.XorT

object Writer {
  def apply[L, T](t:T)(implicit monoid:Monoid[L]) =
    cats.data.Writer(monoid.empty, t)
  def apply[L, T](l:L, t:T) =
    cats.data.Writer(l, t)
}

trait MonadPut[F[_], L] extends Monad[F] {

  def put[A](a: A)(l: L):F[A]

}

trait WriterMonads[L, E] extends ValidMonads[E] {
  implicit def writerMonoid:Monoid[L]

  def empty = writerMonoid.empty

  implicit val writerMonad = new MonadPut[Writer[L, ?], L] {
    private val monadT = data.WriterT.writerTIdMonad[L]

    def flatMap[A, B](fa: Writer[L, A])(f: (A) => Writer[L, B]): Writer[L, B] =
      monadT.flatMap(fa)(f)

    def put[A](a: A)(l: L): Writer[L, A] =
      cats.data.WriterT[Id, L, A](l -> a)

    def pure[A](x: A): Writer[L, A] =
      monadT.pure(x)
  }


  implicit val writerValidMonad = new MonadError[WriterValid[L, E, ?], E] with MonadPut[WriterValid[L, E, ?], L]{
    private val monadT = data.WriterT.writerTMonad[XorT[Id, E, ?], L](validMonad, writerMonoid)

    def raiseError[A](e: E): WriterValid[L, E, A] =
      cats.data.WriterT[Valid[E,?],L,A](validMonad.raiseError[(L,A)](e))

    def handleErrorWith[A](fa: WriterValid[L, E, A])(f: (E) => WriterValid[L, E, A]): WriterValid[L, E, A] =
      cats.data.WriterT[Valid[E,?],L,A](validMonad.handleErrorWith(fa.run)(e => f(e).run))

    def pure[A](x: A): WriterValid[L, E, A] =
      monadT.pure(x)

    def flatMap[A, B](fa: WriterValid[L, E, A])(f: (A) => WriterValid[L, E, B]): WriterValid[L, E, B] =
      monadT.flatMap(fa)(f)

    def put[A](a:A)(l: L): WriterValid[L, E, A] =
      cats.data.WriterT[Valid[E,?],L,A](validMonad.pure(l -> a))
  }

}

trait WriterTransforms[L, E] extends WriterMonads[L, E] {

  implicit val writer2writerValid:Writer[L, ?] ~> WriterValid[L, E, ?] =
    new (Writer[L, ?] ~> WriterValid[L, E, ?]) {
      def apply[A](fa: Writer[L, A]): WriterValid[L, E, A] =
        writerValidMonad.put(fa.run._2)(fa.run._1)
    }

//  implicit val valid2writerValid:Valid[E, ?] ~> WriterValid[L, E, ?] =
//    new (Valid[E, ?] ~> WriterValid[L, E, ?]) {
//      def apply[A](fa: Valid[E, A]):WriterValid[L, E, E] =
//        writerValidMonad.
//    }
}
