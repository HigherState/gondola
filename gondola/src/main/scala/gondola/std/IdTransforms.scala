package gondola.std

import cats.{Applicative, Eval, Monad, Traverse}
import gondola.~>

trait IdTransforms {

  def identity[M[_]] = new (M ~> M) {
    def apply[T](fa: M[T]): M[T] = fa
  }

  def fromIdentity[M[_]](implicit monad:Monad[M]) =
    new (Id ~> M) {
      def apply[T](fa: Id[T]): M[T] =
        monad.pure(fa)
    }

  implicit val idTraverse:Traverse[Id] =
    new Traverse[Id] {
      def traverse[G[_], A, B](fa: Id[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[Id[B]] =
        f(fa)

      def foldLeft[A, B](fa: Id[A], b: B)(f: (B, A) => B): B =
        f(b, fa)

      def foldRight[A, B](fa: Id[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa, lb)
    }

  implicit val id2id:Id ~> Id =
    identity[Id]

}
