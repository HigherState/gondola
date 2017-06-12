package gondola.std

import cats.{Applicative, Eval, Functor, Monad, Traverse}
import gondola.~>

import scala.annotation.tailrec

trait IdMonad {
  implicit val idMonad:Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] =
      f(fa)

    def pure[A](x: A): Id[A] =
      x

    @tailrec
    def tailRecM[A, B](a: A)(f: (A) => Id[Either[A, B]]): Id[B] =
      f(a) match {
        case Left(aa) => tailRecM(aa)(f)
        case Right(b) => b
      }
  }
}

object IdMonads extends IdMonad

trait IdTransformations {

  implicit val idTraverse:Traverse[Id] =
    new Traverse[Id] {
      def traverse[G[_], A, B](fa: Id[A])(f: (A) => G[B])(implicit A: Applicative[G]): G[Id[B]] =
        f(fa)

      def foldLeft[A, B](fa: Id[A], b: B)(f: (B, A) => B): B =
        f(b, fa)

      def foldRight[A, B](fa: Id[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa, lb)
    }

  implicit val id2id:Id ~> Id =
    IdTransformationOps.identity[Id]

  implicit val idFunctor:Functor[Id] = idTraverse
}

object IdTransformations extends IdTransformations with IdMonad

trait IdTransformationOps {
  def identity[M[_]] = new (M ~> M) {
    def apply[T](fa: M[T]): M[T] = fa
  }

  def fromIdentity[M[_]](implicit M:Monad[M]) =
    new (Id ~> M) {
      def apply[T](fa: Id[T]): M[T] =
        M.pure(fa)
    }
}

object IdTransformationOps extends IdTransformationOps
