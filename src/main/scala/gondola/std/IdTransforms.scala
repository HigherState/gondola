package gondola.std

import cats.{~>, Monad}

trait IdTransforms {

  def identity[M[_]] = new (M ~> M) {
    def apply[T](fa: M[T]): M[T] = fa
  }

  def fromIdentity[M[_]](implicit monad:Monad[M]) =
    new (Id ~> M) {
      def apply[T](fa: Id[T]): M[T] =
        monad.pure(fa)
    }

  implicit val id2id:Id ~> Id =
    identity[Id]

}
