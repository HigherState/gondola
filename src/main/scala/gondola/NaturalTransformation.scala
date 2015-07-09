package gondola

import scalaz.Functor


//taken from scalaz
trait ~>[-F[_], +G[_]] {
  self =>

  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }

  def >>>[H[_]](implicit t: G ~> H): F ~> H =
    t.compose[F](this)

  def sub[S[_] <: F[_]]:S ~> G =
    this.asInstanceOf[S ~> G]
}

trait ~~>[-F[_], +G[_]] extends ~>[F, G] {

  def apply[A](fa: F[A]): G[A] = handle(fa)

  def handle[A]:Function[F[A], G[A]]

}

object NaturalTransformationFunctions {
  def lift[M[_]:Functor, F[_], G[_]](nt: ~>[F, G]) =
    new (({type I[T] = M[F[T]]})#I ~> ({type I[T] = M[G[T]]})#I) {
      def apply[A](fa: M[F[A]]): M[G[A]] =
        Functor[M].map(fa)(nt.apply)
    }
}