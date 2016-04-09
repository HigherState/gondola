package gondola

import cats.Functor

trait ~>[-F[_], +G[_]] {
  self =>

  def apply[T](fa: F[T]): G[T]

  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }

  def >>>[H[_]](implicit t: G ~> H): F ~> H =
    t.compose[F](this)

  def sub[S[_] <: F[_]]:S ~> G =
    this.asInstanceOf[S ~> G]
}

trait ~~>[-F[_], +G[_]] extends ~>[F, G] {

  def apply[T](fa: F[T]): G[T] = handle(fa)

  def handle[T]:Function[F[T], G[T]]

}

object NaturalTransformationFunctions {
  def lift[M[_]:Functor, F[_], G[_]](nt: ~>[F, G]) =
    new (({type I[T] = M[F[T]]})#I ~> ({type I[T] = M[G[T]]})#I) { // using kind doesnt work here
    def apply[A](fa: M[F[A]]): M[G[A]] =
      Functor[M].map(fa)(nt.apply)
    }
}