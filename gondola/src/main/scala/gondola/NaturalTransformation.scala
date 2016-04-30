package gondola

import cats._
import cats.data.{Coproduct, Xor}

//Cannot Pimp a Natural transform. had to recreate
trait ~>[F[_], G[_]] {
  self =>

  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f:E ~> F): E ~> G =
    new (E ~> G) {
      def apply[A](fa: E[A]): G[A] = self.apply(f(fa))
    }

  def andThen[H[_]](f: G ~> H): F ~> H =
    f.compose(self)

  def or[H[_]](h: H ~> G): Coproduct[F, H, ?] ~> G =
    new (Coproduct[F, H, ?] ~> G) {
      def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run match {
        case Xor.Left(ff) => self(ff)
        case Xor.Right(gg) => h(gg)
      }
    }

  def >>>[H[_]](implicit t: G ~> H): F ~> H =
    t.compose[F](this)

  def sub[S[_] <: F[_]]:S ~> G =
    this.asInstanceOf[S ~> G]

  def lift[M[_]:Functor]:({type I[A] = M[F[A]]})#I ~> ({type I[A] = M[G[A]]})#I =
    new (({type I[A] = M[F[A]]})#I ~> ({type I[A] = M[G[A]]})#I) { // using kind doesnt work here
      def apply[A](fa: M[F[A]]): M[G[A]] =
        Functor[M].map(fa)(self.apply)
    }
}

trait ~~>[F[_], G[_]] extends ~>[F, G] {

  def apply[A](fa: F[A]): G[A] = handle(fa)

  def handle[A]:Function[F[A], G[A]]

}

trait TransformIntercept[F[_], G[_]]{ self =>

  def intercept[A](nt:F ~> G):PartialFunction[F[A], G[A]]

  def apply(nt:F ~> G):F ~> G =
    new (F ~~> G) {
      def handle[A]: Function[F[A], G[A]] = { fa =>
        if (intercept[A](nt).isDefinedAt(fa))
          intercept[A](nt)(fa)
        else
          nt(fa)
      }
    }
}

trait StateTransformation[M[_], N[_], S] {
  def apply[A](fa: M[A], s:S):N[(S, A)]
}

trait WriterTransformation[M[_], N[_], W] {
  def apply[A](fa: M[A]):N[(W, A)]
}

trait ReaderTransformation[M[_], N[_], R] {
  def apply[A](fa: M[A], r:R):N[A]
}