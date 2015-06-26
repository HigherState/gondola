package gondola


//taken from scalaz
trait ~>[-F[_], +G[_]] {
  self =>

  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }

  def >>>[H[_]](implicit t: G ~> H): F ~> H =
    t.compose[F](this)

}

trait ~~>[-F[_], +G[_]] extends ~>[F, G] {

  def apply[A](fa: F[A]): G[A] = handle(fa)

  def handle[A]:Function[F[A], G[A]]

}