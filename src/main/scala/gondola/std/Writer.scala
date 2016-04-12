package gondola.std

import cats._

object Writer {
  def apply[L, T](t:T)(implicit monoid:Monoid[L]) =
    cats.data.Writer(monoid.empty, t)
  def apply[L, T](l:L, t:T) =
    cats.data.Writer(l, t)
}

trait WriterMonads[L, E] extends ValidMonads[E] {
  implicit def writerMonoid:Monoid[L]

  val writerMonad = data.WriterT.writerTIdMonad[L]

  val writerValidMonad = data.WriterT.writerTMonad[Valid[E ,?], L](validMonad, writerMonoid)
}
