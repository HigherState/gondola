package gondola.std

import cats.data.{ReaderT, XorT}

object IO {

  def apply[T](f: => T):IO[T] = {
    val f2:Function[Unit, T] = Unit => f
    Reader(f2)
  }

  def valid[E, T](f: => T):IOValid[E, T] =
    validF(XorT.right[Id, E, T](f))

  def validF[E, T](f: => Valid[E, T]):IOValid[E, T] =
    ReaderT[Valid[E, ?], Unit, T](_ => f)
}
