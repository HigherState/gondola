package gondola.std

import cats.data.ReaderT

object IO {

  def apply[T](f: => T):IO[T] = {
    val f2:Function[Unit, T] = Unit => f
    Reader(f2)
  }

  def lift[M[_], T](f: => M[T]):IOT[M, T] =
    ReaderT[M, Unit, T](_ => f)

}
