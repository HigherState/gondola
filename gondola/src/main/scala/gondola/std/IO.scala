package gondola.std

import cats.data.ReaderT
import gondola.~>

object IO {

  def apply[T](f: => T):IO[T] = {
    val f2:Function[Unit, T] = Unit => f
    Reader(f2)
  }

  def lift[M[_], T](f: => M[T]):IOT[M, T] =
    ReaderT[M, Unit, T](_ => f)

}

trait IOReaderTransformations {
  implicit def IO2Reader[M[_], R]:IOT[M, ?] ~> ReaderT[M, R, ?] =
    new (IOT[M, ?] ~> ReaderT[M, R, ?]) {
      def apply[A](fa: IOT[M, A]): ReaderT[M, R, A] =
        fa.local(r => ())
    }
}

object IOReaderTransformations extends IOReaderTransformations
