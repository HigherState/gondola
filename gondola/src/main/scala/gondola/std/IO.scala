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
  private def io2ReaderM[M[_], R]:IOT[M, ?] ~> ReaderT[M, R, ?] =
    new (IOT[M, ?] ~> ReaderT[M, R, ?]) {
      def apply[A](fa: IOT[M, A]): ReaderT[M, R, A] =
        fa.local(r => ())
    }

  implicit def io2Reader[R]:IO ~> Reader[R, ?] =
    io2ReaderM[Id, R]

  implicit def ioError2ReaderError[E, R]:IOError[E, ?] ~> ReaderError[R, E, ?] =
    io2ReaderM[Error[E, ?], R]

  implicit def ioWriter2ReaderWriter[W, R]:IOWriter[W, ?] ~> ReaderWriter[R, W, ?] =
    io2ReaderM[Writer[W, ?], R]

  implicit def ioWriterError2ReaderWriterError[W, E, R]:IOWriterError[W, E, ?] ~> ReaderWriterError[R, W, E, ?] =
    io2ReaderM[WriterError[W, E, ?], R]
}

object IOReaderTransformations extends IOReaderTransformations
