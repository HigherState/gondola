package gondola

trait Reader[F, +T] {
  def apply(c: => F):T

  def map[S](transformF: T => S): Reader[F, S]

  def flatMap[S](transformF: T => Reader[F, S]): Reader[F, S]
}

object Reader {
  def apply[F, T](f: (F) => T) =
    ReaderImpl(f)
}

case class ReaderImpl[F, +T](wrappedF: F => T) extends Reader[F, T] {

  def apply(c: => F) = wrappedF(c)

  def map[S](transformF: T => S): Reader[F, S] =
    ReaderImpl(c => transformF(wrappedF(c)))

  def flatMap[S](transformF: T => Reader[F, S]): Reader[F, S] =
    ReaderImpl(c => transformF(wrappedF(c))(c))

}

case class ReaderFacade[F, +T](value:T) extends Reader[F, T] {

  def apply(c: => F) =
    value

  def map[S](transformF: T => S): Reader[F, S] =
    ReaderFacade(transformF(value))

  def flatMap[S](transformF: T => Reader[F, S]): Reader[F, S] =
    transformF(value)
}