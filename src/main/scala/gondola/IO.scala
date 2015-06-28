package gondola

trait IO[+T] {

  def run():T

  def map[S](f: T => S): IO[S]

  def flatMap[S](F: T => IO[S]): IO[S]
}

object IO {
  def apply[T](f: => T):IO[T] =
    IOImpl(() => f)
}

case class IOImpl[+T](t: () => T) extends IO[T] {

  def run():T = t()

  def map[S](f: T => S): IO[S] =
    IOImpl(() => f(t()))

  def flatMap[S](f: T => IO[S]): IO[S] =
    IOImpl(() => f(t()).run())
}
