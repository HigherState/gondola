package gondola.std

import cats.{MonadError, MonadState}
import cats.data.WriterTFunctions
import gondola._

object ImplicitMonadTest extends WriterTFunctions {

  import Monad._

  def mapIntIsEven[M[_]:Monad](value:M[Int]):M[Boolean] = {
    value.map(i => i % 2 == 0)
  }

  def flatMapValue[M[_]:Monad](value:M[Int])(f:Int => M[String]):M[String] = {
    value.flatMap(f)
  }

  def errorValue[M[_], E](value:M[Int], error:E)(implicit me:MonadError[M, E]):M[Boolean] = {
    import gondola.MonadError._

    value.flatMap {
      case i if i % 2 == 0 => raiseError (error)
      case _ => pure (true)
    }
  }

  def write[M[_], W]()(implicit m:cats.MonadWriter[M, Vector[String]]):(M[Int], (Vector[String], Int)) =
    (for {
      v <- m.pure(12)
      _ <- m.tell(Vector("one"))
      _ <- m.tell(Vector("two"))
    } yield v + 4, (Vector("one", "two"), 16))

  def state[M[_]](addValue:Int)(implicit m:MonadState[M, Int]) =
    for {
      a <- m.modify(_ + addValue)
    } yield addValue % 2 == 0
}
