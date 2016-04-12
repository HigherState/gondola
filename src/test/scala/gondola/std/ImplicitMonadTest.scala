package gondola.std

import cats.MonadError
import cats.data.{WriterT, WriterTFunctions, NonEmptyList}
import gondola._

object ImplicitMonadTest extends WriterTFunctions {

  import Monad._

  def mapValue[M[_]:Monad](value:M[Int]):M[Boolean] = {
    value.map(i => i % 2 == 0)
  }

  def flatMapValue[M[_]:Monad](value:M[Int])(f:Int => M[String]):M[String] = {
    value.flatMap(f)
  }

  def errorValue[M[_], E](value:M[Int], error:E)(implicit me:MonadError[M, NonEmptyList[E]]):M[Boolean] = {
    value.flatMap{
      case i if i % 2 == 0 =>
        me.raiseError(NonEmptyList(error))
      case _ =>
        pure(true)
    }
  }

  def putL[M[_], L](value:M[Int], l:L)(implicit monad:Monad[M]):WriterT[M, L, Int] = {
    putT(value)(l)
  }
}
