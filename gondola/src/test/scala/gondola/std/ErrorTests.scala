package gondola.std

import gondola.MonadError
import org.scalatest.{Matchers, FunSuite}

class ErrorTests extends FunSuite with Matchers {

  import ErrorMonads._
  import MonadError._

  test("Valid Monad") {
    type X[T] = Error[String, T]
    val m = errorMonad[String]
    ImplicitMonadTest.mapIntIsEven[X](m.pure(3)) should equal (m.pure(false))
    ImplicitMonadTest.flatMapValue[X](m.pure(5))(i => m.pure(i.toString)) should equal (m.pure("5"))
    ImplicitMonadTest.errorValue[X, String](m.pure(5), "Not Odd") should equal (m.pure(true))
    //ImplicitMonadTest.errorValue[X, String](m.pure(4), "Not Odd") should equal (m.pure("Not Odd"))
  }
}
