package gondola.std

import cats.data.{Xor, NonEmptyList}
import org.scalatest.{Matchers, FunSuite}

object ValidImpl extends ValidMonads[NonEmptyList[String]]

class ValidTests extends FunSuite with Matchers {

  import ValidImpl._

  test("Valid Monad") {
    ImplicitMonadTest.mapValue[Valid[String, ?]](Xor.right(3)) should equal (Xor.right(false))
    ImplicitMonadTest.flatMapValue[Valid[String, ?]](Xor.right(5))(i => Xor.right(i.toString)) should equal (Xor.right("5"))
    ImplicitMonadTest.errorValue[Valid[String, ?], String](Xor.right(5), "Not Odd") should equal (Xor.right(true))
    ImplicitMonadTest.errorValue[Valid[String, ?], String](Xor.right(4), "Not Odd") should equal (Xor.left(NonEmptyList("Not Odd")))
  }
}
