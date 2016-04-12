package gondola.std

import cats.Monoid
import cats.data.NonEmptyList
import org.scalatest.{Matchers, FunSuite}

object WriterImpl extends WriterMonads[Vector[String], NonEmptyList[String]] {
  implicit val writerMonoid: Monoid[Vector[String]] = new Monoid[Vector[String]] {
    def empty: Vector[String] = Vector.empty

    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
  }
}

class WriterTests extends FunSuite with Matchers {

  import WriterImpl._

  test("Writer Monad") {
    ImplicitMonadTest.mapValue[Writer[Vector[String], ?]](Writer(3)) should equal (Writer(false))
    ImplicitMonadTest.flatMapValue[Writer[Vector[String], ?]](Writer(5))(i => Writer(i.toString)) should equal (Writer("5"))
    ImplicitMonadTest.putL[Id, Vector[String]](6, Vector("log")) should equal (Writer(Vector("log"), 6))
  }
}


