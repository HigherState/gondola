package gondola.std

import cats.Monoid
import org.scalatest.{Matchers, FunSuite}

trait StringVectorMonoid {
  implicit val writerMonoid: Monoid[Vector[String]] = new Monoid[Vector[String]] {
    def empty: Vector[String] = Vector.empty

    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
  }
}

object WriterImpl extends StringVectorMonoid with WriterMonads[Vector[String], String]

class WriterTests extends FunSuite with Matchers {

  import WriterImpl._

  test("Writer Monad") {
    type X[T] = Writer[Vector[String], T]
    val m = writerMonad
    ImplicitMonadTest.mapValue[X](m.pure(3)) should equal (m.pure(false))
    ImplicitMonadTest.flatMapValue[Writer[Vector[String], ?]](Writer(5))(i => Writer(i.toString)) should equal (Writer("5"))
    //ImplicitMonadTest.putL[Id, Vector[String]](6, Vector("log")) should equal (Writer(Vector("log"), 6))
  }

  test("Writer Valid Monad") {
    type X[T] = WriterValid[Vector[String], String, T]
    val m = writerValidMonad
    ImplicitMonadTest.mapValue[X](m.pure(3)) should equal (m.pure(false))
    ImplicitMonadTest.flatMapValue[X](m.pure(5))(i => m.pure(i.toString)) should equal (m.pure("5"))
    ImplicitMonadTest.errorValue[X, String](m.pure(5), "Not Odd") should equal (m.pure(true))
   // ImplicitMonadTest.errorValue[X, String](m.pure(4), "Not Odd") should not equal (m.pure(true))

  }
}


