package gondola.std

import cats.Monoid
import gondola.Monad
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}

/**
  * Created by Jamie Pullar on 09/04/2016.
  */
class WriterTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {

  import cats.data.WriterT._

  implicit val monoid:Monoid[Vector[String]] = new Monoid[Vector[String]] {
    override def empty: Vector[String] = Vector.empty

    override def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
  }

  test("Writer Monad") {
    ImplicitMonadTest.mapValue[Writer[Vector[String], ?]](Writer(3)) should equal (Writer(false))
    ImplicitMonadTest.flatMapValue[Writer[Vector[String], ?]](Writer(5))(i => Writer(i.toString)) should equal (Writer("5"))
  }

}


object ImplicitMonadTest {

  import Monad._

  def mapValue[M[_]:Monad](value:M[Int]):M[Boolean] = {
    value.map(i => i % 2 == 0)
  }

  def flatMapValue[M[_]:Monad](value:M[Int])(f:Int => M[String]):M[String] = {
    value.flatMap(f)
  }
}
