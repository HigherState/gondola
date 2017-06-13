package gondola.test

import gondola._
import cats.{MonadWriter, Monoid, MonadError}
import org.scalatest.{Matchers, FunSuite}

class MonadWordsTest extends FunSuite with Matchers with MonadMatchers {
  import gondola.std.ReaderWriterErrorMonads._

  implicit val M = new Monoid[Vector[String]] {
    def empty: Vector[String] = Vector.empty

    def combine(x: Vector[String], y: Vector[String]): Vector[String] =
      x ++ y
  }

  test("fails with") {
    failWith[std.Error[String, ?], String]("fails") should errorWith("fails")
    failWith[std.WriterError[Vector[String], String, ?], String]("fails") should invalidate
  }

  test("log with") {
    logWith[std.Writer[Vector[String], ?], Vector[String]](Vector("one", "two")) should haveLog(Vector("one", "two"))
    logWith[std.WriterError[Vector[String], String, ?], Vector[String]](Vector("one", "two")) should haveLog(Vector("one", "two"))
  }

  test("have value") {
    withValue[std.Writer[Vector[String], ?], Int](4) should haveValue(4)
    withValue[std.Error[String, ?], Int](4) should haveValue(4)
    withValue[std.WriterError[Vector[String], String, ?], Int](4) should haveValue(4)
    withValue[std.Error[String, ?], Int](4) should validate
    withValue[std.WriterError[Vector[String], String, ?], Int](4) should validate
  }

  def failWith[M[_], E](message:E)(implicit M:MonadError[M, E]):M[_] =
    M.raiseError(message)

  def logWith[M[_], W](log:W)(implicit M:MonadWriter[M, W]):M[_] =
    M.writer(log -> Unit)

  def withValue[M[_], A](a:A)(implicit M:Monad[M]):M[A] =
    M.pure(a)
}
