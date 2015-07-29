package gondola

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}

import scalaz.{Failure, NonEmptyList, Success, ValidationNel}


/**
 * Created by jamie.pullar on 27/07/2015.
 */
class SequenceTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {
  import std.Transforms._

  test("Validation Sequence") {
    import scalaz.Scalaz._
    type V[+T] = ValidationNel[String, T]

    val list = List[V[Int]](
      Success(1), Success(2), Success(3), Success(4), Success(5), Failure(NonEmptyList("First")), Success(6), Failure(NonEmptyList("Second"))
    )

    val monad = implicitly[Monad[V]]
    println(monad.sequence(list))

    type RV[+T] = Reader[Int, V[T]]

    val list2 = List[RV[Int]](
      Reader{i => println(i); Success(i +1)}, Reader{i => println(i); Success(i + 2)}, Reader{i => println(i); Success(i + 3)}, Reader{i => println(i); Success(i +4)},
      Reader(_ => Failure(NonEmptyList("First"))), Reader{i => println(i); Success(i + 5)}, Reader(_ => Failure(NonEmptyList("Second")))
    )

    val monad2 = implicitly[Monad[RV]]
    println(monad2.sequence(list2).apply(1))

    type RVW[+T] = RV[Writer[Vector[Int], T]]

    val list3 = List[RVW[Int]](
      Reader{i => println(1); Success(Writer(Vector(i + 1), i +1))},
      Reader{i => println(2); Success(Writer(Vector(i + 2), i + 2))},
      Reader{i => println(3); Success(Writer(Vector(i + 3), i +3))},
      Reader(_ => Failure(NonEmptyList("First"))),
      Reader{i => println(4); Success(Writer(Vector(i + 4), i +4))},
      Reader(_ => Failure(NonEmptyList("Second")))
    )

    val monad3 = implicitly[Monad[RVW]]
    println(monad3.sequence(list3).apply(1))
  }
}
