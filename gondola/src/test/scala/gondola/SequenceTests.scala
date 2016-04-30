//package gondola
//
//import cats.data.Xor
//import gondola.std.Valid
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}
//
//class SequenceTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {
//  import gondola.std.Transforms._
//
//  test("Validation Sequence") {
//    type V[+T] = Valid[String, T]
//
//    val list = List[V[Int]](
//      Xor.Right(1), Xor.Right(2), Xor.Right(3), Xor.Right(4), Xor.Right(5), Xor.left(NonEmptyList("First")), Xor.Right(6), Xor.left(NonEmptyList("Second"))
//    )
//
//    val monad = implicitly[Monad[V]]
//    println(monad.sequence(list))
//
//    type RV[+T] = Reader[Int, V[T]]
//
//    val list2 = List[RV[Int]](
//      Reader{i => println(i); \/-(i +1)}, Reader{i => println(i); \/-(i + 2)}, Reader{i => println(i); \/-(i + 3)}, Reader{i => println(i); \/-(i +4)},
//      Reader(_ => -\/(NonEmptyList("First"))), Reader{i => println(i); \/-(i + 5)}, Reader(_ => -\/(NonEmptyList("Second")))
//    )
//
//    val monad2 = implicitly[Monad[RV]]
//    println(monad2.sequence(list2).apply(1))
//
//    type RVW[+T] = RV[Writer[Vector[Int], T]]
//
//    val list3 = List[RVW[Int]](
//      Reader{i => println(1); \/-(Writer(Vector(i + 1), i +1))},
//      Reader{i => println(2); \/-(Writer(Vector(i + 2), i + 2))},
//      Reader{i => println(3); \/-(Writer(Vector(i + 3), i +3))},
//      Reader(_ => -\/(NonEmptyList("First"))),
//      Reader{i => println(4); \/-(Writer(Vector(i + 4), i +4))},
//      Reader(_ => -\/(NonEmptyList("Second")))
//    )
//
//    val monad3 = implicitly[Monad[RVW]]
//    println(monad3.sequence(list3).apply(1))
//  }
//}
