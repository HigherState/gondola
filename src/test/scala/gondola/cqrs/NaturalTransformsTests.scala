//package org.higherState.cqrs
//
//import org.higherState.cqrs.std._
//import org.scalatest.concurrent.ScalaFutures
//import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
//
//import scala.concurrent.Future
//import scala.language.higherKinds
//import scalaz.{Success, ~>}
//
//class NaturalTransformsTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {
//
//  import Transforms._
//
//  test("get implicits") {
//
//    type V[+T] = Valid[String, T]
//    implicitly[~>[Id, V]].apply(3) should equal (Success(3))
//  }
//
//  test("Futures") {
//    import scala.concurrent.ExecutionContext.Implicits.global
//
//    class Threading() {
//      def call():Id[Unit] = {
//        Thread.sleep(100)
//        println("T" + Thread.currentThread().getId)
//      }
//    }
//
//    class Service extends MonadBound[Future] {
//      val t = new Threading
//      implicit val pipe = Transforms.idFuturePipe
//      val m = Future.successful(123)
//      def call:Future[Unit] = {
//        println(Thread.currentThread().getId)
//
//        bind(pipe(t.call())) {_ =>
//          bind(pipe(t.call())) { _ =>
//            map(pipe(t.call())) { _ =>
//              println(Thread.currentThread().getId)
//              Unit
//            }
//          }
//        }
//      }
//
//      def call2:Future[Unit] = {
//        println(Thread.currentThread().getId)
//        map(pipe(t.call())) { _ =>
//          println(Thread.currentThread().getId)
//        }
//      }
//    }
//    println(Thread.currentThread().getId)
//    new Service().call
//    Thread.sleep(500)
//    println()
//    new Service().call2
//    Thread.sleep(500)
//  }
//}
