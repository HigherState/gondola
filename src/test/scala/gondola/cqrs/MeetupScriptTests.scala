//package org.higherState.cqrs
//
//import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}
//import org.scalatest.concurrent.ScalaFutures
//import org.higherState.cqrs.std.{Id, Transforms}
//import scala.concurrent.Future
//import org.higherState.cqrs
//
//class MeetupScriptTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {
//  import Transforms._
//  import scalaz._
//
//  //Define a Monadic service
//  class MyService[Out[+_]](implicit val monad:Monad[Out]) {
//    def doSomething:Out[String] =
//      monad.point("Something done")
//  }
//  type Valid[+T] = ValidationNel[String, T]
//  type Dis[+T] = \/[String, T]
//
//
//  test("MyService works") {
//    new MyService[Id].doSomething should equal ("Something done")
//    new MyService[Valid].doSomething should equal (Success("Something done"))
//    new MyService[Dis].doSomething should equal (\/-("Something done"))
//  }
//
//
//
//
//
//
//
//  //Service aggregator to pipe through the Monadic service
//  class MyServiceAggregator[Out[+_], In[+_]](myService:MyService[In])
//                                            (implicit val pipe: ~>[In, Out]) {
//
//    def callDoSomething:Out[String] =
//      pipe(myService.doSomething)
//  }
//
//
//  test("MyService aggregator") {
//    new MyServiceAggregator[Valid, Id](new MyService[Id]).callDoSomething should equal (Success("Something done"))
//  }
//
//
//
//
//
//
//
//
//  class ExtendedServiceAggregator[Out[+_], In[+_]](myService:MyService[In])
//                                                  (implicit val monad:Monad[Out], val pipe: ~>[In, Out]) {
//
//    def extendedSomething:Out[String] =
//      monad.map(pipe(myService.doSomething)) { s =>
//        s + " completely"
//      }
//  }
//
//  test ("Myservice extender") {
//    new ExtendedServiceAggregator[Valid, Id](new MyService[Id]).extendedSomething should equal (Success("Something done completely"))
//
//    type R[+T] = cqrs.Reader[String, T]
//    type RValid[+T] = R[Valid[T]]
//    new ExtendedServiceAggregator[RValid, R](new MyService[R]).extendedSomething.apply("Not used") should equal (Success("Something done completely"))
//  }
//
//
//
//
//
//
//  class MyFailableService {
//
//    def success:Valid[String] = Success("Success")
//
//    def failed:Valid[String] = Failure(NonEmptyList("Failed"))
//  }
//
//
//  class MultiplePipes[Out[+_], In[+_]](myService:MyService[In], failableService:MyFailableService)
//                                      (implicit val monad:Monad[Out], val pipe1: ~>[In, Out], val pipe2: ~>[Valid, Out]) {
//
//    def completeSucceed:Out[String] = {
//      monad.bind(pipe1(myService.doSomething)) { s =>
//        monad.map(pipe2(failableService.success)) { s2 =>
//          s2 + ":" + s
//        }
//      }
//    }
//
//    def completeFailed:Out[String] = {
//      monad.bind(pipe1(myService.doSomething)) { s =>
//        monad.map(pipe2(failableService.failed)) { s2 =>
//          s2 + ":" + s
//        }
//      }
//    }
//  }
//
//  test("Multiple pipes") {
//    val service = new MultiplePipes[Valid, Id](new MyService[Id], new MyFailableService)
//    service.completeSucceed should equal (Success("Success:Something done"))
//    service.completeFailed should equal (Failure(NonEmptyList("Failed")))
//  }
//
//
//
//
//
//  type FutureValid[+T] = Future[Valid[T]]
//  test("Simplified service") {
//    import scala.concurrent.ExecutionContext.Implicits.global
//
//    val s = new SimplifiedMultiplePipes[FutureValid, Valid, Future](new SourceService[Valid], new SourceService[Future])
//    whenReady(s.runBind){_ should equal (Success("Result,Result"))}
//    //whenReady(s.runSequence(3)){_ should equal (Success("Result,Result,Result,Result"))}
//  }
//
//}
//
//class SourceService[Out[+_]:scalaz.Monad] extends MonadBound[Out] {
//  def doSomething:Out[String] =
//    point("Result")
//  def someOption:Out[Option[String]] =
//    point(Some("value"))
//}
//import Scalaz._
//class SimplifiedMultiplePipes[Out[+_]:scalaz.Monad, In1[+_]:(~>![Out])#I, In2[+_]:(~>![Out])#I]
//  (myService:SourceService[In1], myService2:SourceService[In2]) {
//
//  import Monad._
//
//  def runBind:Out[String] = {
//    for {
//      s <- myService.doSomething
//      s1 = ","
//      s2 <- myService2.someOption
//    } yield s + s1 + s2
//  }
//
//  def runSingle:Out[String] =
//    myService2.doSomething.map(s => s + ".")
//
////  def runSequence(c:Int):Out[String] = {
////    val seq = sequence((0 to c).toList.map(_ => ~>(myService.doSomething)))
////    map(seq) { s =>
////      s.mkString(",")
////    }
////  }
//}
