package gondola.authentication

import cats.data._
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import org.scalatest.concurrent.ScalaFutures
import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import gondola._
import akka.util.Timeout
import repository._
import java.util.concurrent.atomic.AtomicReference

import cats.~>

import scala.concurrent.{ExecutionContext}
import gondola.std._

object MyTransforms extends FutureValidTransforms[NonEmptyList[String]]

class InstanceTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {

  import scala.concurrent.duration._
  import MyTransforms._

  implicit val system = ActorSystem("System")
  implicit val exectionContext:ExecutionContext = system.dispatcher
  implicit val globalTimeout:Timeout = 5.minutes

  type FV[T] = FutureValid[NonEmptyList[String], T]
  type V[T] = Valid[NonEmptyList[String], T]


  test("CQRS actor implementation on repository with a future handling on the authentication service") {
    //CQRS actors repository service.
    //Single command handler actor and single query executor actor
    val atomicHashMap = new AtomicReference[Map[UserLogin, UserCredentials]](Map.empty[UserLogin, UserCredentials])

    val repo =
      ActorN[KvC[UserLogin, UserCredentials, ?], Id](InMemoryKeyValueCommandHandler[Id, UserLogin, UserCredentials](atomicHashMap)) ->  // Kvc ~> Id ~> Future
      ActorN[KvQ[UserLogin, UserCredentials, ?], Id](InMemoryKeyValueQueryExecutor[Id, UserLogin, UserCredentials](atomicHashMap)) |> // Kvq ~> Id ~> Future
      Couple[KvD[UserLogin, UserCredentials, ?], KvC[UserLogin, UserCredentials, ?], KvQ[UserLogin, UserCredentials, ?], FutureT[Id, ?]] //KvD ~> Future

    val liftedRepo:KvD[UserLogin, UserCredentials, ?] ~> FV = NaturalTransformExt[KvD[UserLogin, UserCredentials, ?], FutureT[Id, ?]](repo).>>>[FV] //KvD ~> Future ~> FutureValid

    val t:Coproduct[KvC[UserLogin, UserCredentials, ?], KvQ[UserLogin, UserCredentials, ?], ?] ~> V = null

    val c = AuthenticationCommandHandler(liftedRepo, 10)
    val q = AuthenticationQueryExecutor(liftedRepo)
    val cq = Couple[AuthenticationDomain, AuthenticationCommand, AuthenticationQuery, FV](c,q)
    val futureAuthenticationService = ActorN.Future[AuthenticationDomain, V](cq)
    //val futureAuthenticationService = new FutureActorTransform[AuthenticationDomain, Valid](auth, None)
//
//
//    //Authentication service will handle futures from repository actor

    whenReady(futureAuthenticationService(CreateNewUser(UserLogin("test@test.com"), Password("password"))).value) { result1 =>
      result1.value should equal (Xor.Right(Acknowledged))
      whenReady(futureAuthenticationService(Authenticate(UserLogin("test@test.com"), Password("password"))).value) {result2 =>
        result2.value should equal (Xor.Right(UserLogin("test@test.com")))
      }
      whenReady(futureAuthenticationService(CreateNewUser(UserLogin("test@test.com"), Password("password"))).value) { result3 =>
        result3.value should equal (Xor.Left(NonEmptyList("UserCredentialsAlreadyExistFailure(userLogin)")))
      }
    }
  }

//  test("CQRS actor authentication service using the simple hashmap repository") {
//    //simple repository service
//    val testHashRepository =
//      new HashMapRepository(mutable.HashMap.empty[UserLogin, UserCredentials])
//
//    val authCommandHandler = system.actorOf(Props(AsActor(new AuthenticationCommandHandler[V, Id](testHashRepository, 10))))
//    val authQueryExecutor = system.actorOf(Props(AsActor(new AuthenticationQueryExecutor[V, Id](testHashRepository))))
//    val akkaCqrsAuthenticationService = new AuthenticationService[FV](CommandQueryController.actor[V](authCommandHandler, authQueryExecutor))
//
//    whenReady(akkaCqrsAuthenticationService.createNewUser(UserLogin("test@test.com"), Password("password"))) { result1 =>
//      result1 should equal (scalaz.Success(Acknowledged))
//      whenReady(akkaCqrsAuthenticationService.authenticate(UserLogin("test@test.com"), Password("password"))) {result2 =>
//        result2 should equal (scalaz.Success(UserLogin("test@test.com")))
//      }
//      whenReady(akkaCqrsAuthenticationService.createNewUser(UserLogin("test@test.com"), Password("password"))) { result3 =>
//        result3 should equal (scalaz.Failure(scalaz.NonEmptyList("UserCredentialsAlreadyExistFailure(userLogin)")))
//      }
//    }
//  }
//
//  test("CQRS actor authentication piping from actor repository service") {
//    val atomicHashMap = new AtomicReference[Map[UserLogin, UserCredentials]](Map.empty[UserLogin, UserCredentials])
//    val repositoryCommandHandler = system.actorOf(Props(AsActor(new InMemoryKeyValueCommandHandler[Id, UserLogin, UserCredentials](atomicHashMap))))
//    val repositoryQueryExecutor = system.actorOf(Props(AsActor[Id,Kvqe[UserLogin, UserCredentials]#I](new InMemoryKeyValueQueryExecutor[Id, UserLogin, UserCredentials](atomicHashMap))))
//    val akkaCqrsRepository = new KeyValueCqrsRepository[Future, UserLogin, UserCredentials](
//      CommandQueryController.actor[Id][KeyValueCommand[UserLogin, UserCredentials], Kvqe[UserLogin, UserCredentials]#I](repositoryCommandHandler, repositoryQueryExecutor)
//    )
//
//    val authCommandHandler = system.actorOf(Props(AsActor(new AuthenticationCommandHandler[FV, Future](akkaCqrsRepository, 10))))
//    val authQueryExecutor = system.actorOf(Props(AsActor(new AuthenticationQueryExecutor[FV, Future](akkaCqrsRepository))))
//    val akkaChainedCqrsAuthenticationService = new AuthenticationService[FV](CommandQueryController.actor[V](authCommandHandler, authQueryExecutor))
//
//    whenReady(akkaChainedCqrsAuthenticationService.createNewUser(UserLogin("test@test.com"), Password("password"))) { result1 =>
//      result1 should equal (scalaz.Success(Acknowledged))
//      whenReady(akkaChainedCqrsAuthenticationService.authenticate(UserLogin("test@test.com"), Password("password"))) {result2 =>
//        result2 should equal (scalaz.Success(UserLogin("test@test.com")))
//      }
//      whenReady(akkaChainedCqrsAuthenticationService.createNewUser(UserLogin("test@test.com"), Password("password"))) { result3 =>
//        result3 should equal (scalaz.Failure(scalaz.NonEmptyList("UserCredentialsAlreadyExistFailure(userLogin)")))
//      }
//    }
//  }

}
