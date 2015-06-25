package gondola.authentication

import org.scalatest.{BeforeAndAfter, Matchers, FunSuite}
import org.scalatest.concurrent.ScalaFutures
import akka.actor.ActorSystem
import gondola._
import akka.util.Timeout
import repository._
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Future, ExecutionContext}

import gondola.std._

class InstanceTests extends FunSuite with Matchers with ScalaFutures with BeforeAndAfter {

  import scala.concurrent.duration._
  import Transforms._
  import IdMonad._

  implicit val system = ActorSystem("System")
  implicit val exectionContext:ExecutionContext = system.dispatcher
  implicit val globalTimeout:Timeout = 5.minutes

  type FV[+T] = FutureValid[String, T]
  type V[+T] = Valid[String, T]




  test("CQRS actor implementation on repository with a future handling on the authentication service") {
    //CQRS actors repository service.
    //Single command handler actor and single query executor actor
    val atomicHashMap = new AtomicReference[Map[UserLogin, UserCredentials]](Map.empty[UserLogin, UserCredentials])

    val c = new InMemoryKeyValueCommandHandler[Id, UserLogin, UserCredentials](atomicHashMap)
    val q = new InMemoryKeyValueQueryExecutor[Id, UserLogin, UserCredentials](atomicHashMap)
    val cA = new ActorTransform[(KvC[UserLogin, UserCredentials])#I, Id](c)
    val qA = new ActorTransform[(KvQ[UserLogin, UserCredentials])#I, Id](q)

    val cqT = CQTransform[(KvD[UserLogin, UserCredentials])#I, (KvC[UserLogin, UserCredentials])#I, (KvQ[UserLogin, UserCredentials])#I, Future](cA, qA)
    val compose = cqT.>>>[FV]
    val authCommandHandler = new AuthenticationCommandHandler[FV](compose, 10)
    val authQueryExecutor = new AuthenticationQueryExecutor[FV](compose)

    val cqT2 = CQTransform(authCommandHandler, authQueryExecutor)
    val futureAuthenticationService = new FutureActorTransform[AuthenticationDomain, V](cqT2)


    //Authentication service will handle futures from repository actor

    whenReady(futureAuthenticationService(CreateNewUser(UserLogin("test@test.com"), Password("password")))) { result1 =>
      result1 should equal (scalaz.Success(Acknowledged))
      whenReady(futureAuthenticationService(Authenticate(UserLogin("test@test.com"), Password("password")))) {result2 =>
        result2 should equal (scalaz.Success(UserLogin("test@test.com")))
      }
      whenReady(futureAuthenticationService(CreateNewUser(UserLogin("test@test.com"), Password("password")))) { result3 =>
        result3 should equal (scalaz.Failure(scalaz.NonEmptyList("UserCredentialsAlreadyExistFailure(userLogin)")))
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
