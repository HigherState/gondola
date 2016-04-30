package gondola.repository

import gondola._
import std._
import java.util.concurrent.atomic.AtomicReference
import akka.actor.ActorSystem
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import akka.util.Timeout

/**
 * Created by Jamie Pullar on 25/06/2015.
 */
object RepositoryTests {

//  def main(args:Array[String]):Unit = {
//
//    implicit val system = ActorSystem("System")
//    implicit val exectionContext:ExecutionContext = system.dispatcher
//    implicit val globalTimeout:Timeout = 5.minutes
//
//
//    val atomicHashMap = new AtomicReference[Map[String, String]](Map.empty[String, String])
//
//    val c = InMemoryKeyValueCommandHandler[Id, String, String](atomicHashMap)
//    val q = InMemoryKeyValueQueryExecutor[Id, String, String](atomicHashMap)
//    val cA = ActorN[(KvC[String, String])#I, Id](c)
//    val qA = new ActorTransform[(KvQ[String, String])#I, Id](q, None)
//
//    val cqT = Couple[(KvD[String, String])#I, (KvC[String, String])#I, (KvQ[String,String])#I, Future](cA, qA)
//
//    for {
//      _ <- c(Add("one" -> "two"))
//      v <- q(Get("one"))
//    } yield println(v)
//  }
}



