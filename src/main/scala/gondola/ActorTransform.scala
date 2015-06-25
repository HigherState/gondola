package gondola

import scala.concurrent.Future
import akka.actor._
import akka.util.Timeout

class ActorTransform[-D[_], R[+_]](transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
  import akka.pattern._

  val actorRef = af.actorOf(Props{ new Actor {
    def receive = {
      case d:D[_]@unchecked =>
        sender ! transform(d)
    }
  }})

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

class FutureActorTransform[-D[_], R[+_]](transform: => D ~> ({type I[+T] = Future[R[T]]})#I)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
  import akka.pattern._

  val actorRef = af.actorOf(Props{ new Actor {
    import context.dispatcher

    def receive = {
      case d:D[_]@unchecked =>
        new PipeableFuture(transform(d)) pipeTo sender
    }
  }})

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

class ReaderActorTransform[D[_], R[+_], S](transform: => D ~> ({type I[+T] = Reader[S, R[T]]})#I, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._

  val actorRef = af.actorOf(Props{ new Actor {
    def receive = {
      case (d:D[_]@unchecked, s:S@unchecked) =>
        sender ! transform(d).apply(s)
    }
  }})

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorRef.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

class SelectionTransform[D[_], R[+_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    actorSelection.ask(fa).asInstanceOf[Future[R[A]]]
}

class ReaderSelectionTransform[D[_], R[+_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorSelection.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

//class PropsActorTransform[D[_], R[+_], S](transform: => D ~> R, scaffold:Actor => ActorRef)(implicit af:ActorContext, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
//
//}

//TODO: FIX THIS!!
object CQTransform {
  def apply[D[_] <: Domain[_], C[_] <: D[_], Q[_] <: D[_], R[+_]](
   cTransform:C ~> R,
   qTransform:Q ~> R
   ) = new ~>[D, R] {
      def apply[A](fa: D[A]): R[A] = {
        if (fa.getClass.getInterfaces.exists(f => f.getSimpleName.contains("Command")))
          cTransform(fa.asInstanceOf[C[A]])
        else
          qTransform(fa.asInstanceOf[Q[A]])
      }
    }
}

