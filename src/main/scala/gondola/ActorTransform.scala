package gondola

import scala.concurrent.Future
import akka.actor._
import akka.util.Timeout

private class ActorTransform[-D[_], R[+_]](transform: => D ~> R, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {

  import akka.pattern._

  def props =
    Props {
      new Actor {
        def receive = {
          case d: D[_]@unchecked =>
            sender ! transform(d)
        }
      }
    }
  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

private class FutureActorTransform[-D[_], R[+_]](transform: => D ~> ({type I[+T] = Future[R[T]]})#I, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
  import akka.pattern._

  def props =
    Props {
      new Actor {
        import context.dispatcher

        def receive = {
          case d: D[_]@unchecked =>
            new PipeableFuture(transform(d)) pipeTo sender
        }
      }
    }
  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

private class ReaderActorTransform[-D[_], R[+_], S](transform: => D ~> ({type I[+T] = Reader[S, R[T]]})#I, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[+T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._

  def props =
    Props{ new Actor {
      def receive = {
        case (d:D[_]@unchecked, s:S@unchecked) =>
          sender ! transform(d).apply(s)
      }
    }}

  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorRef.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

private class SelectionTransform[-D[_], R[+_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    actorSelection.ask(fa).asInstanceOf[Future[R[A]]]
}

private class ReaderSelectionTransform[-D[_], R[+_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[+T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorSelection.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

//class PropsActorTransform[D[_], R[+_], S](transform: => D ~> R, scaffold:Actor => ActorRef)(implicit af:ActorContext, timeout:Timeout) extends ~>[D, ({type I[+T] = Future[R[T]]})#I] {
//
//}

object ActorN {

  def apply[D[_], R[+_]](transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout):(D ~> ({type I[+T] = Future[R[T]]})#I) =
    new ActorTransform[D, R](transform, None)

  def apply[D[_], R[+_]](transform: => D ~> R, name:String)(implicit af:ActorRefFactory, timeout:Timeout):(D ~> ({type I[+T] = Future[R[T]]})#I) =
    new ActorTransform[D, R](transform, Some(name))

  def selection[D[_], R[+_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[+T] = Future[R[T]]})#I) =
    new SelectionTransform[D, R](actorSelection)

  object Future {

    def apply[D[_], R[+ _]](transform: => D ~> ({type I[+T] = Future[R[T]]})#I)(implicit af: ActorRefFactory, timeout: Timeout): (D ~> ({type I[+T] = Future[R[T]]})#I) =
      new FutureActorTransform[D, R](transform, None)

    def apply[D[_], R[+ _]](transform: => D ~> ({type I[+T] = Future[R[T]]})#I, name: String)(implicit af: ActorRefFactory, timeout: Timeout): (D ~> ({type I[+T] = Future[R[T]]})#I) =
      new FutureActorTransform[D, R](transform, Some(name))
  }

  object Reader {
    def apply[D[_], R[+_], S](transform: => D ~> ({type I[+T] = Reader[S, R[T]]})#I)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[+T] = Reader[S, Future[R[T]]]})#I) =
      new ReaderActorTransform[D, R, S](transform, None)

    def apply[D[_], R[+_], S](transform: => D ~> ({type I[+T] = Reader[S, R[T]]})#I, name:String)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[+T] = Reader[S, Future[R[T]]]})#I) =
      new ReaderActorTransform[D, R, S](transform, Some(name))

    def selection[D[_], R[+_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout): ~>[D, ({type I[+T] = Reader[S, Future[R[T]]]})#I] =
      new ReaderSelectionTransform[D, R, S](actorSelection)
  }
}

//TODO: FIX THIS!!
object Couple {
  def apply[D[_], C[_] <: D[_], Q[_] <: D[_], R[_]](cTransform:C ~> R, qTransform:Q ~> R): (D ~> R) =
    new ~>[D, R] {
      def apply[A](fa: D[A]): R[A] = {
        if (fa.getClass.getInterfaces.exists(f => f.getSimpleName.contains("Command")))
          cTransform(fa.asInstanceOf[C[A]])
        else
          qTransform(fa.asInstanceOf[Q[A]])
      }
    }
}

