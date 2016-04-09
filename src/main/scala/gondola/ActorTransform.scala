package gondola

import scala.concurrent.Future
import akka.actor._
import akka.util.Timeout

private class ActorTransform[-D[_], R[_]](transform: => D ~> R, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[T] = Future[R[T]]})#I] {

  import akka.pattern._

  private val f = () => transform

  def props =
    Props {
      val t = f()
      new Actor {
        def receive = {
          case d: D[_]@unchecked =>
            sender ! t(d)
        }
      }
    }
  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

private class FutureActorTransform[-D[_], R[_]](transform: => D ~> ({type I[T] = Future[R[T]]})#I, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[T] = Future[R[T]]})#I] {
  import akka.pattern._

  private val f = () => transform

  def props =
    Props {
      val t = f()
      new Actor {
        import context.dispatcher

        def receive = {
          case d: D[_]@unchecked =>
            new PipeableFuture(t(d)).pipeTo(sender)
            ()
        }
      }
    }
  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    actorRef.ask(fa).asInstanceOf[Future[R[A]]]
}

private class ReaderActorTransform[-D[_], R[_], S](transform: => D ~> ({type I[T] = Reader[S, R[T]]})#I, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout)
  extends ~>[D, ({type I[T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._

  private val f = () => transform

  def props =
    Props{
      val t = f()
      new Actor {
        def receive = {
          case (d:D[_]@unchecked, s:S@unchecked) =>
            sender ! t(d).apply(s)
        }
      }
    }

  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorRef.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

private class SelectionTransform[-D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[T] = Future[R[T]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    actorSelection.ask(fa).asInstanceOf[Future[R[A]]]
}

private class ReaderSelectionTransform[-D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout) extends ~>[D, ({type I[T] = Reader[S, Future[R[T]]]})#I] {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    Reader((s:S) => actorSelection.ask(fa -> s).asInstanceOf[Future[R[A]]])
}

//class PropsActorTransform[D[_], R[_], S](transform: => D ~> R, scaffold:Actor => ActorRef)(implicit af:ActorContext, timeout:Timeout) extends ~>[D, ({type I[T] = Future[R[T]]})#I] {
//
//}

object ActorN {

  def apply[D[_], R[_]](transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout):(D ~> ({type I[T] = Future[R[T]]})#I) =
    new ActorTransform[D, R](transform, None)

  def apply[D[_], R[_]](transform: => D ~> R, name:String)(implicit af:ActorRefFactory, timeout:Timeout):(D ~> ({type I[T] = Future[R[T]]})#I) =
    new ActorTransform[D, R](transform, Some(name))

  def selection[D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[T] = Future[R[T]]})#I) =
    new SelectionTransform[D, R](actorSelection)

  object Future {

    def apply[D[_], R[_]](transform: => D ~> ({type I[T] = Future[R[T]]})#I)(implicit af: ActorRefFactory, timeout: Timeout): (D ~> ({type I[T] = Future[R[T]]})#I) =
      new FutureActorTransform[D, R](transform, None)

    def apply[D[_], R[_]](transform: => D ~> ({type I[T] = Future[R[T]]})#I, name: String)(implicit af: ActorRefFactory, timeout: Timeout): (D ~> ({type I[T] = Future[R[T]]})#I) =
      new FutureActorTransform[D, R](transform, Some(name))
  }

  object Reader {
    def apply[D[_], R[_], S](transform: => D ~> ({type I[T] = Reader[S, R[T]]})#I)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[T] = Reader[S, Future[R[T]]]})#I) =
      new ReaderActorTransform[D, R, S](transform, None)

    def apply[D[_], R[_], S](transform: => D ~> ({type I[T] = Reader[S, R[T]]})#I, name:String)(implicit af:ActorRefFactory, timeout:Timeout): (D ~> ({type I[T] = Reader[S, Future[R[T]]]})#I) =
      new ReaderActorTransform[D, R, S](transform, Some(name))

    def selection[D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout): ~>[D, ({type I[T] = Reader[S, Future[R[T]]]})#I] =
      new ReaderSelectionTransform[D, R, S](actorSelection)
  }
}


object ActorListener {

  def apply[E <: Event](listener: => EventListener[E])(implicit af:ActorRefFactory):ActorRef =
    af.actorOf(props(listener))

  def apply[E <: Event](listener: => EventListener[E], name:String)(implicit af:ActorRefFactory):ActorRef =
    af.actorOf(props(listener), name)

  private def props[E <: Event](listener: => EventListener[E]) = {
    val f = () => listener
    Props {
      new Actor {
        private val lifted = f().handle.lift

        def receive = {
          case e: E@unchecked =>
            lifted(e)
            ()
        }
      }
    }
  }
}

object ActorListenerN {

  def apply[M[_], N[_], E <: Event](listener: => EventListenerN[M, E])(implicit af: ActorRefFactory, transform: M ~> N): ActorRef =
    af.actorOf(props(listener))

  def apply[M[_], N[_], E <: Event](listener: => EventListenerN[M, E], name: String)(implicit af: ActorRefFactory, transform: M ~> N): ActorRef =
    af.actorOf(props(listener), name)

  def props[M[_], N[_], E <: Event](listener: => EventListenerN[M, E])(implicit transform: M ~> N) = {
    val f = () => listener
    Props {
      new Actor {
        private val lifted = f().handle.lift

        def receive = {
          case e: E@unchecked =>
            lifted(e).map(transform.apply)
            ()
        }
      }
    }
  }

  def passthrough[M[_], E <: Event](listener: => EventListenerN[M, E])(implicit af: ActorRefFactory): ActorRef =
    af.actorOf(passthroughProps(listener))

  def passthrough[M[_], E <: Event](listener: => EventListenerN[M, E], name: String)(implicit af: ActorRefFactory): ActorRef =
    af.actorOf(passthroughProps(listener), name)

  def passthroughProps[M[_], E <: Event](listener: => EventListenerN[M, E]) = {
    val f = () => listener
    Props {
      new Actor {
        private val lifted = f().handle.lift

        def receive = {
          case e: E@unchecked =>
            try {
              val result = lifted(e)
              sender ! result
            } catch {
              case ex:Throwable =>
                //return to parent and throw to activate supervisor
                sender ! akka.actor.Status.Failure(ex)
                throw ex
            }
        }
      }
    }
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



