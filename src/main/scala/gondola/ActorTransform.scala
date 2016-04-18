package gondola

import cats.data.{Reader, ReaderT}
import cats.{Traverse, ~>}

import scala.concurrent.Future
import akka.actor._
import akka.util.Timeout
import gondola.std.FutureT

private class ActorTransform[D[_], R[_]](transform: => D ~> R, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
  extends (D ~> FutureT[R, ?]) {

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
    FutureT[R, A](actorRef.ask(fa).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T)
}

private class FutureActorTransform[D[_], R[_]](transform: => D ~> FutureT[R, ?], name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
  extends (D ~> FutureT[R, ?]) {
  import akka.pattern._

  private val f = () => transform

  def props =
    Props {
      val t = f()
      new Actor {
        import context.dispatcher

        def receive = {
          case d: D[_]@unchecked =>
            new PipeableFuture(t(d).value).pipeTo(sender)
            ()
        }
      }
    }
  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    FutureT[R, A](actorRef.ask(fa).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T)
}

private class ReaderActorTransform[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?], name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
  extends (D ~> ReaderT[FutureT[R, ?], S, ?]) {
  import akka.pattern._

  private val f = () => transform

  def props =
    Props{
      val t = f()
      new Actor {
        def receive = {
          case (d:D[_]@unchecked, s:S@unchecked) =>
            sender ! t(d).run(s)
        }
      }
    }

  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    ReaderT[FutureT[R, ?], S, A](s => FutureT[R, A](actorRef.ask(fa -> s).asInstanceOf[Future[R[A]]])(af.dispatcher, M ,T))
}

private class SelectionTransform[D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]) extends (D ~> FutureT[R, ?]) {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    FutureT(actorSelection.ask(fa).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T)
}

private class ReaderSelectionTransform[D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]) extends (D ~> ReaderT[FutureT[R, ?], S, ?]) {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    ReaderT[FutureT[R, ?], S, A](s => FutureT[R, A](actorSelection.ask(fa -> s).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T))
}



object ActorN {

  def apply[D[_], R[_]](transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new ActorTransform[D, R](transform, None)

  def apply[D[_], R[_]](transform: => D ~> R, name:String)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new ActorTransform[D, R](transform, Some(name))

  def selection[D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new SelectionTransform[D, R](actorSelection)

  object Future {

    def apply[D[_], R[_]](transform: => D ~> FutureT[R, ?])(implicit af: ActorRefFactory, timeout: Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
      new FutureActorTransform[D, R](transform, None)

    def apply[D[_], R[_]](transform: => D ~> FutureT[R, ?], name: String)(implicit af: ActorRefFactory, timeout: Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
      new FutureActorTransform[D, R](transform, Some(name))
  }

  object Reader {
    def apply[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]): D ~> ReaderT[FutureT[R, ?], S, ?] =
      new ReaderActorTransform[D, R, S](transform, None)

    def apply[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?], name:String)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> ReaderT[FutureT[R, ?], S, ?] =
      new ReaderActorTransform[D, R, S](transform, Some(name))

    def selection[D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]): D ~> ReaderT[FutureT[R, ?], S, ?] =
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



