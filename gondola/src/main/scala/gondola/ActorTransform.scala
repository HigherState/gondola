package gondola

import cats.data.{ReaderT, StateT}
import cats.Traverse

import scala.concurrent.Future
import akka.actor._
import akka.util.Timeout
import gondola.std.FutureT

private class ActorTransformation[D[_], R[_]](transform: => D ~> R, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
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

private class FutureActorTransformation[D[_], R[_]](transform: => D ~> FutureT[R, ?], name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
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

private class ReaderActorTransformation[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?], name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R])
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

private class StateActorTransformation[D[_], R[_], R2[_], S](initial: => S, transform:D ~> R, name:Option[String])(implicit af:ActorRefFactory, timeout:Timeout, ST:StateTransformation[R, R2, S], M:Monad[R2], T:Traverse[R2])
  extends (D ~> FutureT[R2, ?]) {
  import akka.pattern._

  private val f = () => initial

  def props =
    Props{
      new Actor {
        private var state:S = f()
        def receive = {
          case d:D[_]@unchecked =>
            sender ! M.map(ST(transform(d), state)){r =>
              state = r._1
              r._2
            }
        }
      }
    }

  val actorRef = name.fold(af.actorOf(props)){n => af.actorOf(props, n)}

  def apply[A](fa: D[A]) =
    FutureT[R2, A](actorRef.ask(fa).asInstanceOf[Future[R2[A]]])(af.dispatcher, M, T)
}


private class SelectionTransformation[D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]) extends (D ~> FutureT[R, ?]) {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    FutureT(actorSelection.ask(fa).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T)
}

private class ReaderSelectionTransformation[D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]) extends (D ~> ReaderT[FutureT[R, ?], S, ?]) {
  import akka.pattern._
  import af.dispatcher

  def apply[A](fa: D[A]) =
    ReaderT[FutureT[R, ?], S, A](s => FutureT[R, A](actorSelection.ask(fa -> s).asInstanceOf[Future[R[A]]])(af.dispatcher, M, T))
}



object ActorN {

  def apply[D[_], R[_]](transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new ActorTransformation[D, R](transform, None)

  def apply[D[_], R[_]](transform: => D ~> R, name:String)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new ActorTransformation[D, R](transform, Some(name))

  def selection[D[_], R[_]](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
    new SelectionTransformation[D, R](actorSelection)

  object Future {

    def apply[D[_], R[_]](transform: => D ~> FutureT[R, ?])(implicit af: ActorRefFactory, timeout: Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
      new FutureActorTransformation[D, R](transform, None)

    def apply[D[_], R[_]](transform: => D ~> FutureT[R, ?], name: String)(implicit af: ActorRefFactory, timeout: Timeout, M:Monad[R], T:Traverse[R]):D ~> FutureT[R, ?] =
      new FutureActorTransformation[D, R](transform, Some(name))
  }

  object Reader {
    def apply[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?])(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]): D ~> ReaderT[FutureT[R, ?], S, ?] =
      new ReaderActorTransformation[D, R, S](transform, None)

    def apply[D[_], R[_], S](transform: => D ~> ReaderT[R, S, ?], name:String)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]):D ~> ReaderT[FutureT[R, ?], S, ?] =
      new ReaderActorTransformation[D, R, S](transform, Some(name))

    def selection[D[_], R[_], S](actorSelection:ActorSelection)(implicit af:ActorRefFactory, timeout:Timeout, M:Monad[R], T:Traverse[R]): D ~> ReaderT[FutureT[R, ?], S, ?] =
      new ReaderSelectionTransformation[D, R, S](actorSelection)
  }

  object State {
    def apply[D[_], R[_], R2[_], S](initial: => S, transform: => D ~> R)(implicit af:ActorRefFactory, timeout:Timeout, ST:StateTransformation[R, R2, S], M:Monad[R2], T:Traverse[R2]): D ~> FutureT[R2, ?] =
      new StateActorTransformation[D, R, R2, S](initial, transform, None)

    def apply[D[_], R[_], R2[_], S](initial: => S, transform: => D ~> R, name:String)(implicit af:ActorRefFactory, timeout:Timeout, ST:StateTransformation[R, R2, S], M:Monad[R2], T:Traverse[R2]): D ~> FutureT[R2, ?] =
      new StateActorTransformation[D, R, R2, S](initial, transform, Some(name))
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

  def apply[M[_], N[_], E <: Event](listener: => EventListenerN[M, E])(implicit af: ActorRefFactory, T: M ~> N): ActorRef =
    af.actorOf(props(listener))

  def apply[M[_], N[_], E <: Event](listener: => EventListenerN[M, E], name: String)(implicit af: ActorRefFactory, T: M ~> N): ActorRef =
    af.actorOf(props(listener), name)

  def props[M[_], N[_], E <: Event](listener: => EventListenerN[M, E])(implicit T: M ~> N) = {
    val f = () => listener
    Props {
      new Actor {
        private val lifted = f().handle.lift

        def receive = {
          case e: E@unchecked =>
            lifted(e).map(T.apply)
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
  def apply[D[_], C[_] <: D[_], Q[_] <: D[_], R[_]](CT:C ~> R, QT:Q ~> R): (D ~> R) =
    new ~>[D, R] {
      def apply[A](fa: D[A]): R[A] = {
        if (fa.getClass.getInterfaces.exists(f => f.getSimpleName.contains("Command")))
          CT(fa.asInstanceOf[C[A]])
        else
          QT(fa.asInstanceOf[Q[A]])
      }
    }
}



