import akka.actor.{ActorRef, ActorSelection}

package object gondola {
  trait TF {
    type I[Y[+_]]
  }
  type ~>![X[+_]] = TF { type I[Y[+_]] = ~>[Y, X] }

  type EitherActor = Either[ActorRef, ActorSelection]
  type Monad[F[_]] = scalaz.Monad[F]
  type Ack = Acknowledged

  implicit class AnyExt[T](val self:T) extends AnyVal {
    def |>[U](func:T => U) = func(self)
  }

  implicit class TupleExt[A,B](val a:(A,B)) extends AnyVal  {
    def |>[C](func:(A, B) => C) = func(a._1, a._2)
  }
}
