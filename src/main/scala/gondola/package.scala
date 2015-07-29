import akka.actor.{ActorRef, ActorSelection}

package object gondola {
  trait TF {
    type I[Y[+_]]
  }
  type ~>![X[+_]] = TF { type I[Y[+_]] = ~>[Y, X] }
  type !~>[X[+_]] = TF { type I[Y[+_]] = ~>[X, Y] }

  type EitherActor = Either[ActorRef, ActorSelection]
  type Monad[F[_]] = scalaz.Monad[F]
  type Ack = Acknowledged

  type FWMonad[E, L, M[+_]] = FMonad[E, M] with WMonad[L, M]

  implicit class AnyExt[T](val self:T) extends AnyVal {
    def |>[U](func:T => U) = func(self)
  }

  implicit class TupleExt[A,B](val a:(A,B)) extends AnyVal  {
    def |>[C](func:(A, B) => C) = func(a._1, a._2)
  }

  def acknowledged[M[+_]](implicit m:Monad[M]):M[Acknowledged] =
    m.point(Acknowledged)

  implicit class MExt[M[+_], T](val self:M[T]) extends AnyVal {
    def ~>[N[+_]](implicit nt:M ~> N) =
      nt(self)
  }
}
