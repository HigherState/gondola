import akka.actor.{ActorRef, ActorSelection}

package object gondola {
  trait TF {
    type I[Y[_]]
  }

  type EitherActor = Either[ActorRef, ActorSelection]
  type Monad[F[_]] = cats.Monad[F]

  implicit class AnyExt[T](val self:T) extends AnyVal {
    def |>[U](func:T => U) = func(self)
  }

  implicit class TupleExt[A,B](val a:(A,B)) extends AnyVal  {
    def |>[C](func:(A, B) => C) = func(a._1, a._2)
  }

  def acknowledged[M[_]](implicit m:Monad[M]):M[Ack] =
    m.pure(Acknowledged)

  implicit class MExt[M[_], T](val self:M[T]) extends AnyVal {
    def ~>[N[_]](implicit nt:M ~> N) =
      nt(self)
  }

  val Acknowledged:Ack = new Ack{}
}
