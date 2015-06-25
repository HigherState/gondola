package gondola.std

import scala.concurrent.{CanAwait, ExecutionContext, Future}
import gondola._
import gondola.Reader
import scala.util.Try
import scala.concurrent.duration.Duration
import scalaz.{Success, NonEmptyList, Failure, \/-, -\/}

object IdMonad {
  implicit val idMonad = scalaz.Id.id
}

object ValidMonads extends ValidMonads

object DisrupterMonads extends DisrupterMonads

object FutureMonads extends FutureMonads

trait ValidMonads {

  implicit def validMonad[E] = new FMonad[E, ({type V[+T] = Valid[E,T]})#V] {
    def bind[A, B](fa: Valid[E, A])(f: (A) => Valid[E, B]): Valid[E, B] =
      fa.flatMap(f)

    def point[A](a: => A): Valid[E, A] =
      Success(a)

    def failures(validationFailures: => NonEmptyList[E]): Valid[E, Nothing] =
      Failure(validationFailures)

    def failure(validationFailure: => E): Valid[E, Nothing] =
      Failure(NonEmptyList(validationFailure))

    def onFailure[T, S >: T](value: Valid[E, T])(f: (NonEmptyList[E]) => Valid[E, S]):Valid[E, S] =
      value match {
        case Failure(n) => f(n)
        case e => e
      }
  }
}

trait DisrupterMonads {
  implicit def disruptMonad[E] = new FMonad[E, ({type V[+T] = EitherValid[E,T]})#V] {
    def bind[A, B](fa: EitherValid[E, A])(f: (A) => EitherValid[E, B]): EitherValid[E, B] =
      fa.flatMap(f)
    def point[A](a: => A): EitherValid[E, A] =
      \/-(a)

    def failure(validationFailure: => E): EitherValid[E, Nothing] =
      -\/(NonEmptyList(validationFailure))

    def failures(validationFailures: => NonEmptyList[E]): EitherValid[E, Nothing] =
      -\/(validationFailures)

    def onFailure[T, S >: T](value: EitherValid[E, T])(f: (NonEmptyList[E]) => EitherValid[E, S]):EitherValid[E, S] =
      value match {
        case -\/(n) => f(n)
        case e => e
      }
  }
}

case class FutureLift[T](t: T) extends Future[T] {

  override def isCompleted: Boolean = true

  def value = Some(util.Success(t))

  def tryComplete(value: Try[T]): Boolean = false

  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext) {
    func(util.Success(t))
  }

  override def map[S](f: (T) => S)(implicit executor: ExecutionContext): Future[S] =
    FutureLift(f(t))

  override def flatMap[S](f: (T) => Future[S])(implicit executor: ExecutionContext): Future[S] =
    f(t)

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

  def result(atMost: Duration)(implicit permit: CanAwait): T = t
}

trait FutureMonads extends ValidMonads with DisrupterMonads {
  implicit def futureMonad(implicit ec:ExecutionContext):Monad[scala.concurrent.Future] =
    new Monad[Future] {
      def point[A](a: => A): Future[A] = FutureLift(a)
      def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa flatMap f
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
    }

  implicit def futureValidMonad[E](implicit monad:Monad[Future]):FMonad[E, ({type V[+T] = FutureValid[E,T]})#V] =
    new FMonad[E, ({type V[+T] = FutureValid[E,T]})#V] {
      def bind[A, B](fa: FutureValid[E, A])(f: (A) => FutureValid[E, B]): FutureValid[E, B] =
        monad.bind(fa) {
          case Failure(vf) =>
            monad.point(validMonad.failures(vf))
          case Success(s) => f(s)
        }

      def point[A](a: => A): FutureValid[E, A] =
        monad.point(validMonad.point(a))

      def failure(validationFailure: => E): FutureValid[E, Nothing] =
        monad.point(validMonad.failure(validationFailure))

      def failures(validationFailures: => NonEmptyList[E]):FutureValid[E, Nothing] =
        monad.point(validMonad.failures(validationFailures))

      def onFailure[T, S >: T](value: FutureValid[E, T])(f: (NonEmptyList[E]) => FutureValid[E, S]):FutureValid[E, S] =
        monad.bind(value) {
          case Failure(vf) =>
            f(vf)
          case _ => value
        }
    }

  implicit def futureDisruptMonad[E](implicit monad:Monad[Future]):FMonad[E, ({type V[+T] = FutureEitherValid[E,T]})#V] =
    new FMonad[E, ({type V[+T] = FutureEitherValid[E,T]})#V] {
      def bind[A, B](fa: FutureEitherValid[E, A])(f: (A) => FutureEitherValid[E, B]): FutureEitherValid[E, B] =
        monad.bind(fa) {
          case -\/(vf) =>
            monad.point(disruptMonad.failures(vf))
          case \/-(s) => f(s)
        }

      def point[A](a: => A): FutureEitherValid[E, A] =
        monad.point(disruptMonad.point(a))

      def failure(validationFailure: => E): FutureEitherValid[E, Nothing] =
        monad.point(disruptMonad.failure(validationFailure))

      def failures(validationFailures: => NonEmptyList[E]):FutureEitherValid[E, Nothing] =
        monad.point(disruptMonad.failures(validationFailures))

      def onFailure[T, S >: T](value: FutureEitherValid[E, T])(f: (NonEmptyList[E]) => FutureEitherValid[E, S]):FutureEitherValid[E, S] =
        monad.bind(value) {
          case -\/(vf) =>
            f(vf)
          case e => monad.point(e)
        }
    }
}



trait ReaderMonads extends ValidMonads with FutureMonads {
  implicit def readerMonad[F] = new Monad[({type R[+T] = Reader[F,T]})#R] {
    def bind[A, B](fa:Reader[F,A])(f: (A) => Reader[F,B]):Reader[F,B] =
      fa.flatMap(f)

    def point[A](a: => A) =
      ReaderFacade(a)
  }

  implicit def readerValidMonad[F,E] = new FMonad[E, ({type RV[+T] = ReaderValid[F,E,T]})#RV] {

    def bind[A,B](fa:ReaderValid[F,E,A])(f: (A) => ReaderValid[F,E,B]):ReaderValid[F,E,B] =
      fa.flatMap{
        case Failure(vf) =>
          readerMonad.point(validMonad.failures(vf))
        case Success(s) =>
          f(s)
      }

    def point[A](a: => A)=
      readerMonad.point(Success(a))

    def onFailure[T, S >: T](value: ReaderValid[F, E, T])(f: (NonEmptyList[E]) => ReaderValid[F, E, S]) =
      value.flatMap{
        case Failure(vf) =>
          f(vf)
        case _ => value
      }

    def failures(validationFailures: => NonEmptyList[E]) =
      readerMonad.point(validMonad.failures(validationFailures))

    def failure(validationFailure: => E) =
      readerMonad.point(validMonad.failure(validationFailure))
  }

  implicit def readerFutureMonad[F](implicit monad:Monad[Future]):Monad[({type RF[+T] = ReaderFuture[F, T]})#RF] =
    new Monad[({type RF[+T] = ReaderFuture[F, T]})#RF] {

      def bind[A, B](fa:ReaderFuture[F, A])(f: (A) => ReaderFuture[F,B]):ReaderFuture[F,B] =
        readerMonad.bind(fa) { ft =>
          Reader{t =>
            monad.bind(ft) { a =>
              f(a)(t)
            }
          }
        }

      def point[A](a: => A) =
        readerMonad.point(monad.point(a))
    }

  implicit def readerFutureValidMonad[F,E](implicit monad:Monad[Future]):FMonad[E, ({type RFV[+T] = ReaderFutureValid[F,E,T]})#RFV] =
    new FMonad[E, ({type RFV[+T] = ReaderFutureValid[F,E,T]})#RFV]{
      def bind[A, B](fa:ReaderFutureValid[F,E,A])(f: (A) => ReaderFutureValid[F,E,B]) =
        fa.flatMap{ft =>
          Reader{t =>
            monad.bind(ft){
              case Failure(vf) =>
                monad.point(validMonad.failures(vf))
              case Success(s) =>
                f(s).apply(t)
            }
          }
        }

      def point[A](a: => A) =
        readerMonad.point(monad.point(Success(a)))

      def onFailure[T, S >: T](value:ReaderFutureValid[F,E,T])(f: (NonEmptyList[E]) => ReaderFutureValid[F,E,S]) =
        value.flatMap{ ft =>
          Reader{t =>
            monad.bind(ft) {
              case Failure(vf) =>
                f(vf)(t)
              case Success(s) =>
                ft
            }
          }
        }

      def failures(validationFailures: => NonEmptyList[E])=
        readerMonad.point(monad.point(validMonad.failures(validationFailures)))


      def failure(validationFailure: => E) =
        readerMonad.point(monad.point(validMonad.failure(validationFailure)))
    }
}

