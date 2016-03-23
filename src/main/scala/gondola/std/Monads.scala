package gondola.std

import scala.concurrent.{CanAwait, ExecutionContext, Future}
import gondola._
import gondola.Reader
import gondola.Writer
import gondola.Monad
import scala.util.Try
import scala.concurrent.duration.Duration
import scalaz._

object IdMonad {
  implicit val idMonad = scalaz.Id.id
}

object ValidMonads extends ValidMonads


object FutureMonads extends FutureMonads

trait WriterMonads {

  implicit def writerMonad[F:Monoid] = new WMonad[F, ({type R[+T] = Writer[F, T]})#R] {

    def write[T](log: F, value: T): Writer[F, T] =
      Writer(log, value)

    def point[A](a: => A): Writer[F, A] =
      Writer.zero(a)

    def bind[A, B](fa: Writer[F, A])(f: (A) => Writer[F, B]): Writer[F, B] =
      fa.flatMap(f)
  }

}

trait ValidMonads extends WriterMonads {

  implicit def validMonad[E] = new FMonad[E, ({type V[+T] = Valid[E,T]})#V] {
    def bind[A, B](fa: Valid[E, A])(f: (A) => Valid[E, B]): Valid[E, B] =
      fa.flatMap(f)

    def point[A](a: => A): Valid[E, A] =
      \/-(a)

    def failures(validationFailures: => NonEmptyList[E]): Valid[E, Nothing] =
      -\/(validationFailures)

    def failure(validationFailure: => E): Valid[E, Nothing] =
      -\/(NonEmptyList(validationFailure))

    def onFailure[T, S >: T](value: Valid[E, T])(f: (NonEmptyList[E]) => Valid[E, S]):Valid[E, S] =
      value match {
        case -\/(n) => f(n)
        case e => e
      }
  }

  implicit def validWriterMonad[E, F:Monoid] = new FWMonad[E, F, ({type R[+T] = ValidWriter[E, F, T]})#R] {

    def write[T](log:F, value:T): ValidWriter[E, F, T] =
      \/-(Writer(log, value))

    def failure(validationFailure: => E): ValidWriter[E, F, Nothing] =
      validMonad.failure(validationFailure)

    def failures(validationFailures: => NonEmptyList[E]): ValidWriter[E, F, Nothing] =
      validMonad.failures(validationFailures)

    def onFailure[T, S >: T](value: ValidWriter[E, F, T])(f: (NonEmptyList[E]) => ValidWriter[E, F, S]): ValidWriter[E, F, S] =
      value match {
        case \/-(_) =>
          value
        case -\/(vf) =>
          f(vf)
      }

    def point[A](a: => A): ValidWriter[E, F, A] =
      validMonad.point(writerMonad.point(a))

    def bind[A, B](fa: ValidWriter[E, F, A])(f: (A) => ValidWriter[E, F, B]): ValidWriter[E, F, B] =
      fa.flatMap{v =>
        f(v.value).map{w =>
          v.flatMap(_ => w)
        }
      }
  }
}

case class FutureLift[T](t: T) extends Future[T] {

  override def isCompleted: Boolean = true

  def value = Some(util.Success(t))

  def tryComplete(value: Try[T]): Boolean = false

  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext):Unit = {
    func(util.Success(t))
    ()
  }

  override def map[S](f: (T) => S)(implicit executor: ExecutionContext): Future[S] =
    FutureLift(f(t))

  override def flatMap[S](f: (T) => Future[S])(implicit executor: ExecutionContext): Future[S] =
    f(t)

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

  def result(atMost: Duration)(implicit permit: CanAwait): T = t
}

trait FutureMonads extends ValidMonads {
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
          case -\/(vf) =>
            monad.point(validMonad.failures(vf))
          case \/-(s) => f(s)
        }

      def point[A](a: => A): FutureValid[E, A] =
        monad.point(validMonad.point(a))

      def failure(validationFailure: => E): FutureValid[E, Nothing] =
        monad.point(validMonad.failure(validationFailure))

      def failures(validationFailures: => NonEmptyList[E]):FutureValid[E, Nothing] =
        monad.point(validMonad.failures(validationFailures))

      def onFailure[T, S >: T](value: FutureValid[E, T])(f: (NonEmptyList[E]) => FutureValid[E, S]):FutureValid[E, S] =
        monad.bind(value) {
          case -\/(vf) =>
            f(vf)
          case _ => value
        }
    }

  implicit def futureValidMonadWriter[E, L](implicit monad:Monad[Future], monoid:Monoid[L]):FWMonad[E, L, ({type V[+T] = FutureValidWriter[E,L,T]})#V] =
    new FWMonad[E, L, ({type V[+T] = FutureValidWriter[E,L,T]})#V] {
      def write[T](log: L, value: T): FutureValidWriter[E, L, T] =
        monad.point(\/-(Writer(log, value)))

      def failure(validationFailure: => E): FutureValidWriter[E, L, Nothing] =
        monad.point(validMonad.failure(validationFailure))

      def failures(validationFailures: => NonEmptyList[E]): FutureValidWriter[E, L, Nothing] =
        monad.point(validMonad.failures(validationFailures))

      def onFailure[T, S >: T](value: FutureValidWriter[E, L, T])(f: (NonEmptyList[E]) => FutureValidWriter[E, L, S]): FutureValidWriter[E, L, S] =
        monad.bind(value) {
          case -\/(vf) =>
            f(vf)
          case _ => value
        }

      def point[A](a: => A): FutureValidWriter[E, L, A] =
        monad.point(validMonad.point(writerMonad.point(a)))

      def bind[A, B](fa: FutureValidWriter[E, L, A])(f: (A) => FutureValidWriter[E, L, B]): FutureValidWriter[E, L, B] =
        monad.bind(fa) {
          case -\/(vf) =>
            monad.point(validMonad.failures(vf))
          case \/-(w@Writer(_, s)) =>
            monad.map(f(s)){
              case -\/(vf) =>
                validMonad.failures(vf)
              case \/-(r) =>
                validMonad.point(w.flatMap(_ => r))
            }
        }
    }
}



trait IOMonads extends ValidMonads with WriterMonads {
  implicit val ioMonad = new Monad[({type R[+T] = IO[T]})#R] {
    def bind[A, B](fa:IO[A])(f: (A) => IO[B]):IO[B] =
      fa.flatMap(f)

    def point[A](a: => A) =
      IO(a)
  }

  implicit def ioValidMonad[E] = new FMonad[E, ({type RV[+T] = IOValid[E,T]})#RV] {
    def bind[A, B](fa: IOValid[E,A])(f: (A) => IOValid[E,B]):IOValid[E, B] =
      fa.flatMap{
        case -\/(vf) =>
          ioMonad.point(validMonad.failures(vf))
        case \/-(s) =>
          f(s)
      }

    def point[A](a: => A) =
      ioMonad.point(\/-(a))

    def onFailure[T, S >: T](value: IOValid[E,T])(f: (NonEmptyList[E]) => IOValid[E,S]) =
      value.flatMap{
        case -\/(vf) =>
          f(vf)
        case _ => value
      }

    def failures(validationFailures: => NonEmptyList[E]) =
      ioMonad.point(validMonad.failures(validationFailures))

    def failure(validationFailure: => E) =
      ioMonad.point(validMonad.failure(validationFailure))
  }

  implicit def ioWriterMonad[L](implicit monoid:Monoid[L]) = new WMonad[L, ({type IW[+T] = IOWriter[L, T]})#IW] {

    override def write[T](log: L, value: T): IOWriter[L, T] =
      IO(Writer(log, value))

    def point[A](a: => A): IOWriter[L, A] = IO(Writer.zero(a))

    def bind[A, B](fa: IOWriter[L, A])(f: (A) => IOWriter[L, B]): IOWriter[L, B] =
      fa.flatMap{w =>
        f(w.value).map{w2 =>
          Writer(monoid.append(w.log, w2.log), w2.value)
        }
      }
  }

  implicit def ioValidWriterMonad[E, L](implicit monoid:Monoid[L]) = new FWMonad[E, L, ({type IWV[+T] = IOValidWriter[E, L, T]})#IWV] {

    def write[T](log: L, value: T): IOValidWriter[E, L, T] =
      IO(\/-(Writer(log, value)))

    def point[A](a: => A): IOValidWriter[E, L, A] =
      IO(\/-(Writer.zero(a)))

    def failure(validationFailure: => E): IOValidWriter[E, L, Nothing] =
      IO(-\/(NonEmptyList(validationFailure)))

    def failures(validationFailures: => NonEmptyList[E]): IOValidWriter[E, L, Nothing] =
      IO(-\/(validationFailures))

    def onFailure[T, S >: T](value: IOValidWriter[E, L, T])(f: (NonEmptyList[E]) => IOValidWriter[E, L, S]): IOValidWriter[E, L, S] =
      value.flatMap{
        case -\/(vf) =>
          f(vf)
        case \/-(_) =>
          value
      }

    def bind[A, B](fa: IOValidWriter[E, L, A])(f: (A) => IOValidWriter[E, L, B]): IOValidWriter[E, L, B] =
      fa.flatMap{
        case -\/(_) =>
          fa.asInstanceOf[IOValidWriter[E, L, B]]
        case \/-(Writer(l, v)) =>
          f(v).map(_.map(w => Writer(monoid.append(l, w.log), w.value)))
      }
  }
}

trait ReaderMonads extends ValidMonads with FutureMonads with WriterMonads{

  implicit def readerMonad[F] = new Monad[({type R[+T] = Reader[F,T]})#R] {
    def bind[A, B](fa:Reader[F,A])(f: (A) => Reader[F,B]):Reader[F,B] =
      fa.flatMap(f)

    def point[A](a: => A) =
      ReaderFacade(a)
  }

  implicit def readerValidMonad[F,E] = new FMonad[E, ({type RV[+T] = ReaderValid[F,E,T]})#RV] {

    def bind[A,B](fa:ReaderValid[F,E,A])(f: (A) => ReaderValid[F,E,B]):ReaderValid[F,E,B] =
      fa.flatMap{
        case -\/(vf) =>
          readerMonad.point(validMonad.failures(vf))
        case \/-(s) =>
          f(s)
      }

    def point[A](a: => A)=
      readerMonad.point(\/-(a))

    def onFailure[T, S >: T](value: ReaderValid[F, E, T])(f: (NonEmptyList[E]) => ReaderValid[F, E, S]) =
      value.flatMap{
        case -\/(vf) =>
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
              case -\/(vf) =>
                monad.point(validMonad.failures(vf))
              case \/-(s) =>
                f(s).apply(t)
            }
          }
        }

      def point[A](a: => A) =
        readerMonad.point(monad.point(\/-(a)))

      def onFailure[T, S >: T](value:ReaderFutureValid[F,E,T])(f: (NonEmptyList[E]) => ReaderFutureValid[F,E,S]) =
        value.flatMap{ ft =>
          Reader{t =>
            monad.bind(ft) {
              case -\/(vf) =>
                f(vf)(t)
              case \/-(s) =>
                ft
            }
          }
        }

      def failures(validationFailures: => NonEmptyList[E])=
        readerMonad.point(monad.point(validMonad.failures(validationFailures)))


      def failure(validationFailure: => E) =
        readerMonad.point(monad.point(validMonad.failure(validationFailure)))
    }

  implicit def readerWriterMonad[F, L](implicit monoid:Monoid[L]) = new WMonad[L, ({type IW[+T] = ReaderWriter[F, L, T]})#IW] {

    def write[T](log: L, value: T): ReaderWriter[F, L, T] =
      ReaderFacade(Writer(log, value))

    def point[A](a: => A): ReaderWriter[F, L, A] = ReaderFacade(Writer.zero(a))

    def bind[A, B](fa: ReaderWriter[F, L, A])(f: (A) => ReaderWriter[F, L, B]): ReaderWriter[F, L, B] =
      fa.flatMap{w =>
        f(w.value).map{w2 =>
          Writer(monoid.append(w.log, w2.log), w2.value)
        }
      }
  }

  implicit def readerValidWriterMonad[F, E, L](implicit monoid:Monoid[L]) = new FWMonad[E, L, ({type IVW[+T] = ReaderValidWriter[F, E, L, T]})#IVW] {

    def write[T](log: L, value: T): ReaderValidWriter[F, E, L, T] =
      ReaderFacade(\/-(Writer(log, value)))

    def failure(validationFailure: => E): ReaderValidWriter[F, E, L, Nothing] =
      ReaderFacade(-\/(NonEmptyList(validationFailure)))

    def failures(validationFailures: => NonEmptyList[E]): ReaderValidWriter[F, E, L, Nothing] =
      ReaderFacade(-\/(validationFailures))

    def onFailure[T, S >: T](value: ReaderValidWriter[F, E, L, T])(f: (NonEmptyList[E]) => ReaderValidWriter[F, E, L, S]): ReaderValidWriter[F, E, L, S] =
      value.flatMap{
        case \/-(w) =>
          ReaderFacade(\/-(w))  // returning value here causes loss of information, not sure why
        case -\/(vf) =>
          f(vf)
      }

    def point[A](a: => A): ReaderValidWriter[F, E, L, A] =
      ReaderFacade(\/-(Writer.zero(a)))

    def bind[A, B](fa: ReaderValidWriter[F, E, L, A])(f: (A) => ReaderValidWriter[F, E, L, B]): ReaderValidWriter[F, E, L, B] =
      fa.flatMap{
        case \/-(Writer(l,v)) =>
          f(v).map(_.map(w => Writer(monoid.append(l, w.log), w.value)))
        case -\/(vf) =>
          ReaderFacade(-\/(vf))
      }
  }
}

