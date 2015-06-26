package gondola.std

import scala.concurrent.{Future, ExecutionContext}
import gondola._

trait IdentityTransforms extends ReaderMonads {

  implicit def directPipe[T[_]] = new (T ~> T) {

    def apply[A](fa: T[A]): T[A] = fa
  }

  //scala seems unable to resolve this as an implicit
  private def identityOutPipe[T[+_]:Monad]: ~>[Id, T] = new (Id ~> T) {

    def apply[A](fa: Id[A]): T[A] = implicitly[Monad[T]].point(fa)
  }

  implicit def idValidPipe[E]: (Id ~> ({type V[+T] = Valid[E,T]})#V) =
    identityOutPipe[({type V[+T] = Valid[E,T]})#V]

  implicit def idFuturePipe(implicit ex:ExecutionContext): (Id ~> scala.concurrent.Future) =
    identityOutPipe[scala.concurrent.Future]

  implicit def idReaderPipe[F]: (Id ~> ({type R[+T] = Reader[F,T]})#R) =
    identityOutPipe[({type R[+T] = Reader[F,T]})#R]

  implicit def idFutureValidPipe[E](implicit ex:ExecutionContext): (Id ~> ({type FV[+T] = FutureValid[E,T]})#FV) =
    identityOutPipe[({type V[+T] = FutureValid[E,T]})#V]

  implicit def idReaderValidPipe[F,E]: (Id ~> ({type RV[+T] = ReaderValid[F, E,T]})#RV) =
    identityOutPipe[({type RV[+T] = ReaderValid[F, E,T]})#RV]

  implicit def idReaderFuturePipe[F](implicit ex:ExecutionContext): (Id ~> ({type FR[+T] = ReaderFuture[F, T]})#FR) =
    identityOutPipe[({type FR[+T] = ReaderFuture[F, T]})#FR]

  implicit def idReaderFutureValidPipe[F,E](implicit ex:ExecutionContext): (Id ~> ({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV) =
    identityOutPipe[({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV]

}

trait ValidTransforms {

  implicit def ValidFutureValidPipe[E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type V[+T] = FutureValid[E,T]})#V) {

    def apply[T](value:Valid[E, T]) = FutureLift(value)
  }

  implicit def ValidReaderValid[F,E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type RV[+T] = ReaderValid[F, E,T]})#RV) {

    def apply[T](value: Valid[E, T]) = ReaderFacade(value)
  }

  implicit def ValidReaderFutureValid[F,E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type RV[+T] = ReaderFutureValid[F, E,T]})#RV) {

    def apply[T](value: Valid[E, T]) = ReaderFacade(FutureLift(value))
  }

}

trait FutureTransforms {
  import scala.concurrent.Future

  implicit def FutureFutureValidPipe[E](implicit ec:ExecutionContext) =
    new (Future ~> ({type V[+T] = FutureValid[E,T]})#V) {

      def apply[A](value: Future[A]) = value.map(scalaz.Success(_))
    }

  implicit def FutureReaderFuturePipe[F](implicit ec:ExecutionContext) =
    new (Future ~> ({type RF[+T] = ReaderFuture[F,T]})#RF) {

      def apply[A](value: Future[A]) = ReaderFacade(value)
    }

  implicit def FutureFutureReaderValidPipe[F, E](implicit ec:ExecutionContext) =
    new (Future ~> ({type RFV[+T] = ReaderFutureValid[F, E, T]})#RFV) {
      def apply[T](value: Future[T]) =
        ReaderFacade(value.map(scalaz.Success(_)))
    }

  implicit def FutureValidReaderFutureValid[F, E](implicit ec:ExecutionContext) =
    new (({type V[+T] = FutureValid[E,T]})#V ~> ({type RFV[+T] = ReaderFutureValid[F, E, T]})#RFV) {

      def apply[T](value: FutureValid[E, T]) =
        ReaderFacade(value)
    }
}

trait ReaderTransforms {

  implicit def ReaderReaderValid[F, E] = new (({type R[+T] = Reader[F,T]})#R ~> ({type RV[+T] = ReaderValid[F, E,T]})#RV) {

    def apply[T](value: Reader[F, T]) =
      value.map(scalaz.Success(_))
  }

  implicit def ReaderValidReaderFutureValid[F,E] = new (({type RV[+T] = ReaderValid[F, E,T]})#RV ~> ({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV) {

    def apply[T](value: ReaderValid[F, E, T]) =
      value.map(FutureLift(_))
  }

  implicit def ReaderFutureFutureReaderValid[F, E] (implicit ec:ExecutionContext) =
    new (({type FR[+T] = ReaderFuture[F,T]})#FR ~> ({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV) {

      def apply[T](value:ReaderFuture[F,T]) =
        value.map(_.map(scalaz.Success(_)))
    }
}

object Transforms extends IdentityTransforms with ValidTransforms with FutureTransforms with ReaderTransforms
