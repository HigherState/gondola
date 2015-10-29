package gondola.std

import scala.concurrent.ExecutionContext
import gondola._
import scala.language.higherKinds
import scala.language.reflectiveCalls

import scalaz.{Success, Monoid}

trait IdentityTransforms extends ReaderMonads with IOMonads {

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

  implicit def idWriter[L:Monoid]:(Id ~> ({type R[+T] = Writer[L,T]})#R) =
    identityOutPipe[({type R[+T] = Writer[L,T]})#R]

  implicit def idValidWriterPipe[E, L:Monoid]: (Id ~> ({type RV[+T] = ValidWriter[E, L, T]})#RV) =
    identityOutPipe[({type RV[+T] = ValidWriter[E, L, T]})#RV]

  implicit def idReaderValidPipe[F,E]: (Id ~> ({type RV[+T] = ReaderValid[F, E,T]})#RV) =
    identityOutPipe[({type RV[+T] = ReaderValid[F, E,T]})#RV]

  implicit def idReaderWriterPipe[F,L:Monoid]: (Id ~> ({type RV[+T] = ReaderWriter[F, L, T]})#RV) =
    identityOutPipe[({type RV[+T] = ReaderWriter[F, L, T]})#RV]

  implicit def idReaderValidWriterPipe[F,L:Monoid, E]: (Id ~> ({type RV[+T] = ReaderValidWriter[F, E, L, T]})#RV) =
    identityOutPipe[({type RV[+T] = ReaderValidWriter[F, E, L, T]})#RV]

  implicit def idReaderFuturePipe[F](implicit ex:ExecutionContext): (Id ~> ({type FR[+T] = ReaderFuture[F, T]})#FR) =
    identityOutPipe[({type FR[+T] = ReaderFuture[F, T]})#FR]

  implicit def idReaderFutureValidPipe[F,E](implicit ex:ExecutionContext): (Id ~> ({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV) =
    identityOutPipe[({type FRV[+T] = ReaderFutureValid[F, E, T]})#FRV]

  implicit val idIOPipe:(Id ~> IO) =
    identityOutPipe[IO]

  implicit def idIOValidPipe[E]:(Id ~> ({type IV[+T] = IOValid[E,T]})#IV) =
    identityOutPipe[({type IV[+T] = IOValid[E,T]})#IV]

  implicit def idIOWriterPipe[L:Monoid]: (Id ~> ({type RV[+T] = IOWriter[L, T]})#RV) =
    identityOutPipe[({type RV[+T] = IOWriter[L, T]})#RV]

  implicit def idIOValidWriterPipe[E, L:Monoid]: (Id ~> ({type RV[+T] = IOValidWriter[E, L, T]})#RV) =
    identityOutPipe[({type RV[+T] = IOValidWriter[E, L, T]})#RV]

}

trait WriterTransforms {
  implicit def WriterValidWriter[L, E] = new (({type W[+T] = Writer[L,T]})#W ~> ({type WV[+T] = ValidWriter[E, L, T]})#WV) {

    def apply[T](value: Writer[L, T]) =
      scalaz.Success(value)
  }

  implicit def WriterFutureWriter[L] = new (({type W[+T] = Writer[L, T]})#W ~> ({type FW[+T] = FutureWriter[L, T]})#FW) {

    def apply[T](value: Writer[L, T]) =
      FutureLift(value)
  }


  implicit def WriterReaderWriter[F,L] = new (({type W[+T] = Writer[L,T]})#W ~> ({type RW[+T] = ReaderWriter[F,L,T]})#RW) {
    def apply[A](fa: Writer[L, A]): ReaderWriter[F, L, A] =
      ReaderFacade(fa)
  }

  implicit def WriterReaderValidWriter[F, E, L] = new (({type W[+T] = Writer[L, T]})#W ~> ({type RW[+T] = ReaderValidWriter[F, E, L, T]})#RW) {
    def apply[A](fa: Writer[L, A]): ReaderValidWriter[F, E, L, A] =
      ReaderFacade(Success(fa))
  }


  implicit def WriterIOWriter[L] = new (({type W[+T] = Writer[L,T]})#W ~> ({type IW[+T] = IOWriter[L,T]})#IW) {
    def apply[A](fa: Writer[L, A]): IOWriter[L, A] =
      IO(fa)
  }

  implicit def WriterIOValidWriter[E, L] = new (({type W[+T] = Writer[L, T]})#W ~> ({type IWV[+T] = IOValidWriter[E,L,T]})#IWV) {
    def apply[A](fa: Writer[L, A]): IOValidWriter[E, L, A] =
      IO(Success(fa))
  }
}

trait ValidTransforms {

  implicit def ValidFutureValidPipe[E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type V[+T] = FutureValid[E,T]})#V) {

    def apply[T](value:Valid[E, T]) = FutureLift(value)
  }

  implicit def ValidReaderValid[F,E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type RV[+T] = ReaderValid[F, E,T]})#RV) {

    def apply[T](value: Valid[E, T]) = ReaderFacade(value)
  }

  implicit def ValidIOValid[E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type RV[+T] = IOValid[E,T]})#RV) {

    def apply[T](value: Valid[E, T]) = IO(value)
  }

  implicit def ValidValidWriter[E,L:Monoid] = new (({type V[+T] = Valid[E,T]})#V ~> ({type VW[+T] = ValidWriter[E,L,T]})#VW) {

    def apply[T](value: Valid[E, T]) = value.map(Writer.zero(_))
  }

  implicit def ValidReaderFutureValid[F,E] = new (({type V[+T] = Valid[E,T]})#V ~> ({type RV[+T] = ReaderFutureValid[F, E,T]})#RV) {

    def apply[T](value: Valid[E, T]) = ReaderFacade(FutureLift(value))
  }

  implicit def ValidWriterFutureValidWriter[E, L] = new (({type WV[+T] = ValidWriter[E, L, T]})#WV ~> ({type FWV[+T] = FutureValidWriter[E, L, T]})#FWV) {
    def apply[A](fa: ValidWriter[E, L, A]): FutureValidWriter[E, L, A] =
      FutureLift(fa)
  }

  implicit def ValidWriterReaderValidWriter[F, E, L] = new (({type WV[+T] = ValidWriter[E, L, T]})#WV ~> ({type RW[+T] = ReaderValidWriter[F,E,L,T]})#RW) {
    def apply[A](fa: ValidWriter[E, L, A]): ReaderValidWriter[F, E, L, A] = {
      ReaderFacade(fa)
    }
  }

  implicit def ValidWriterIOValidWriter[E, L] = new (({type WV[+T] = ValidWriter[E, L, T]})#WV ~> ({type IWV[+T] = IOValidWriter[E,L,T]})#IWV) {
    def apply[A](fa: ValidWriter[E,L, A]): IOValidWriter[E, L, A] = {
      IO(fa)
    }
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

  implicit def FutureValidFutureValidWriter[E, L](implicit ec:ExecutionContext, m:Monoid[L]) =
    new (({type V[+T] = FutureValid[E,T]})#V ~> ({type FVW[+T] = FutureValidWriter[E, L, T]})#FVW) {

      def apply[T](value: FutureValid[E, T]) =
        value.map(_.map(Writer.zero(_)))
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

  implicit def ReaderReaderWriter[F,L](implicit m:Monoid[L]) = new (({type R[+T] = Reader[F,T]})#R ~> ({type RW[+T] = ReaderWriter[F,L,T]})#RW) {
    def apply[A](fa: Reader[F, A]): ReaderWriter[F, L, A] =
      fa.map(t => Writer(m.zero, t))
  }

  implicit def ReaderReaderValidWriter[F, E, L](implicit m:Monoid[L]) = new (({type R[+T] = Reader[F,T]})#R ~> ({type RWV[+T] = ReaderValidWriter[F, E, L, T]})#RWV) {
    def apply[A](fa: Reader[F, A]): ReaderValidWriter[F, E, L, A] =
      fa.map(t => Success(Writer(m.zero, t)))
  }

  implicit def ReaderWriterReaderValidWriter[F, E, L] = new (({type R[+T] = ReaderWriter[F,L,T]})#R ~> ({type RWV[+T] = ReaderValidWriter[F, E, L, T]})#RWV) {
    def apply[A](fa: ReaderWriter[F, L, A]): ReaderValidWriter[F, E, L, A] =
      fa.map(Success(_))
  }

  implicit def ReaderValidReaderValidWriter[F, E, L](implicit m:Monoid[L]) = new (({type R[+T] = ReaderValid[F,E,T]})#R ~> ({type RWV[+T] = ReaderValidWriter[F, E, L, T]})#RWV) {
    def apply[A](fa: ReaderValid[F, E, A]): ReaderValidWriter[F, E, L, A] =
      fa.map(_.map(Writer(m.zero, _)))
  }

}



trait IOTransforms {

  implicit val IOId = new (IO ~> Id) {
    def apply[A](fa: IO[A]): Id[A] = fa.run()
  }
  implicit def IOValidValid[E] = new (({type IV[+T] = IOValid[E,T]})#IV ~> ({type V[+T] = Valid[E, T]})#V) {
    def apply[A](fa: IOValid[E, A]): Valid[E, A] = fa.run()
  }
  implicit def IOValid[E] = new (IO ~> ({type V[+T] = Valid[E, T]})#V) {
    def apply[A](fa: IO[A]): Valid[E, A] = Success(fa.run())
  }
  implicit def IOWriterWriter[L] = new (({type IW[+T] = IOWriter[L, T]})#IW ~> ({type W[+T] = Writer[L, T]})#W) {
    def apply[A](fa: IOWriter[L, A]): Writer[L, A] = fa.run()
  }

  implicit def IOIOValid[E] = new (({type R[+T] = IO[T]})#R ~> ({type IV[+T] = IOValid[E,T]})#IV) {
    def apply[T](value: IO[T]) =
      value.map(scalaz.Success(_))
  }

  implicit def IOValidWriterValidWriter[E,L](implicit m:Monoid[L]) = new (({type IVW[+T] = IO[Valid[E, Writer[L,T]]]})#IVW ~> ({type VW[+T] = Valid[E, Writer[L,T]]})#VW) {
    def apply[A](fa: IO[Valid[E, Writer[L, A]]]): Valid[E, Writer[L, A]] =
      fa.run()
  }

  implicit def IOIOValidWriter[E,L](implicit m:Monoid[L]) = new (IO ~> ({type IVW[+T] = IO[Valid[E, Writer[L,T]]]})#IVW) {
    def apply[A](fa: IO[A]): IO[Valid[E, Writer[L, A]]] =
      fa.map(v => Success(Writer.zero(v)))
  }

  implicit def IOValidIOValidWriter[E,L](implicit m:Monoid[L]) = new (({type IV[+T] = IO[Valid[E, T]]})#IV ~> ({type IVW[+T] = IO[Valid[E, Writer[L,T]]]})#IVW) {
    def apply[A](fa: IO[Valid[E, A]]): IO[Valid[E, Writer[L, A]]] =
      fa.map(_.map(s => Writer.zero(s)))
  }

  implicit def IOReader[F] = new (IO ~> ({type R[+T] = Reader[F, T]})#R) {
    def apply[T](value: IO[T]) =
      Reader(t => value.run())
  }
  implicit def IOReaderValid[F, E] = new (IO ~> ({type R[+T] = ReaderValid[F, E, T]})#R) {
    def apply[T](value: IO[T]) =
      Reader(t => scalaz.Success(value.run()))
  }
  implicit def IOReaderValidWriter[F, E, L](implicit m:Monoid[L]) = new (IO ~> ({type R[+T] = ReaderValidWriter[F, E, L, T]})#R) {
    def apply[T](value: IO[T]):ReaderValidWriter[F, E, L, T] =
      Reader(t => scalaz.Success(Writer.zero(value.run())))
  }
  implicit def IOValidReaderValid[F, E] = new (({type IV[+T] = IOValid[E,T]})#IV ~> ({type R[+T] = ReaderValid[F, E, T]})#R) {
    def apply[A](fa: IOValid[E, A]): ReaderValid[F, E, A] =
      Reader(t => fa.run())
  }
  implicit def IOValidReaderValidWriter[F, E, L](implicit m:Monoid[L]) = new (({type IV[+T] = IOValid[E,T]})#IV ~> ({type R[+T] = ReaderValidWriter[F, E, L, T]})#R) {
    def apply[A](fa: IOValid[E, A]): ReaderValidWriter[F, E, L, A] =
      Reader(t => fa.run().map(Writer.zero(_)))
  }
}

object Transforms extends IdentityTransforms with ValidTransforms with WriterTransforms with FutureTransforms with ReaderTransforms with IOTransforms

