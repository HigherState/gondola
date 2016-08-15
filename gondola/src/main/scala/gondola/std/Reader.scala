package gondola.std

import cats.{Monad, MonadError, MonadReader, MonadWriter}
import cats.data._
import gondola.{ReaderTransformation, WriterTransformation, ~>}

import scala.concurrent.{ExecutionContext, Future}

object Reader {
  def apply[A, B](f: A => B): Reader[A, B] =
    ReaderT[Id, A, B](f)

  def apply[A, B](b:B):Reader[A,B] =
    ReaderT.pure(b)

  def lift[M[_], A, B](f: A => M[B]):ReaderT[M, A, B] =
    ReaderT[M, A, B](f)
}

trait ReaderMonad {

  implicit def readerMonad[R]:MonadReader[Reader[R, ?], R] =
    Kleisli.kleisliIdMonadReader[R]
}

object ReaderMonads
  extends ReaderMonad
  with IdMonad

trait ReaderTransformationOps {
  def toReaderTransform[M[_], N[_], R](transform:M ~> N, monad:Monad[N]): M ~> ReaderT[N, R, ?] =
    new (M ~> ReaderT[N, R, ?]) {
      def apply[A](fa: M[A]): ReaderT[N, R, A] =
        Kleisli[N, R, A](_ => transform(fa))
    }

  def fromReaderTransform[M[_], N[_], R](transform:M ~> N): ReaderT[M, R, ?] ~> ReaderT[N, R, ?] =
    new (ReaderT[M, R, ?] ~> ReaderT[N, R, ?]) {
      def apply[A](fa: ReaderT[M, R, A]): ReaderT[N, R, A] =
        fa.mapF[N, A](transform.apply)
    }

  def dropReader[M[_], R]: ReaderTransformation[ReaderT[M, R, ?], M, R] =
    new ReaderTransformation[ReaderT[M, R, ?], M, R] {
      def apply[A](fa: ReaderT[M, R, A], r: R): M[A] =
        fa.run(r)
    }
}

object ReaderTransformationOps extends ReaderTransformationOps

trait ReaderTransformations {

  implicit def id2Reader[R](implicit M:Monad[Id]): Id ~> Reader[R, ?] =
    IdTransformationOps.fromIdentity[Reader[R, ?]]

  implicit def reader2Reader[R]: Reader[R, ?] ~> Reader[R, ?] =
    IdTransformationOps.identity[Reader[R, ?]]

  implicit def reader2Id[R]: ReaderTransformation[Reader[R, ?], Id, R] =
    ReaderTransformationOps.dropReader[Id, R]
}

object ReaderTransformations
  extends ReaderTransformations
  with IdTransformations

private sealed abstract class MonadReaderImpl[F[_], R](implicit val M:Monad[F]) extends MonadReader[ReaderT[F, R, ?], R] {
  def ask: ReaderT[F, R, R] =
    Kleisli[F, R, R](M.pure)

  def local[A](f: (R) => R)(fa: ReaderT[F, R, A]): ReaderT[F, R, A] =
    Kleisli[F, R, A](f.andThen(fa.run))

  def pure[A](x: A): ReaderT[F, R, A] =
    Kleisli.pure[F, R, A](x)(M)

  def flatMap[A, B](fa: ReaderT[F, R, A])(f: (A) => ReaderT[F, R, B]): ReaderT[F, R, B] =
    fa.flatMap(f)
}

private sealed abstract class MonadReaderErrorImpl[F[_], R, E](implicit override val M:MonadError[F, E]) extends MonadReaderImpl[F, R]()(M) with MonadError[ReaderT[F, R, ?], E] {

  def raiseError[A](e: E): ReaderT[F, R, A] =
    Kleisli[F, R, A](_ => M.raiseError[A](e))

  def handleErrorWith[A](fa: ReaderT[F, R, A])(f: (E) => ReaderT[F, R, A]): ReaderT[F, R, A] =
    Kleisli[F, R, A](r => M.handleErrorWith(fa.run(r))(e => f(e).run(r)))

}

private sealed abstract class MonadReaderWriterImpl[F[_], R, W](implicit override val M:MonadWriter[F, W]) extends MonadReaderImpl[F, R]()(M) with MonadWriter[ReaderT[F, R, ?], W] {

  def writer[A](aw: (W, A)): ReaderT[F, R, A] =
    Kleisli[F, R, A](_ => M.writer(aw))

  def listen[A](fa: ReaderT[F, R, A]): ReaderT[F, R, (W, A)] =
    Kleisli[F, R, (W, A)](r => M.listen(fa.run(r)))

  def pass[A](fa: ReaderT[F, R, ((W) => W, A)]): ReaderT[F, R, A] =
    Kleisli[F, R, A](r => M.pass(fa.run(r)))
}

trait ReaderWriterMonad {

  implicit def readerWriterMonad[R, W](implicit MW:MonadWriter[Writer[W, ?], W]):MonadReader[ReaderWriter[R, W, ?], R] with MonadWriter[ReaderWriter[R, W, ?], W] =
    new MonadReaderWriterImpl[Writer[W, ?], R, W]()(MW) {}
}

object ReaderWriterMonads
  extends ReaderWriterMonad
  with ReaderMonad
  with WriterMonad
  with IdMonad

trait ReaderWriterTransformations {

  implicit def id2ReaderWriter[R, W](implicit M:Monad[ReaderWriter[R, W,?]]): Id ~> ReaderWriter[R, W, ?] =
    IdTransformationOps.fromIdentity[ReaderWriter[R, W, ?]](M)

  implicit def readerWriter2ReaderWriter[R, W]: ReaderWriter[R, W, ?] ~> ReaderWriter[R, W, ?] =
    IdTransformationOps.identity[ReaderWriter[R, W, ?]]

  implicit def reader2ReaderWriter[R, W](implicit T:Id ~> Writer[W, ?]): Reader[R, ?] ~> ReaderWriter[R, W, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, Writer[W, ?], R](T)
  
  implicit def writer2ReaderWriter[R, W](implicit T:Writer[W, ?] ~> Writer[W, ?], N:Monad[Writer[W, ?]]):Writer[W, ?] ~> ReaderWriter[R, W, ?] =
    ReaderTransformationOps.toReaderTransform[Writer[W, ?], Writer[W, ?], R](T, N)

  implicit def readerWriter2reader[R, W]: WriterTransformation[ReaderWriter[R, W, ?], Reader[R, ?], W] =
    new WriterTransformation[ReaderWriter[R, W, ?], Reader[R, ?], W] {
      def apply[A](fa: ReaderWriter[R, W, A]): Reader[R, (W, A)] =
        fa.mapF[Id, (W, A)](_.run)
    }

  implicit def readerWriter2Writer[R, W]: ReaderTransformation[ReaderWriter[R, W, ?], Writer[W, ?], R] =
    ReaderTransformationOps.dropReader[Writer[W, ?], R]
}

object ReaderWriterTransformations
  extends ReaderWriterTransformations
  with ReaderTransformations
  with WriterTransformations
  with IdTransformations
  with ReaderWriterMonad
  with ReaderMonad
  with WriterMonad
  with IdMonad


trait ReaderErrorMonad {

  implicit def readerErrorMonad[R, E](implicit ME:MonadError[Error[E, ?], E]):MonadReader[ReaderError[R, E, ?], R] with MonadError[ReaderError[R, E, ?], E] =
    new MonadReaderErrorImpl[Error[E, ?], R, E]()(ME) with MonadError[ReaderError[R, E, ?], E]
}

object ReaderErrorMonads
  extends ReaderErrorMonad
  with ReaderMonad
  with ErrorMonad
  with IdMonad

trait ReaderErrorTransformations {

  implicit def id2ReaderError[R, E](implicit M:Monad[ReaderError[R, E,?]]): Id ~> ReaderError[R, E, ?] =
    IdTransformationOps.fromIdentity[ReaderError[R, E, ?]](M)

  implicit def readerError2ReaderError[R, E]: ReaderError[R, E, ?] ~> ReaderError[R, E, ?] =
    IdTransformationOps.identity[ReaderError[R, E, ?]]

  implicit def reader2ReaderError[R, E](implicit T:Id ~> Error[E, ?]): Reader[R, ?] ~> ReaderError[R, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, Error[E, ?], R](T)

  implicit def error2ReaderError[R, E](implicit T:Error[E, ?] ~> Error[E, ?], N:Monad[Error[E, ?]]): Error[E, ?] ~> ReaderError[R, E, ?] =
    ReaderTransformationOps.toReaderTransform[Error[E, ?], Error[E, ?], R](T, N)

  implicit def readerError2Error[R, E]:ReaderTransformation[ReaderError[R, E, ?], Error[E, ?], R] =
    ReaderTransformationOps.dropReader[Error[E, ?], R]
}

object ReaderErrorTransformations
  extends ReaderErrorTransformations
  with ReaderTransformations
  with ErrorTransformations
  with IdTransformations
  with ReaderErrorMonad
  with ReaderMonad
  with ErrorMonad
  with IdMonad

trait ReaderFutureMonad {

  implicit def readerFutureMonad[R, E](implicit ec:ExecutionContext, ME:MonadError[Future, Throwable]):MonadReader[ReaderFuture[R, ?], R] with MonadError[ReaderFuture[R, ?], Throwable]
    = new MonadReaderErrorImpl[Future, R, Throwable]()(ME) with MonadError[ReaderFuture[R, ?], Throwable]
}

object ReaderFutureMonads
  extends ReaderFutureMonad
  with ReaderMonad
  with FutureMonad
  with IdMonad

trait ReaderFutureTransformations {

  implicit def id2ReaderFuture[R](implicit ec:ExecutionContext, T:Id ~> Future, N:Monad[Future]):Id ~> ReaderFuture[R, ?] =
    ReaderTransformationOps.toReaderTransform[Id, Future, R](T, N)

  implicit def reader2ReaderFuture[R](implicit ec:ExecutionContext, T:Id ~> Future):Reader[R, ?] ~> ReaderFuture[R, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, Future, R](T)

  implicit def future2ReaderFuture[R](implicit ec:ExecutionContext, T:Future ~> Future, N:Monad[Future]):Future ~> ReaderFuture[R, ?] =
    ReaderTransformationOps.toReaderTransform[Future, Future, R](T, N)

  implicit def readerFuture2ReaderFuture[R]:ReaderFuture[R,?] ~> ReaderFuture[R, ?] =
    IdTransformationOps.identity[ReaderFuture[R, ?]]

  implicit def readerFuture2Future[R]:ReaderTransformation[ReaderFuture[R, ?], Future, R] =
    ReaderTransformationOps.dropReader[Future, R]
}

trait ReaderWriterErrorMonad {

  implicit def readerWriterErrorMonad[R, W, E](implicit MWE:MonadWriter[WriterError[W, E, ?], W] with MonadError[WriterError[W, E, ?], E]):MonadReader[ReaderWriterError[R, W, E, ?], R] with MonadWriter[ReaderWriterError[R, W, E, ?], W] with MonadError[ReaderWriterError[R, W, E, ?], E] =
    new MonadReaderErrorImpl[WriterError[W, E, ?], R, E]()(MWE) with MonadWriter[ReaderWriterError[R, W, E, ?], W] with MonadError[ReaderWriterError[R, W, E, ?], E] {

      def listen[A](fa: ReaderWriterError[R, W, E, A]): ReaderWriterError[R, W, E, (W, A)] =
        Kleisli[WriterError[W, E, ?], R, (W, A)](r => MWE.listen(fa.run(r)))

      def writer[A](aw: (W, A)): ReaderWriterError[R, W, E, A] =
        Kleisli[WriterError[W, E, ?], R, A](_ => MWE.writer(aw))

      def pass[A](fa: ReaderWriterError[R, W, E, ((W) => W, A)]): ReaderWriterError[R, W, E, A] =
        Kleisli[WriterError[W, E, ?], R, A](r => MWE.pass(fa.run(r)))
    }
}

object ReaderWriterErrorMonads
  extends ReaderWriterErrorMonad
  with ReaderWriterMonad
  with ReaderErrorMonad
  with WriterErrorMonad
  with ReaderMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad

trait ReaderWriterErrorTransformations {

  implicit def id2ReaderWriterError[R, W, E](implicit M:Monad[ReaderWriterError[R, W, E, ?]]):Id ~> ReaderWriterError[R, W, E, ?] =
    IdTransformationOps.fromIdentity[ReaderWriterError[R, W, E, ?]](M)

  implicit def reader2ReaderWriterError[R, W, E](implicit T:Id ~> WriterError[W, E, ?]):Reader[R, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, WriterError[W, E, ?], R](T)

  implicit def writerError2ReaderWriterError[R, W, E](implicit T:WriterError[W, E, ?] ~> WriterError[W, E, ?], M:Monad[WriterError[W, E, ?]]):WriterError[W, E, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[WriterError[W, E, ?], WriterError[W, E, ?], R](T, M)

  implicit def writer2ReaderWriterError[R, W, E](implicit T:Writer[W, ?] ~> WriterError[W, E, ?], M:Monad[WriterError[W, E, ?]]):Writer[W, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Writer[W, ?], WriterError[W, E, ?], R](T, M)

  implicit def error2ReaderWriterError[R, W, E](implicit T:Error[E, ?] ~> WriterError[W, E, ?], M:Monad[WriterError[W, E, ?]]):Error[E, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Error[E, ?], WriterError[W, E, ?], R](T, M)

  implicit def readerWriter2ReaderWriterError[R, W, E](implicit T:Writer[W, ?] ~> WriterError[W, E, ?]):ReaderWriter[R, W, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Writer[W, ?], WriterError[W, E, ?], R](T)

  implicit def readerError2ReaderWriterError[R, W, E](implicit T:Error[E, ?] ~> WriterError[W, E, ?]):ReaderError[R, E, ?] ~> ReaderWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Error[E, ?], WriterError[W, E, ?], R](T)

  implicit def readerWriterError2ReaderWriterError[R, W, E]:ReaderWriterError[R, W, E, ?] ~> ReaderWriterError[R, W, E, ?] =
    IdTransformationOps.identity[ReaderWriterError[R, W, E, ?]]

  implicit def readerWriterError2ReaderWriter[R, W, E]: WriterTransformation[ReaderWriterError[R, W, E, ?], ReaderError[R, E, ?], W] =
    new WriterTransformation[ReaderWriterError[R, W, E, ?], ReaderError[R, E, ?], W] {
      def apply[A](fa: ReaderWriterError[R, W, E, A]): ReaderError[R, E, (W, A)] = {
        fa.mapF[Error[E, ?], (W, A)](_.run)
      }
    }

  implicit def readerWriterError2WriterError[R, W, E]: ReaderTransformation[ReaderWriterError[R, W, E, ?], WriterError[W, E, ?], R] =
    ReaderTransformationOps.dropReader[WriterError[W, E, ?], R]
}

object ReaderWriterErrorTransformations
  extends ReaderWriterErrorTransformations
  with ReaderWriterTransformations
  with ReaderErrorTransformations
  with WriterErrorTransformations
  with ReaderTransformations
  with WriterTransformations
  with ErrorTransformations
  with IdTransformations
  with ReaderWriterErrorMonad
  with ReaderWriterMonad
  with ReaderErrorMonad
  with WriterErrorMonad
  with ReaderMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad

trait ReaderFutureErrorMonad {

  implicit def readerFutureErrorMonad[R, E](implicit ec:ExecutionContext, M:MonadError[FutureError[E, ?], E]):MonadReader[ReaderFutureError[R, E, ?], R] with MonadError[ReaderFutureError[R, E, ?], E] =
    new MonadReaderErrorImpl[FutureError[E, ?], R, E]()(M) {}
}

object ReaderFutureErrorMonads
  extends ReaderFutureErrorMonad
  with ReaderFutureMonad
  with ReaderErrorMonad
  with FutureErrorMonad
  with ReaderMonad
  with FutureMonad
  with ErrorMonad
  with IdMonad

trait ReaderFutureErrorTransformations {

  implicit def readerFutureError2FutureError[R, E]: ReaderTransformation[ReaderFutureError[R, E, ?], FutureError[E, ?], R] =
    ReaderTransformationOps.dropReader[FutureError[E, ?], R]
}

object ReaderFutureErrorTransforms
  extends ReaderFutureErrorTransformations
  with ReaderFutureTransformations
  with ReaderErrorTransformations
  with FutureErrorTransformations
  with ReaderTransformations
  with FutureTransformations
  with ErrorTransformations
  with IdTransformations
  with ReaderFutureErrorMonad
  with ReaderFutureMonad
  with ReaderErrorMonad
  with FutureErrorMonad
  with ReaderMonad
  with FutureMonad
  with ErrorMonad
  with IdMonad

trait ReaderFutureWriterMonad {

  implicit def readerFutureWriterMonad[R, W](implicit ec:ExecutionContext, M:MonadWriter[FutureWriter[W, ?], W]):MonadReader[ReaderFutureWriter[R, W, ?], R] with MonadWriter[ReaderFutureWriter[R, W, ?], W] =
    new MonadReaderWriterImpl[FutureWriter[W, ?], R, W]()(M) {}
}

object ReaderFutureWriterMonads
  extends ReaderFutureWriterMonad
    with ReaderFutureMonad
    with ReaderWriterMonad
    with FutureWriterMonad
    with ReaderMonad
    with FutureMonad
    with ErrorMonad
    with IdMonad

trait ReaderFutureWriterTransformations {

  implicit def readerFutureWriter2FutureWriter[R, W]: ReaderTransformation[ReaderFutureWriter[R, W, ?], FutureWriter[W, ?], R] =
    ReaderTransformationOps.dropReader[FutureWriter[W, ?], R]
}

object ReaderFutureWriterTransforms
  extends ReaderFutureWriterTransformations
    with ReaderFutureTransformations
    with ReaderWriterTransformations
    with FutureWriterTransformations
    with ReaderTransformations
    with FutureTransformations
    with WriterTransformations
    with IdTransformations
    with ReaderFutureWriterMonad
    with ReaderFutureMonad
    with ReaderWriterMonad
    with FutureWriterMonad
    with ReaderMonad
    with FutureMonad
    with WriterMonad
    with IdMonad

trait ReaderFutureWriterErrorMonad {
  implicit def readerFutureWriterError[R, W, E](implicit ec:ExecutionContext, MW:MonadError[FutureWriterError[W, E, ?], E] with MonadWriter[FutureWriterError[W, E, ?], W]):
    MonadReader[ReaderFutureWriterError[R, W, E, ?], R] with MonadError[ReaderFutureWriterError[R, W, E, ?], E] with MonadWriter[ReaderFutureWriterError[R, W, E, ?], W] =
      new MonadReaderErrorImpl[FutureWriterError[W, E, ?], R, E] with MonadWriter[ReaderFutureWriterError[R, W, E, ?], W] {
        def writer[A](aw: (W, A)): ReaderFutureWriterError[R, W, E, A] =
          Kleisli[FutureWriterError[W, E, ?], R, A](_ => MW.writer(aw))

        def listen[A](fa: ReaderFutureWriterError[R, W, E, A]): ReaderFutureWriterError[R, W, E, (W, A)] =
          Kleisli[FutureWriterError[W, E, ?], R, (W, A)](r => MW.listen(fa.run(r)))

        def pass[A](fa: ReaderFutureWriterError[R, W, E, ((W) => W, A)]): ReaderFutureWriterError[R, W, E, A] =
          Kleisli[FutureWriterError[W, E, ?], R, A](r => MW.pass(fa.run(r)))
      }
}

object ReaderFutureWriterErrorMonads
  extends ReaderFutureWriterErrorMonad
  with ReaderFutureErrorMonad
  with ReaderFutureWriterMonad
  with ReaderWriterErrorMonad
  with FutureWriterErrorMonad
  with ReaderFutureMonad
  with ReaderErrorMonad
  with ReaderWriterMonad
  with FutureErrorMonad
  with FutureWriterMonad
  with WriterErrorMonad
  with ReaderMonad
  with FutureMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad

trait ReaderFutureWriterErrorTransformations {

  implicit def id2ReaderFutureWriterError[R,W,E](implicit M:Monad[ReaderFutureWriterError[R, W, E, ?]]): Id ~> ReaderFutureWriterError[R, W, E, ?] =
    IdTransformationOps.fromIdentity[ReaderFutureWriterError[R, W, E, ?]](M)

  implicit def reader2ReaderFutureWriterError[R, W, E](implicit T:Id ~> FutureWriterError[W, E, ?]):Reader[R, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, FutureWriterError[W, E, ?], R](T)

  implicit def readerFutureWriterError2FutureWriterError[R, W, E]: ReaderTransformation[ReaderFutureWriterError[R, W, E, ?], FutureWriterError[W, E, ?], R] =
    ReaderTransformationOps.dropReader[FutureWriterError[W, E, ?], R]

  implicit def error2ReaderFutureWriterError[R, W, E](implicit T:Error[E, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):Error[E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Error[E, ?], FutureWriterError[W, E, ?], R](T, M)

  implicit def writer2ReaderFutureWriterError[R, W, E](implicit T:Writer[W, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):Writer[W, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Writer[W, ?], FutureWriterError[W, E, ?], R](T, M)

  implicit def writerError2ReaderFutureWriterError[R, W, E](implicit T:WriterError[W, E, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):WriterError[W, E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[WriterError[W, E, ?], FutureWriterError[W, E, ?], R](T, M)

  implicit def readerError2ReaderFutureWriterError[R, W, E](implicit T:Error[E, ?] ~> FutureWriterError[W, E, ?]):ReaderError[R, E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Error[E, ?], FutureWriterError[W, E, ?], R](T)

  implicit def readerWriter2ReaderFutureWriterError[R, W, E](implicit T:Writer[W, ?] ~> FutureWriterError[W, E, ?]):ReaderWriter[R, W, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Writer[W, ?], FutureWriterError[W, E, ?], R](T)

  implicit def readerWriterError2ReaderFutureWriterError[R, W, E](implicit T:WriterError[W, E, ?] ~> FutureWriterError[W, E, ?]):ReaderWriterError[R, W, E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[WriterError[W, E, ?], FutureWriterError[W, E, ?], R](T)

  implicit def future2ReaderFutureWriterError[R, W, E](implicit T:Future ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):Future ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Future, FutureWriterError[W, E, ?], R](T, M)

  implicit def futureError2ReaderFutureWriterError[R, W, E](implicit T:FutureError[E, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):FutureError[E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[FutureError[E, ?], FutureWriterError[W, E, ?], R](T, M)

  implicit def futureWriter2ReaderFutureWriterError[R, W, E](implicit T:FutureWriter[W, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):FutureWriter[W, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[FutureWriter[W, ?], FutureWriterError[W, E, ?], R](T, M)

  implicit def futureWriterError2ReaderFutureWriterError[R, W, E](implicit T:FutureWriterError[W, E, ?] ~> FutureWriterError[W, E, ?], M:Monad[FutureWriterError[W, E, ?]]):FutureWriterError[W, E, ?] ~> ReaderFutureWriterError[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[FutureWriterError[W, E, ?], FutureWriterError[W, E, ?], R](T, M)
}

object ReaderFutureWriterErrorTransformations
  extends ReaderFutureWriterErrorTransformations
    with ReaderFutureErrorTransformations
    with ReaderFutureWriterTransformations
    with ReaderWriterErrorTransformations
    with FutureWriterErrorTransformations
    with ReaderFutureTransformations
    with ReaderErrorTransformations
    with ReaderWriterTransformations
    with FutureErrorTransformations
    with FutureWriterTransformations
    with WriterErrorTransformations
    with ReaderTransformations
    with FutureTransformations
    with WriterTransformations
    with ErrorTransformations
    with IdTransformations
    with ReaderFutureWriterErrorMonad
    with ReaderFutureErrorMonad
    with ReaderFutureWriterMonad
    with ReaderWriterErrorMonad
    with FutureWriterErrorMonad
    with ReaderFutureMonad
    with ReaderErrorMonad
    with ReaderWriterMonad
    with FutureErrorMonad
    with FutureWriterMonad
    with WriterErrorMonad
    with ReaderMonad
    with FutureMonad
    with WriterMonad
    with ErrorMonad
    with IdMonad


