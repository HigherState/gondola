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

  implicit def id2Reader[R](M:Monad[Id]): Id ~> Reader[R, ?] =
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

trait ReaderWriterMonad {

  implicit def readerWriterMonad[R, W](implicit MW:MonadWriter[Writer[W, ?], W]):MonadReader[ReaderWriter[R, W, ?], R] with MonadWriter[ReaderWriter[R, W, ?], W] =
    new MonadReaderImpl[Writer[W, ?], R]()(MW) with MonadWriter[ReaderWriter[R, W, ?], W] {

      def writer[A](aw: (W, A)): ReaderWriter[R, W, A] =
        Kleisli[Writer[W, ?], R, A](_ => MW.writer(aw))

      def listen[A](fa: ReaderWriter[R, W, A]): ReaderWriter[R, W, (W, A)] =
        Kleisli[Writer[W, ?], R, (W, A)](r => MW.listen(fa.run(r)))

      def pass[A](fa: ReaderWriter[R, W, ((W) => W, A)]): ReaderWriter[R, W, A] =
        Kleisli[Writer[W, ?], R, A](r => MW.pass(fa.run(r)))
    }
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


trait ReaderValidMonad {

  implicit def readerValidMonad[R, E](implicit ME:MonadError[Valid[E, ?], E]):MonadReader[ReaderValid[R, E, ?], R] with MonadError[ReaderValid[R, E, ?], E] =
    new MonadReaderErrorImpl[Valid[E, ?], R, E]()(ME) with MonadError[ReaderValid[R, E, ?], E]
}

object ReaderValidMonads
  extends ReaderValidMonad
  with ReaderMonad
  with ValidMonad
  with IdMonad

trait ReaderValidTransformations {

  implicit def id2ReaderValid[R, E](implicit M:Monad[ReaderValid[R, E,?]]): Id ~> ReaderValid[R, E, ?] =
    IdTransformationOps.fromIdentity[ReaderValid[R, E, ?]](M)

  implicit def readerValid2ReaderValid[R, E]: ReaderValid[R, E, ?] ~> ReaderValid[R, E, ?] =
    IdTransformationOps.identity[ReaderValid[R, E, ?]]

  implicit def reader2ReaderValid[R, E](implicit T:Id ~> Valid[E, ?]): Reader[R, ?] ~> ReaderValid[R, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, Valid[E, ?], R](T)

  implicit def valid2ReaderValid[R, E](implicit T:Valid[E, ?] ~> Valid[E, ?], N:Monad[Valid[E, ?]]): Valid[E, ?] ~> ReaderValid[R, E, ?] =
    ReaderTransformationOps.toReaderTransform[Valid[E, ?], Valid[E, ?], R](T, N)

  implicit def readerValid2Valid[R, E]:ReaderTransformation[ReaderValid[R, E, ?], Valid[E, ?], R] =
    ReaderTransformationOps.dropReader[Valid[E, ?], R]
}

object ReaderValidTransformations
  extends ReaderValidTransformations
  with ReaderTransformations
  with ValidTransformations
  with IdTransformations
  with ReaderValidMonad
  with ReaderMonad
  with ValidMonad
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

trait ReaderWriterValidMonad {

  implicit def readerWriterValidMonad[R, W, E](implicit MWE:MonadWriter[WriterValid[W, E, ?], W] with MonadError[WriterValid[W, E, ?], E]):MonadReader[ReaderWriterValid[R, W, E, ?], R] with MonadWriter[ReaderWriterValid[R, W, E, ?], W] with MonadError[ReaderWriterValid[R, W, E, ?], E] =
    new MonadReaderErrorImpl[WriterValid[W, E, ?], R, E]()(MWE) with MonadWriter[ReaderWriterValid[R, W, E, ?], W] with MonadError[ReaderWriterValid[R, W, E, ?], E] {

      def listen[A](fa: ReaderWriterValid[R, W, E, A]): ReaderWriterValid[R, W, E, (W, A)] =
        Kleisli[WriterValid[W, E, ?], R, (W, A)](r => MWE.listen(fa.run(r)))

      def writer[A](aw: (W, A)): ReaderWriterValid[R, W, E, A] =
        Kleisli[WriterValid[W, E, ?], R, A](_ => MWE.writer(aw))

      def pass[A](fa: ReaderWriterValid[R, W, E, ((W) => W, A)]): ReaderWriterValid[R, W, E, A] =
        Kleisli[WriterValid[W, E, ?], R, A](r => MWE.pass(fa.run(r)))
    }
}

object ReaderWriterValidMonads
  extends ReaderWriterValidMonad
  with ReaderWriterMonad
  with ReaderValidMonad
  with WriterValidMonad
  with ReaderMonad
  with WriterMonad
  with IdMonad

trait ReaderWriterValidTransformations {

  implicit def id2ReaderWriterValid[R, W, E](implicit M:Monad[ReaderWriterValid[R, W, E, ?]]):Id ~> ReaderWriterValid[R, W, E, ?] =
    IdTransformationOps.fromIdentity[ReaderWriterValid[R, W, E, ?]](M)

  implicit def reader2ReaderWriterValid[R, W, E](implicit T:Id ~> WriterValid[W, E, ?]):Reader[R, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Id, WriterValid[W, E, ?], R](T)

  implicit def writerValid2ReaderWriterValid[R, W, E](implicit T:WriterValid[W, E, ?] ~> WriterValid[W, E, ?], M:Monad[WriterValid[W, E, ?]]):WriterValid[W, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[WriterValid[W, E, ?], WriterValid[W, E, ?], R](T, M)

  implicit def writer2ReaderWriterValid[R, W, E](implicit T:Writer[W, ?] ~> WriterValid[W, E, ?], M:Monad[WriterValid[W, E, ?]]):Writer[W, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Writer[W, ?], WriterValid[W, E, ?], R](T, M)

  implicit def valid2ReaderWriterValid[R, W, E](implicit T:Valid[E, ?] ~> WriterValid[W, E, ?], M:Monad[WriterValid[W, E, ?]]):Valid[E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.toReaderTransform[Valid[E, ?], WriterValid[W, E, ?], R](T, M)

  implicit def readerWriter2ReaderWriterValid[R, W, E](implicit T:Writer[W, ?] ~> WriterValid[W, E, ?]):ReaderWriter[R, W, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Writer[W, ?], WriterValid[W, E, ?], R](T)

  implicit def readerValid2ReaderWriterValid[R, W, E](implicit T:Valid[E, ?] ~> WriterValid[W, E, ?]):ReaderValid[R, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    ReaderTransformationOps.fromReaderTransform[Valid[E, ?], WriterValid[W, E, ?], R](T)

  implicit def readerWriterValid2ReaderWriterValid[R, W, E]:ReaderWriterValid[R, W, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    IdTransformationOps.identity[ReaderWriterValid[R, W, E, ?]]

  implicit def readerWriterValid2ReaderWriter[R, W, E]: WriterTransformation[ReaderWriterValid[R, W, E, ?], ReaderValid[R, E, ?], W] =
    new WriterTransformation[ReaderWriterValid[R, W, E, ?], ReaderValid[R, E, ?], W] {
      def apply[A](fa: ReaderWriterValid[R, W, E, A]): ReaderValid[R, E, (W, A)] = {
        fa.mapF[Valid[E, ?], (W, A)](_.run)
      }
    }

  implicit def readerWriterValid2WriterValid[R, W, E]: ReaderTransformation[ReaderWriterValid[R, W, E, ?], WriterValid[W, E, ?], R] =
    ReaderTransformationOps.dropReader[WriterValid[W, E, ?], R]
}

object ReaderWriterValidTransformations
  extends ReaderWriterValidTransformations
  with ReaderWriterTransformations
  with ReaderValidTransformations
  with WriterValidTransformations
  with ReaderTransformations
  with WriterTransformations
  with ValidTransformations
  with IdTransformations
  with ReaderWriterValidMonad
  with ReaderWriterMonad
  with ReaderValidMonad
  with WriterValidMonad
  with ReaderMonad
  with WriterMonad
  with IdMonad

trait ReaderFutureValidMonad {

  implicit def readerFutureValidMonad[R, E](implicit ec:ExecutionContext, M:MonadError[FutureValid[E, ?], E]):MonadReader[ReaderFutureValid[R, E, ?], R] with MonadError[ReaderFutureValid[R, E, ?], E] =
    new MonadReaderErrorImpl[FutureValid[E, ?], R, E]()(M) with MonadError[ReaderFutureValid[R, E, ?], E]
}

object ReaderFutureValidMonads
  extends ReaderFutureValidMonad
  with ReaderFutureMonad
  with ReaderValidMonad
  with FutureValidMonad
  with ReaderMonad
  with FutureMonad
  with ValidMonad
  with IdMonad

trait ReaderFutureValidTransformations {

  implicit def readerFutureValid2FutureValid[R, E]: ReaderTransformation[ReaderFutureValid[R, E, ?], FutureValid[E, ?], R] =
    ReaderTransformationOps.dropReader[FutureValid[E, ?], R]
}

object ReaderFutureValidTransforms
  extends ReaderFutureValidTransformations
  with ReaderFutureTransformations
  with ReaderValidTransformations
  with FutureValidTransformations
  with ReaderTransformations
  with FutureTransformations
  with ValidTransformations
  with IdTransformations
  with ReaderFutureValidMonad
  with ReaderFutureMonad
  with ReaderValidMonad
  with FutureValidMonad
  with ReaderMonad
  with FutureMonad
  with ValidMonad
  with IdMonad


