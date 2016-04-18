package gondola.std

import cats.{Monad, MonadError, MonadReader, ~>}
import cats.data._

import scala.concurrent.{ExecutionContext, Future}

object Reader {
  def apply[A, B](f: A => B): Reader[A, B] =
    ReaderT[Id, A, B](f)

  def apply[A, B](b:B):Reader[A,B] =
    ReaderT.pure(b)
}

trait ReaderMonads[R] {

  implicit val readerMonad:MonadReader[Reader[R, ?], R] =
    Kleisli.kleisliIdMonadReader[R]
}

trait ReaderTransforms[R] extends ReaderMonads[R] with IdTransforms {

  def toReaderTransform[M[_], N[_]](implicit transform:M ~> N, monad:Monad[N]): M ~> ReaderT[N, R, ?] =
    new (M ~> ReaderT[N, R, ?]) {
      def apply[A](fa: M[A]): ReaderT[N, R, A] =
        Kleisli[N, R, A](_ => transform(fa))
    }

  def fromReaderTransform[M[_], N[_]](implicit transform:M ~> N): ReaderT[M, R, ?] ~> ReaderT[N, R, ?] =
    new (ReaderT[M, R, ?] ~> ReaderT[N, R, ?]) {
      def apply[A](fa: ReaderT[M, R, A]): ReaderT[N, R, A] =
        fa.mapF[N, A](transform.apply)
    }

  implicit val id2Reader: Id ~> Reader[R, ?] =
    fromIdentity[Reader[R, ?]]

  implicit val reader2Reader: Reader[R, ?] ~> Reader[R, ?] =
    identity[Reader[R, ?]]
}

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

trait ReaderWriterMonads[R, W] extends ReaderMonads[R] with WriterMonads[W] {

  implicit val readerWriterMonad:MonadReader[ReaderWriter[R, W, ?], R] with MonadWriter[ReaderWriter[R, W, ?], W] =
    new MonadReaderImpl[Writer[W, ?], R] with MonadWriter[ReaderWriter[R, W, ?], W] {

      def writer[A](aw: (W, A)): ReaderWriter[R, W, A] =
        Kleisli[Writer[W, ?], R, A](_ => writerMonad.writer(aw))

      def listen[A](fa: ReaderWriter[R, W, A]): ReaderWriter[R, W, (W, A)] =
        Kleisli[Writer[W, ?], R, (W, A)](r => writerMonad.listen(fa.run(r)))

      def pass[A](fa: ReaderWriter[R, W, ((W) => W, A)]): ReaderWriter[R, W, A] =
        Kleisli[Writer[W, ?], R, A](r => writerMonad.pass(fa.run(r)))
    }
}

trait ReaderWriterTransforms[R, W] extends ReaderTransforms[R] with WriterTransforms[W] with ReaderWriterMonads[R, W] {

  implicit val id2ReaderWriter: Id ~> ReaderWriter[R, W, ?] =
    fromIdentity[ReaderWriter[R, W, ?]]

  implicit val readerWriter2ReaderWriter: ReaderWriter[R, W, ?] ~> ReaderWriter[R, W, ?] =
    identity[ReaderWriter[R, W, ?]]

  implicit val reader2ReaderWriter: Reader[R, ?] ~> ReaderWriter[R, W, ?] =
    fromReaderTransform[Id, Writer[W, ?]]
  
  implicit val writer2ReaderWriter: Writer[W, ?] ~> ReaderWriter[R, W, ?] =
    toReaderTransform[Writer[W, ?], Writer[W, ?]]
}

trait ReaderValidMonads[R, E] extends ReaderMonads[R] with ValidMonads[E] {

  implicit val readerValidMonad:MonadReader[ReaderValid[R, E, ?], R] with MonadError[ReaderValid[R, E, ?], E] =
    new MonadReaderErrorImpl[Valid[E, ?], R, E] with MonadError[ReaderValid[R, E, ?], E]
}

trait ReaderValidTransforms[R, E] extends ReaderTransforms[R] with ValidTransforms[E] with ReaderValidMonads[R, E] {

  implicit val id2ReaderValid: Id ~> ReaderValid[R, E, ?] =
    fromIdentity[ReaderValid[R, E, ?]]

  implicit val readerValid2ReaderValid: ReaderValid[R, E, ?] ~> ReaderValid[R, E, ?] =
    identity[ReaderValid[R, E, ?]]

  implicit val reader2ReaderValid: Reader[R, ?] ~> ReaderValid[R, E, ?] =
    fromReaderTransform[Id, Valid[E, ?]]

  implicit val valid2ReaderValid: Valid[E, ?] ~> ReaderValid[R, E, ?] =
    toReaderTransform[Valid[E, ?], Valid[E, ?]]
}

trait ReaderFutureMonads[R] extends ReaderMonads[R] with FutureMonads {

  implicit def readerFutureMonad(implicit ec:ExecutionContext):MonadReader[ReaderFuture[R, ?], R] with MonadError[ReaderFuture[R, ?], Throwable]
    = new MonadReaderErrorImpl[Future, R, Throwable] with MonadError[ReaderFuture[R, ?], Throwable]
}

trait ReaderFutureTransforms[R] extends ReaderFutureMonads[R] with ReaderTransforms[R] with FutureTransfoms {

  implicit def id2ReaderFuture(implicit ec:ExecutionContext):Id ~> ReaderFuture[R, ?] =
    toReaderTransform[Id, Future]

  implicit def reader2ReaderFuture(implicit ec:ExecutionContext):Reader[R, ?] ~> ReaderFuture[R, ?] =
    fromReaderTransform[Id, Future]

  implicit def future2ReaderFuture(implicit ec:ExecutionContext):Future ~> ReaderFuture[R, ?] =
    toReaderTransform[Future, Future]

  implicit val readerFuture2ReaderFuture:ReaderFuture[R,?] ~> ReaderFuture[R, ?] =
    identity[ReaderFuture[R, ?]]
}

trait ReaderWriterValidMonads[R, W, E] extends ReaderWriterMonads[R, W] with ReaderValidMonads[R, E] with WriterValidMonads[W, E] {

  implicit val readerWriterValidMonad:MonadReader[ReaderWriterValid[R, W, E, ?], R] with MonadWriter[ReaderWriterValid[R, W, E, ?], W] with MonadError[ReaderWriterValid[R, W, E, ?], E] =
    new MonadReaderErrorImpl[WriterValid[W, E, ?], R, E] with MonadWriter[ReaderWriterValid[R, W, E, ?], W] with MonadError[ReaderWriterValid[R, W, E, ?], E] {

      def listen[A](fa: ReaderWriterValid[R, W, E, A]): ReaderWriterValid[R, W, E, (W, A)] =
        Kleisli[WriterValid[W, E, ?], R, (W, A)](r => writerValidMonad.listen(fa.run(r)))

      def writer[A](aw: (W, A)): ReaderWriterValid[R, W, E, A] =
        Kleisli[WriterValid[W, E, ?], R, A](_ => writerValidMonad.writer(aw))

      def pass[A](fa: ReaderWriterValid[R, W, E, ((W) => W, A)]): ReaderWriterValid[R, W, E, A] =
        Kleisli[WriterValid[W, E, ?], R, A](r => writerValidMonad.pass(fa.run(r)))
    }
}

trait ReaderWriterValidTransforms[R, W, E]
  extends ReaderWriterValidMonads[R, W, E]
  with ReaderWriterTransforms[R, W]
  with ReaderValidTransforms[R, E]
  with WriterValidTransforms[W, E] {

  implicit val id2ReaderWriterValid:Id ~> ReaderWriterValid[R, W, E, ?] =
    toReaderTransform[Id, WriterValid[W, E, ?]]

  implicit val reader2ReaderWriterValid:Reader[R, ?] ~> ReaderWriterValid[R, W, E, ?] =
    fromReaderTransform[Id, WriterValid[W, E, ?]]

  implicit val writerValid2ReaderWriterValid:WriterValid[W, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    toReaderTransform[WriterValid[W, E, ?], WriterValid[W, E, ?]]

  implicit val writer2ReaderWriterValid:Writer[W, ?] ~> ReaderWriterValid[R, W, E, ?] =
    writer2writerValid.andThen[ReaderWriterValid[R, W, E, ?]](writerValid2ReaderWriterValid)

  implicit val valid2ReaderWriterValid:Valid[E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    valid2writerValid.andThen[ReaderWriterValid[R, W, E, ?]](writerValid2ReaderWriterValid)

  implicit val readerWriter2ReaderWriterValid:ReaderWriter[R, W, ?] ~> ReaderWriterValid[R, W, E, ?] =
    fromReaderTransform[Writer[W, ?], WriterValid[W, E, ?]]

  implicit val readerValid2ReaderWriterValid:ReaderValid[R, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    fromReaderTransform[Valid[E, ?], WriterValid[W, E, ?]]

  implicit val readerWriterValid2ReaderWriterValid:ReaderWriterValid[R, W, E, ?] ~> ReaderWriterValid[R, W, E, ?] =
    identity[ReaderWriterValid[R, W, E, ?]]

}

trait ReaderFutureValidMonads[R, E] extends ReaderFutureMonads[R] with ReaderValidMonads[R, E] with FutureValidMonads[E] {

  implicit def readerFutureValidMonad(implicit ec:ExecutionContext):MonadReader[ReaderFutureValid[R, E, ?], R] with MonadError[ReaderFutureValid[R, E, ?], E] =
    new MonadReaderErrorImpl[FutureValid[E, ?], R, E] with MonadError[ReaderFutureValid[R, E, ?], E]
}


