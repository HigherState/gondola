package gondola

import cats.Functor.ToFunctorOps
import cats.instances.list._
import cats.syntax.{FlatMapSyntax, TraverseSyntax}
import gondola.std.MonadAsync

import scala.concurrent.Future


object VectorInstances extends cats.instances.VectorInstances

trait OptionOps {
  implicit class OptionExt[T](value:Option[T]) {
    def mapM[M[_], S](f:T => M[S])(implicit monad:Monad[M]):M[Option[S]] =
      value.fold[M[Option[S]]](monad.pure(None))(t => monad.map(f(t))(Some(_)))
    def flatMapM[M[_], S](f:T => M[Option[S]])(implicit monad:Monad[M]):M[Option[S]] =
      value.fold[M[Option[S]]](monad.pure(None))(t => f(t))
    def foldM[M[_], S](default: => M[S])(f:T => M[S])(implicit monad:Monad[M]):M[S] =
      value.fold(default)(t =>
        f(t)
      )
    def getOrElseM[M[_]](f: => M[T])(implicit monad:Monad[M]):M[T] =
      value.fold[M[T]](f)(t => monad.pure(t))
    def orElseM[M[_]](f: => M[Option[T]])(implicit monad:Monad[M]):M[Option[T]] =
      value.fold[M[Option[T]]](f)(t => monad.pure(Some(t)))

    //Collect broken under 2.11 this is a workaround
    def collectF[S](pf: PartialFunction[T, S]):Option[S] =
      if (value.nonEmpty) pf.lift(value.get) else None

    def collectM[M[_], S](pf: PartialFunction[T, M[S]])(implicit monad:Monad[M]):M[Option[S]] =
      if (value.nonEmpty)
        pf.lift(value.get).fold[M[Option[S]]](monad.pure(None)){ v => monad.map(v)(Some(_))}
      else monad.pure(None)

    def expected[E, M[_]](failure: => E)(implicit monad:cats.MonadError[M, E]):M[T] =
      value.fold(monad.raiseError[T](failure))(monad.pure)

    def expectedMap[E, M[_], S](failure: => E)(f: T => S)(implicit monad:cats.MonadError[M, E]):M[S] =
      value.fold(monad.raiseError[S](failure))(t => monad.pure(f(t)))
  }
}

trait MonadOps extends FlatMapSyntax with OptionOps with TraverseSyntax with ToFunctorOps {

  def pure[F[_], T](t: => T)(implicit monad:Monad[F]):F[T] =
    monad.pure(t)

  def sequence[F[_], T](l:List[F[T]])(implicit monad:Monad[F]):F[List[T]] =
    monad.sequence(l)

  def sequence[F[_], T](v:Vector[F[T]])(implicit monad:Monad[F]):F[Vector[T]] = {
    import VectorInstances._
    monad.sequence(v)
  }
}

trait MonadErrorOps extends MonadOps {

  def raiseError[F[_], E, A](e:E)(implicit M:cats.MonadError[F, E]):F[A] =
    M.raiseError(e)

  def handleErrorWith[F[_], E, A](fa: F[A])(f: E => F[A])(implicit M:cats.MonadError[F, E]): F[A] =
    M.handleErrorWith(fa)(f)

  def handleError[F[_], E, A](fa: F[A])(f: E => A)(implicit M:cats.MonadError[F, E]): F[A] =
    handleErrorWith(fa)(f andThen M.pure)

  def recover[F[_], E, A](fa: F[A])(pf: PartialFunction[E, A])(implicit M:cats.MonadError[F, E]): F[A] =
    M.recover(fa)(pf)

  def recoverWith[F[_], E, A](fa: F[A])(pf: PartialFunction[E, F[A]])(implicit M:cats.MonadError[F, E]): F[A] =
    M.recoverWith(fa)(pf)

}

trait MonadWriterOps extends MonadOps {
  def writer[F[_], W, A](aw: (W, A))(implicit M:cats.MonadWriter[F, W]): F[A] =
    M.writer(aw)

  def listen[F[_], W, A](fa: F[A])(implicit M:cats.MonadWriter[F, W]): F[(W, A)] =
    M.listen(fa)

  def pass[F[_], W, A](fa: F[(W => W, A)])(implicit M:cats.MonadWriter[F, W]): F[A] =
    M.pass(fa)

  def tell[F[_], W](w: W)(implicit M:cats.MonadWriter[F, W]): F[Unit] =
    M.tell(w)

  def listens[F[_], W, A, B](fa: F[A])(f: W => B)(implicit M:cats.MonadWriter[F, W]): F[(B, A)] =
    M.listens(fa)(f)

  def censor[F[_], W, A](fa: F[A])(f: W => W)(implicit M:cats.MonadWriter[F, W]): F[A] =
    M.censor(fa)(f)
}

trait MonadReaderOps extends MonadOps {

  def ask[F[_], R](implicit M:cats.MonadReader[F, R]): F[R] =
    M.ask

  /** Modify the environment */
  def local[F[_], R, A](f: R => R)(fa: F[A])(implicit M:cats.MonadReader[F, R]): F[A] =
    M.local(f)(fa)
}

trait MonadStateOps extends MonadOps {
  def get[F[_], S](implicit M:cats.MonadState[F, S]): F[S] =
    M.get

  def set[F[_], S](s: S)(implicit M:cats.MonadState[F, S]): F[Unit] =
    M.set(s)

  def modify[F[_], S](f: S => S)(implicit M:cats.MonadState[F, S]): F[Unit] =
    M.flatMap(get)(s => set(f(s)))

  def inspect[F[_], S, A](f: S => A)(implicit M:cats.MonadState[F, S]): F[A] =
    M.map(get)(f)
}

trait MonadAsyncOps extends MonadOps {

  def liftFuture[F[_], A](f:Future[A])(implicit M:MonadAsync[F]):F[A] =
    M.liftFuture(f)

}



object Monad extends MonadOps

object MonadError extends MonadErrorOps

object MonadReader extends MonadReaderOps

object MonadWriter extends MonadWriterOps

object MonadWriterError extends MonadWriterOps

object MonadState extends MonadStateOps


