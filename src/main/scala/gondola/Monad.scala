package gondola

import cats.Functor.ToFunctorOps
import cats.Monad.ToMonadOps
import cats.data.NonEmptyList
import cats.std.list._
import cats.syntax.{FunctorSyntax, TraverseSyntax, FlatMapSyntax}

//import cats.syntax.traverse._


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
    def getOrElseM[M[_], S >: T](f: => M[S])(implicit monad:Monad[M]):M[S] =
      value.fold[M[S]](f)(t => monad.pure(t))
    def orElseM[M[_], S >: T](f: => M[Option[S]])(implicit monad:Monad[M]):M[Option[S]] =
      value.fold[M[Option[S]]](f)(t => monad.pure(Some(t)))

    //Collect broken under 2.11 this is a workaround
    def collectF[S](pf: PartialFunction[T, S]):Option[S] =
      if (value.nonEmpty) pf.lift(value.get) else None

    def collectM[M[_], S](pf: PartialFunction[T, M[S]])(implicit monad:Monad[M]):M[Option[S]] =
      if (value.nonEmpty)
        pf.lift(value.get).fold[M[Option[S]]](monad.pure(None)){ v => monad.map(v)(Some(_))}
      else monad.pure(None)
  }
}

trait MonadOps {

  def pure[M[_], T](t: => T)(implicit monad:Monad[M]):M[T] =
    monad.pure(t)

  def sequence[M[_], T](l:List[M[T]])(implicit monad:Monad[M]):M[List[T]] =
    monad.sequence(l)

  implicit class SeqMonad[M[_], A](in: Seq[M[A]])(implicit monad: Monad[M]) {

    def sequence:M[List[A]] =
      monad.sequence(in.toList)
  }
}

object Monad extends MonadOps with FlatMapSyntax with OptionOps with TraverseSyntax with ToFunctorOps

trait FMonad[E, M[_]] extends Monad[M] {

  def failure(validationFailure: => E):M[Nothing]

  def failures(validationFailures: => NonEmptyList[E]):M[Nothing]

  def onFailure[T](value:M[T])(f:NonEmptyList[E] => M[T]):M[T]
}

trait PipeFMonad {

  implicit class FailureMonad[M[_], E, A](M:M[A])(implicit monad: FMonad[E, M]) {
    def onFailure(f:NonEmptyList[E] => M[A]):M[A] =
      monad.onFailure[A](M)(f)
  }
}

trait FMonadOps extends MonadOps {
  def failure[E, M[_], T](validationFailure: => E)(implicit fmonad:FMonad[E, M]):M[T] =
    fmonad.failures(NonEmptyList(validationFailure)).asInstanceOf[M[T]]

  def failures[E, M[_], T](validationFailures: => NonEmptyList[E])(implicit fmonad:FMonad[E, M]):M[T] =
    fmonad.failures(validationFailures).asInstanceOf[M[T]]
}

object FMonad extends PipeFMonad with FMonadOps with ToMonadOps with OptionOps

trait WMonad[L, M[_]] extends Monad[M] {

  def write(log:L):M[Ack] =
    write(log, Acknowledged)

  def write[T](log:L, value:T):M[T]
}

trait WMonadOps extends MonadOps {

  def write[L, M[_]](log: => L)(implicit wmonad:WMonad[L, M]):M[Ack] =
    wmonad.write(log)
}

trait FWMonad[E, L, M[_]] extends FMonad[E, M] with WMonad[L, M]


