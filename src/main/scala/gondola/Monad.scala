package gondola

import scalaz._
import scalaz.syntax.ToMonadOps

trait OptionOps {
  implicit class OptionExt[T](value:Option[T]) {
    def mapM[M[+_], S](f:T => M[S])(implicit monad:Monad[M]):M[Option[S]] =
      value.fold[M[Option[S]]](monad.point(None))(t => monad.map(f(t))(Some(_)))
    def flatMapM[M[+_], S](f:T => M[Option[S]])(implicit monad:Monad[M]):M[Option[S]] =
      value.fold[M[Option[S]]](monad.point(None))(t => f(t))
    def foldM[M[+_], S](default: => M[S])(f:T => M[S])(implicit monad:Monad[M]):M[S] =
      value.fold(default)(t =>
        f(t)
      )
    
    //Collect broken under 2.11 this is a workaround
    def collectF[S](pf: PartialFunction[T, S]):Option[S] =
      if (value.nonEmpty) pf.lift(value.get) else None
    
    def collectM[M[+_], S](pf: PartialFunction[T, M[S]])(implicit monad:Monad[M]):M[Option[S]] =
      if (value.nonEmpty)
        pf.lift(value.get).fold[M[Option[S]]](monad.point(None)){ v => monad.map(v)(Some(_))}
      else monad.point(None)
  }
}

trait MonadOps {
  import scalaz.Scalaz._

  def point[M[+_], T](t: => T)(implicit monad:Monad[M]):M[T] =
    monad.point(t)

  def sequence[M[+_], T](l:List[M[T]])(implicit monad:Monad[M]):M[List[T]] =
    monad.sequence(l)

  implicit class SeqMonad[M[+_], A](in: Seq[M[A]])(implicit monad: Monad[M]) {
    import scalaz.Scalaz._

    def sequence:M[Seq[A]] =
      monad.sequence(in.toList)
  }
}

object Monad extends MonadOps with ToMonadOps with OptionOps

trait FMonad[E, M[+_]] extends Monad[M] {

  def failure(validationFailure: => E):M[Nothing]

  def failures(validationFailures: => NonEmptyList[E]):M[Nothing]

  def onFailure[T, S >: T](value:M[T])(f:NonEmptyList[E] => M[S]):M[S]
}

trait PipeFMonad {

  implicit class FailureMonad[M[+_], E, A](M:M[A])(implicit monad: FMonad[E, M]) {
    def onFailure[T >: A](f:NonEmptyList[E] => M[T]):M[T] =
      monad.onFailure[A, T](M)(f)
  }
}

trait FMonadOps extends MonadOps {
  def failure[E, M[+_]](validationFailure: => E)(implicit fmonad:FMonad[E, M]):M[Nothing] =
    fmonad.failures(NonEmptyList(validationFailure))

  def failures[E, M[+_]](validationFailures: => NonEmptyList[E])(implicit fmonad:FMonad[E, M]):M[Nothing] =
    fmonad.failures(validationFailures)
}

object FMonad extends PipeFMonad with FMonadOps with ToMonadOps with OptionOps

object Scalaz extends
  StateFunctions        // Functions related to the state monad
  with scalaz.syntax.ToTypeClassOps    // syntax associated with type classes
  with scalaz.syntax.ToDataOps         // syntax associated with Scalaz data structures
  //with scalaz.std.AllInstances         // BREAKS Monads
  with scalaz.std.AllFunctions         // Functions related to standard library types
  with scalaz.syntax.std.ToAllStdOps   // syntax associated with standard library types
  with scalaz.IdInstances              // Identity type and instances

