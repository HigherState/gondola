package gondola

import scalaz._
import scalaz.syntax.ToMonadOps

trait OptionOps {
  implicit class OptionExt[T](value:Option[T]) {
    def mapM[Out[+_], S](f:T => Out[S])(implicit monad:Monad[Out]):Out[Option[S]] =
      value.fold[Out[Option[S]]](monad.point(None))(t => monad.map(f(t))(Some(_)))
    def flatMapM[Out[+_], S](f:T => Out[Option[S]])(implicit monad:Monad[Out]):Out[Option[S]] =
      value.fold[Out[Option[S]]](monad.point(None))(t => f(t))
    def foldM[Out[+_], S](default: => Out[S])(f:T => Out[S])(implicit monad:Monad[Out]):Out[S] =
      value.fold(default)(t =>
        f(t)
      )
  }
}

trait MonadOps {
  import scalaz.Scalaz._

  def point[Out[+_], T](t: => T)(implicit monad:Monad[Out]):Out[T] =
    monad.point(t)

  def sequence[Out[+_], T](l:List[Out[T]])(implicit monad:Monad[Out]):Out[List[T]] =
    monad.sequence(l)
}

object Monad extends MonadOps with ToMonadOps with OptionOps

trait FMonad[E, Out[+_]] extends Monad[Out] {

  def failure(validationFailure: => E):Out[Nothing]

  def failures(validationFailures: => NonEmptyList[E]):Out[Nothing]

  def onFailure[T, S >: T](value:Out[T])(f:NonEmptyList[E] => Out[S]):Out[S]
}

trait PipeFMonad {

  implicit class FailureMonad[Out[+_], E, A](out:Out[A])(implicit monad: FMonad[E, Out]) {
    def onFailure[T >: A](f:NonEmptyList[E] => Out[T]):Out[T] =
      monad.onFailure[A, T](out)(f)
  }
}

trait FMonadOps extends MonadOps {
  def failure[E, Out[+_]](validationFailure: => E)(implicit fmonad:FMonad[E, Out]):Out[Nothing] =
    fmonad.failures(NonEmptyList(validationFailure))

  def failures[E, Out[+_]](validationFailures: => NonEmptyList[E])(implicit fmonad:FMonad[E, Out]):Out[Nothing] =
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

