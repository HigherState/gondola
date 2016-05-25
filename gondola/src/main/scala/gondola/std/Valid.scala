package gondola.std

import cats.{Eval, Monad, MonadError, Traverse}
import gondola.~>


trait ErrorMonad {

  implicit def errorMonad[E]:MonadError[Error[E, ?], E] with Traverse[Error[E, ?]] =
    cats.data.Xor.xorInstances[E]

}

object ErrorMonads
  extends ErrorMonad
    with IdMonad

trait ErrorTransformations {

  implicit def id2Error[E](implicit M:Monad[Error[E, ?]]): Id ~> Error[E, ?] =
    IdTransformationOps.fromIdentity[Error[E, ?]](M)

  implicit def error2Error[E]:Error[E, ?] ~> Error[E, ?] =
    IdTransformationOps.identity[Error[E, ?]]

  implicit def eval2Error[E](implicit M:Monad[Error[E, ?]]):Eval ~> Error[E, ?] =
    new (Eval ~> Error[E, ?]) {
      def apply[A](fa: Eval[A]): Error[E, A] =
        M.pure(fa.value)
    }

}

object ErrorTransformations
  extends ErrorTransformations
  with ErrorMonad
  with IdMonad
  with IdTransformations
