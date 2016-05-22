package gondola.std

import cats.data.XorT
import cats.{Eval, Monad, MonadError, Traverse}
import gondola.~>


trait ValidMonad {

  implicit def validMonad[E]:MonadError[Valid[E, ?], E] =
    cats.data.XorT.xorTMonadError[Id, E](cats.idInstances)

  implicit def validTraverse[E]:Traverse[Valid[E, ?]] =
    XorT.xorTTraverse[Id, E]

}

object ValidMonads
  extends ValidMonad
    with IdMonad

trait ValidTransformations {

  implicit def id2Valid[E](implicit M:Monad[Valid[E, ?]]): Id ~> Valid[E, ?] =
    IdTransformationOps.fromIdentity[Valid[E, ?]](M)

  implicit def valid2Valid[E]:Valid[E, ?] ~> Valid[E, ?] =
    IdTransformationOps.identity[Valid[E, ?]]

  implicit def eval2Valid[E](implicit M:Monad[Valid[E, ?]]):Eval ~> Valid[E, ?] =
    new (Eval ~> Valid[E, ?]) {
      def apply[A](fa: Eval[A]): Valid[E, A] =
        M.pure(fa.value)
    }

}

object ValidTransformations
  extends ValidTransformations
  with ValidMonad
  with IdMonad
  with IdTransformations
