package gondola.std

import cats.data.XorT
import cats.{Eval, MonadError, ~>}


trait ValidMonads[E] extends IdTransforms {

  implicit val validMonad:MonadError[Valid[E, ?], E] =
    cats.data.XorT.xorTMonadError[Id, E](cats.Id)

  implicit val validTraverse = XorT.xorTTraverse[Id, E]

}

trait ValidTransforms[E] extends ValidMonads[E] with IdTransforms {

  implicit val id2Valid: Id ~> Valid[E, ?] =
    fromIdentity[Valid[E, ?]]

  implicit val valid2Valid:Valid[E, ?] ~> Valid[E, ?] =
    identity[Valid[E, ?]]

  implicit val eval2Valid:Eval ~> Valid[E, ?] =
    new (Eval ~> Valid[E, ?]) {
      def apply[A](fa: Eval[A]): Valid[E, A] =
        validMonad.pure(fa.value)
    }

}
