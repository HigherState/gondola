package gondola.std

import cats.MonadError


trait ValidMonads[E] {

  val validMonad:MonadError[Valid[E,?], E] =
    cats.data.XorT.xorTMonadError[Id, E](cats.Id)
}
