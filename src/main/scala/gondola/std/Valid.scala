package gondola.std

import cats.data.NonEmptyList

trait ValidMonads[E] {

  val validMonad = cats.data.XorT.xorTMonadError[Id, NonEmptyList[E]](cats.Id)
}
