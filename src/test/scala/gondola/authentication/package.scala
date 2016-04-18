package gondola

import java.util.UUID

import cats.Functor.ToFunctorOps
import cats.Monad.ToMonadOps
import cats.data.NonEmptyList
import cats.syntax.{FlatMapSyntax, TraverseSyntax}

package object authentication {

  type VMonad[M[_]] = cats.MonadError[M, NonEmptyList[String]]
  //type FutureValid[+T] = Future[FMonad[String, T]]

  /* illustrative only, passwords not hashed*/
  case class Password(value:String) extends AnyVal {
    def isUnset = value.isEmpty

    def isMatch(password:Password) =
      !isUnset && password.value == value
  }
  object Password {
    def unSet = Password("")
  }

  case class UserLogin(value:String) extends AnyVal

  case class ResetToken(uuid:UUID) extends AnyVal

  case class UserCredentials(
                              userLogin:UserLogin,
                              password:Password,
                              isLocked:Boolean,
                              failureCount:Int)

}
