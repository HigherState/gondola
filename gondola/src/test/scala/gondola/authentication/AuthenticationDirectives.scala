package gondola.authentication

import cats.data.NonEmptyList
import gondola.repository._
import gondola._
import gondola.services.Get

abstract class AuthenticationDirectives[M[_]:VMonad]
  (repository:KvD[UserLogin, UserCredentials, ?] ~> M) {

  import MonadError._

  protected def withValidUniqueLogin[T](userLogin:UserLogin)(f: => M[T]):M[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        raiseError(NonEmptyList("UserCredentialsAlreadyExistFailure(userLogin)"))
      case _ =>
        f
    }

  protected def withRequiredCredentials[T](userLogin:UserLogin)(f: UserCredentials => M[T]):M[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        f(uc)
      case None =>
        raiseError(NonEmptyList("UserCredentialsNotFoundFailure(userLogin)"))
    }

  protected def withRequiredAuthenticatedCredentials[T](userLogin:UserLogin, password:Password)(f:UserCredentials => M[T]):M[T] =
    withRequiredCredentials(userLogin) { uc =>
      if (uc.password.isMatch(password))
        f(uc)
      else {
        //Event publisher, publish failure of authentication
        raiseError(NonEmptyList("InvalidPasswordFailure(userLogin)"))
      }
    }
}
