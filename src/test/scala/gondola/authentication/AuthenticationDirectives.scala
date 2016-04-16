package gondola.authentication

import cats.~>
import gondola.repository._
import gondola._

abstract class AuthenticationDirectives[M[_]:VMonad]
  (repository: (KvD[UserLogin, UserCredentials])#I ~> M) {

  import VMonad._

  protected def withValidUniqueLogin[T](userLogin:UserLogin)(f: => M[T]):M[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        failure("UserCredentialsAlreadyExistFailure(userLogin)")
      case _ =>
        f
    }

  protected def withRequiredCredentials[T](userLogin:UserLogin)(f: UserCredentials => M[T]):M[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        f(uc)
      case None =>
        failure("UserCredentialsNotFoundFailure(userLogin)")
    }

  protected def withRequiredAuthenticatedCredentials[T](userLogin:UserLogin, password:Password)(f:UserCredentials => M[T]):M[T] =
    withRequiredCredentials(userLogin) { uc =>
      if (uc.password.isMatch(password))
        f(uc)
      else {
        //Event publisher, publish failure of authentication
        failure("InvalidPasswordFailure(userLogin)")
      }
    }
}
