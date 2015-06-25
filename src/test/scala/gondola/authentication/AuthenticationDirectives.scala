package gondola.authentication

import gondola.repository._
import gondola._

abstract class AuthenticationDirectives[Out[+_]:VMonad]
  (repository: (KvD[UserLogin, UserCredentials])#I ~> Out) {

  import VMonad._

  protected def withValidUniqueLogin[T](userLogin:UserLogin)(f: => Out[T]):Out[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        failure("UserCredentialsAlreadyExistFailure(userLogin)")
      case _ =>
        f
    }

  protected def withRequiredCredentials[T](userLogin:UserLogin)(f: UserCredentials => Out[T]):Out[T] =
    repository(Get(userLogin)).flatMap {
      case Some(uc) =>
        f(uc)
      case None =>
        failure("UserCredentialsNotFoundFailure(userLogin)")
    }

  protected def withRequiredAuthenticatedCredentials[T](userLogin:UserLogin, password:Password)(f:UserCredentials => Out[T]):Out[T] =
    withRequiredCredentials(userLogin) { uc =>
      if (uc.password.isMatch(password))
        f(uc)
      else {
        //Event publisher, publish failure of authentication
        failure("InvalidPasswordFailure(userLogin)")
      }
    }
}
