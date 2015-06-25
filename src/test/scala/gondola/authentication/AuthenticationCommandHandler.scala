package gondola.authentication

import gondola._
import gondola.repository._

class AuthenticationCommandHandler[Out[+_]:VMonad]
  (repository: (KvD[UserLogin, UserCredentials])#I ~> Out, maxNumberOfTries:Int)
  extends AuthenticationDirectives(repository) with (AuthenticationCommand ~~> Out) {

  import VMonad._

  def handle[T] = {
    case CreateNewUser(userLogin, password) =>
      withValidUniqueLogin(userLogin) {
        repository(Add(userLogin -> (
          UserCredentials(
            userLogin,
            password,
            false,
            0
          ))
        ))
      }

    case UpdatePasswordWithCurrentPassword(userLogin, currentPassword, newPassword) =>
      withRequiredAuthenticatedCredentials(userLogin, currentPassword) { uc =>
        repository(Add(uc.userLogin -> uc.copy(password = newPassword, isLocked = false)))
      }

    case DeleteUser(userLogin) =>
      withRequiredCredentials(userLogin) { uc =>
        repository(Remove(uc.userLogin))
      }


    case SetLock(userLogin, isLocked) =>
      withRequiredCredentials(userLogin) { uc =>
        repository(Add(uc.userLogin -> uc.copy(isLocked = isLocked)))
      }

    case IncrementFailureCount(userLogin) =>
      repository(Get(userLogin)).flatMap {
        case Some(uc) =>
          val newCount = uc.failureCount + 1
          repository(Add(uc.userLogin -> uc.copy(failureCount = newCount, isLocked = newCount >= maxNumberOfTries)))
        case None =>
          point(Acknowledged)
      }

    case ResetFailureCount(userLogin) =>
      repository(Get(userLogin)).flatMap {
        case Some(uc) =>
          repository(Add(uc.userLogin -> uc.copy(failureCount = 0)))
        case None =>
          point(Acknowledged)
      }
  }
}
