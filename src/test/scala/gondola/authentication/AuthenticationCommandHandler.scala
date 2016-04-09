package gondola.authentication

import gondola.Monad._
import gondola._
import gondola.repository._

object AuthenticationCommandHandler {

  def apply[M[_]:VMonad](repository: (KvD[UserLogin, UserCredentials])#I ~> M, maxNumberOfTries:Int) =
    new AuthenticationDirectives(repository) with (AuthenticationCommand ~~> M) {

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
              acknowledged
          }

        case ResetFailureCount(userLogin) =>
          repository(Get(userLogin)).flatMap {
            case Some(uc) =>
              repository(Add(uc.userLogin -> uc.copy(failureCount = 0)))
            case None =>
              acknowledged
          }
      }
    }
}
