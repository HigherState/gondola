package gondola.authentication

import gondola._
import gondola.repository._

object AuthenticationQueryExecutor {

  def apply[Out[+_]:VMonad](repository: (KvD[UserLogin, UserCredentials])#I ~> Out) =
    new AuthenticationDirectives(repository) with (AuthenticationQuery ~~> Out) {

      import VMonad._

      def handle[T] = {
        case Authenticate(userLogin, password) =>
          withRequiredAuthenticatedCredentials(userLogin, password) {
            case UserCredentials(actualUserLogin, _, true, _) =>
              failure("UserLockedFailure(actualUserLogin)")
            case UserCredentials(actualUserLogin, _, _, _) =>
              point(actualUserLogin)
          }

        case IsLocked(userLogin) =>
          for {
            mc <- repository(Get(userLogin))
          } yield mc.exists(_.isLocked)

        case GetLockedUserLogins =>
          repository(Values()).map { credentials =>
            credentials.filter(_.isLocked)
          }
      }
    }
}
