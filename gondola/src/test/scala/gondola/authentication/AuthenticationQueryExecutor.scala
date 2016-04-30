package gondola.authentication

import cats.data.NonEmptyList
import gondola._
import gondola.repository._

object AuthenticationQueryExecutor {

  def apply[M[_]:VMonad](repository:KvD[UserLogin, UserCredentials, ?] ~> M):AuthenticationQuery ~> M =
    new AuthenticationDirectives(repository) with (AuthenticationQuery ~~> M) {

      import MonadError._

      def handle[T] = {
        case Authenticate(userLogin, password) =>
          withRequiredAuthenticatedCredentials(userLogin, password) {
            case UserCredentials(actualUserLogin, _, true, _) =>
              raiseError(NonEmptyList("UserLockedFailure(actualUserLogin)"))
            case UserCredentials(actualUserLogin, _, _, _) =>
              pure(actualUserLogin)
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
