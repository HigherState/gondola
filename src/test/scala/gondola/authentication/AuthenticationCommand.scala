package gondola.authentication

import gondola.{Acknowledged, Domain}

sealed trait AuthenticationDomain[Y] extends Domain[Y]

sealed trait AuthenticationCommand[Y] extends AuthenticationDomain[Y]

case class DeleteUser(userLogin:UserLogin) extends AuthenticationCommand[Acknowledged]

case class UpdatePasswordWithCurrentPassword(userLogin:UserLogin, currentPassword:Password, newPassword:Password) extends AuthenticationCommand[Acknowledged]

case class CreateNewUser(userLogin:UserLogin, password:Password) extends AuthenticationCommand[Acknowledged]

case class SetLock(userLogin:UserLogin, isLocked:Boolean) extends AuthenticationCommand[Acknowledged]

case class IncrementFailureCount(userLogin:UserLogin) extends AuthenticationCommand[Acknowledged]

case class ResetFailureCount(userLogin:UserLogin) extends AuthenticationCommand[Acknowledged]



sealed trait AuthenticationQuery[R] extends AuthenticationDomain[R]

case class Authenticate(userLogin:UserLogin, password:Password) extends AuthenticationQuery[UserLogin]

case class IsLocked(userLogin:UserLogin) extends AuthenticationQuery[Boolean]

case object GetLockedUserLogins extends AuthenticationQuery[TraversableOnce[UserCredentials]]


