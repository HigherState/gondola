package gondola.authentication

import gondola.Ack

sealed trait AuthenticationDomain[Y] extends Serializable

sealed trait AuthenticationCommand[Y] extends AuthenticationDomain[Y]

case class DeleteUser(userLogin:UserLogin) extends AuthenticationCommand[Ack]

case class UpdatePasswordWithCurrentPassword(userLogin:UserLogin, currentPassword:Password, newPassword:Password) extends AuthenticationCommand[Ack]

case class CreateNewUser(userLogin:UserLogin, password:Password) extends AuthenticationCommand[Ack]

case class SetLock(userLogin:UserLogin, isLocked:Boolean) extends AuthenticationCommand[Ack]

case class IncrementFailureCount(userLogin:UserLogin) extends AuthenticationCommand[Ack]

case class ResetFailureCount(userLogin:UserLogin) extends AuthenticationCommand[Ack]



sealed trait AuthenticationQuery[R] extends AuthenticationDomain[R]

case class Authenticate(userLogin:UserLogin, password:Password) extends AuthenticationQuery[UserLogin]

case class IsLocked(userLogin:UserLogin) extends AuthenticationQuery[Boolean]

case object GetLockedUserLogins extends AuthenticationQuery[TraversableOnce[UserCredentials]]


