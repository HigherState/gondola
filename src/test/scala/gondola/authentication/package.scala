package gondola

import java.util.UUID

import scalaz.NonEmptyList
import scalaz.syntax.ToMonadOps

package object authentication {

  type VMonad[Out[+_]] = FMonad[String, Out]
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


  object VMonad extends FMonadOps with ToMonadOps {


    //Taken from Trait PipeFMonad, could just use a with, but it removes a lot of red form intellij...
    implicit class PipeMonad[Out[+_], In[+_],A](in: In[A])(implicit monad: VMonad[Out], pipe:In ~> Out) {
      def flatMap[T](f:A => Out[T]):Out[T] =
        monad.bind(pipe(in))(f)
      def map[T](f:A => T):Out[T] =
        monad.map(pipe(in))(f)
      def onFailure[T >: A](f:NonEmptyList[String] => Out[T]):Out[T] =
        monad.onFailure[A, T](pipe(in))(f)
    }
    implicit class FailureMonad[Out[+_], A](out:Out[A])(implicit monad: VMonad[Out]) {
      def onFailure[T >: A](f:NonEmptyList[String] => Out[T]):Out[T] =
        monad.onFailure[A, T](out)(f)
    }

  }

}
