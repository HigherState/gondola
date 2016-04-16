package gondola

import java.util.UUID

import cats.Functor.ToFunctorOps
import cats.Monad.ToMonadOps
import cats.data.NonEmptyList
import cats.syntax.{TraverseSyntax, FlatMapSyntax}
import cats.~>

package object authentication {

  type VMonad[M[_]] = FMonad[String, M]
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


  object VMonad extends FMonadOps with ToMonadOps with FlatMapSyntax with TraverseSyntax with ToFunctorOps {


    //Taken from Trait PipeFMonad, could just use a with, but it removes a lot of red form intellij...
    implicit class PipeMonad[Out[_], In[_],A](in: In[A])(implicit monad: VMonad[Out], pipe:In ~> Out) {
      def flatMap[T](f:A => Out[T]):Out[T] =
        monad.flatMap(pipe(in))(f)
      def map[T](f:A => T):Out[T] =
        monad.map(pipe(in))(f)
      def onFailure(f:NonEmptyList[String] => Out[A]):Out[A] =
        monad.onFailure[A](pipe(in))(f)
    }
    implicit class FailureMonad[Out[_], A](out:Out[A])(implicit monad: VMonad[Out]) {
      def onFailure(f:NonEmptyList[String] => Out[A]):Out[A] =
        monad.onFailure[A](out)(f)
    }

  }

}
