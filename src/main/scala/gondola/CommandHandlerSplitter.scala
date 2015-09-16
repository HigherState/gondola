package gondola

import scala.language.higherKinds

object CommandHandlerSplitter {

  def apply[M[+_]:Monad, C[_]](primary:C ~> M, handlers:C ~> M*):C ~> M =
    new (C ~> M) {
      import Monad._
      def apply[A](fa: C[A]): M[A] =
        for {
          r <- primary.apply(fa)
          _ <- handlers.map(_.apply(fa)).sequence
        } yield r
    }
}
