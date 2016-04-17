package gondola.std

import cats.data.Xor
import cats._

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, Future, ExecutionContext}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal


final case class FutureT[F[_], A](value:Future[F[A]])(implicit ec: ExecutionContext, M:Monad[F], T:Traverse[F]) {

  private implicit def applicative:Applicative[FutureT[F, ?]] = FutureMonads.futureTApplicative[F]

  def map[B](f: A ⇒ B): FutureT[F, B] =
    value match {
      case FulfilledFuture(a) ⇒
        FutureT[F, B](FulfilledFuture(M.map(a)(f)))
      case e:ErrorFuture    ⇒
        FutureT[F, B](e)
      case _ ⇒
        FutureT[F, B](value.map(M.map(_)(f)))
    }

  def flatMap[B](f: A ⇒ FutureT[F, B]): FutureT[F, B] =
    value match {
      case FulfilledFuture(a) ⇒
        val m = M.map(a)(f)
        val t = T
          .sequence[FutureT[F, ?], B](m)(applicative)
        FutureT[F, B](t.value.map(mmb => M.flatten(mmb)))

      case e:ErrorFuture    ⇒
        FutureT[F, B](e)
      case _ ⇒
        FutureT[F, B] {
          value.flatMap { a =>
            val m = M.map(a)(f)
            val t = T
              .sequence[FutureT[F, ?], B](m)
            t.value.map(mmb => M.flatten(mmb))
          }
        }
    }


//  def filter(pred: A ⇒ Boolean)(implicit executor: ExecutionContext): Future[A] =
////    flatMap { r ⇒
////      if (pred(r)) future
////      else throw new NoSuchElementException("Future.filter predicate is not satisfied") // FIXME: avoid stack trace generation
////    }
//
//  def foreach(f: A ⇒ Unit)(implicit ec: ExecutionContext): Unit = map(f)
//
//  def transformWith[B](f: Try[A] ⇒ FutureT[F[_], B])(implicit executor: ExecutionContext): FutureT[F[_], B] =
//    transformWith(a ⇒ f(Success(a)), e ⇒ f(Failure(e)))
//
//  def transformWith[B](s: A ⇒ FutureT[F[_], B], f: Throwable ⇒ FutureT[F[_], B])(implicit executor: ExecutionContext): FutureT[F[_], B] = {
//    def strictTransform[T](x: F[T], f: T ⇒ FutureT[F[_], B]) =
//      try f(x)
//      catch { case NonFatal(e) ⇒ ErrorFuture(e) }
//
//    value match {
//      case FulfilledFuture(a) ⇒ strictTransform(a, s)
//      case ErrorFuture(e)     ⇒ strictTransform(e, f)
//      case _ ⇒ future.value match {
//        case None ⇒
//          val p = Promise[B]()
//          future.onComplete {
//            case Success(a) ⇒ p completeWith strictTransform(a, s)
//            case Failure(e) ⇒ p completeWith strictTransform(e, f)
//          }
//          p.future
//        case Some(Success(a)) ⇒ strictTransform(a, s)
//        case Some(Failure(e)) ⇒ strictTransform(e, f)
//      }
//    }
//  }
//
//  def recover[B >: A](pf: PartialFunction[Throwable, B])(implicit ec: ExecutionContext): FutureT[F[_], B] =
//    transformWith(FastFuture.successful, t ⇒ if (pf isDefinedAt t) FastFuture.successful(pf(t)) else future)
//
//  def recoverWith[B >: A](pf: PartialFunction[Throwable, Future[B]])(implicit ec: ExecutionContext): FutureT[F[_], B] =
//    transformWith(FastFuture.successful, t ⇒ pf.applyOrElse(t, (_: Throwable) ⇒ future))
}

object FutureT {
  def fast[F[_], A](x:A)(implicit ec:ExecutionContext, M:Monad[F], T:Traverse[F]):FutureT[F, A] =
    FutureT[F, A](FulfilledFuture(M.pure(x)))
}

//taken from akka http
private case class FulfilledFuture[A](a: A) extends Future[A] {
  def value = Some(Success(a))
  def onComplete[U](f: Try[A] ⇒ U)(implicit executor: ExecutionContext) = Future.successful(a).onComplete(f)
  def isCompleted = true
  def result(atMost: Duration)(implicit permit: CanAwait) = a
  def ready(atMost: Duration)(implicit permit: CanAwait) = this
  def transform[S](f: scala.util.Try[A] => scala.util.Try[S])(implicit executor: scala.concurrent.ExecutionContext): scala.concurrent.Future[S] =
    f(Success(a)) match {
      case util.Success(a) => FulfilledFuture(a)
      case util.Failure(e) => ErrorFuture(e)
    }
  def transformWith[S](f: scala.util.Try[A] => scala.concurrent.Future[S])(implicit executor: scala.concurrent.ExecutionContext): scala.concurrent.Future[S] =
    ???
    //new FastFuture(this).transformWith(f)
}
private case class ErrorFuture(error: Throwable) extends Future[Nothing] {
  def value = Some(Failure(error))
  def onComplete[U](f: Try[Nothing] ⇒ U)(implicit executor: ExecutionContext) = Future.failed(error).onComplete(f)
  def isCompleted = true
  def result(atMost: Duration)(implicit permit: CanAwait) = throw error
  def ready(atMost: Duration)(implicit permit: CanAwait) = this
  def transform[S](f: scala.util.Try[Nothing] => scala.util.Try[S])(implicit executor: scala.concurrent.ExecutionContext): scala.concurrent.Future[S] =
    f(Failure(error)) match {
      case util.Success(a) => FulfilledFuture(a)
      case util.Failure(e) => ErrorFuture(e)
    }

  def transformWith[S](f: scala.util.Try[Nothing] => scala.concurrent.Future[S])(implicit executor: scala.concurrent.ExecutionContext): scala.concurrent.Future[S] =
    ???
    //new FastFuture(this).transformWith(f)
}

object FutureMonads extends FutureMonads

trait FutureMonads {

  implicit def futureMonad(implicit ec: ExecutionContext): MonadError[Future, Throwable] with CoflatMap[Future] =
    new CoflatMap[Future] with MonadError[Future, Throwable]{
      def pure[A](x: A): Future[A] =
        FulfilledFuture(x)

      override def pureEval[A](x: Eval[A]): Future[A] =
        pure(x.value)

      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)

      def handleErrorWith[A](fea: Future[A])(f: Throwable => Future[A]): Future[A] =
        fea.recoverWith { case t => f(t) }

      def raiseError[A](e: Throwable): Future[A] =
        ErrorFuture(e)

      override def handleError[A](fea: Future[A])(f: Throwable => A): Future[A] = fea.recover { case t => f(t) }

      override def attempt[A](fa: Future[A]): Future[Throwable Xor A] =
        (fa map Xor.right) recover { case NonFatal(t) => Xor.left(t) }

      override def recover[A](fa: Future[A])(pf: PartialFunction[Throwable, A]): Future[A] = fa.recover(pf)

      override def recoverWith[A](fa: Future[A])(pf: PartialFunction[Throwable, Future[A]]): Future[A] = fa.recoverWith(pf)

      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

      def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
    }

  implicit def futureTApplicative[F[_]](implicit ec: ExecutionContext, F:Monad[F], T:Traverse[F]):Applicative[FutureT[F, ?]] =
    new Applicative[FutureT[F, ?]] {
      def pure[A](x: A): FutureT[F, A] =
        FutureT(FulfilledFuture(F.pure(x)))

      def ap[A, B](ff: FutureT[F, (A) => B])(fa: FutureT[F, A]): FutureT[F, B] =
        FutureT[F, B] {
          ff.value.flatMap{ funcf =>
            fa.value.map{ af =>
              F.flatMap(af)(a => F.map(funcf)(f => f(a)))
            }
          }
        }

      def map[A, B](fa: FutureT[F, A])(f: (A) => B): FutureT[F, B] =
        FutureT[F, B] {
          fa.value.map{a =>
            F.map(a)(f)
          }
        }

      def product[A, B](fa: FutureT[F, A], fb: FutureT[F, B]): FutureT[F, (A, B)] =
        FutureT[F, (A, B)] {
          fa.value.flatMap{a =>
            fb.value.map{b =>
              F.product(a, b)
            }
          }
        }
    }
}

trait FutureTransfoms extends IdTransforms with FutureMonads {
  def toFutureTransform[M[_], N[_]](implicit ec: ExecutionContext, M:Monad[N], T:Traverse[N], transform:M ~> N): M ~> FutureT[N, ?] =
    new (M ~> FutureT[N, ?]) {
      def apply[A](fa: M[A]): FutureT[N, A] =
        FutureT[N, A](FulfilledFuture(transform(fa)))
    }

  def fromFutureTTransform[M[_], N[_]](implicit ec: ExecutionContext, M:Monad[N], T:Traverse[N], transform:M ~> N): FutureT[M, ?] ~> FutureT[N, ?] =
    new (FutureT[M, ?] ~> FutureT[N, ?]) {
      def apply[A](fa: FutureT[M, A]): FutureT[N, A] =
        FutureT[N, A](fa.value.map(transform.apply))
    }

  def fromFutureTransform[N[_]](implicit ec: ExecutionContext, M:Monad[N], T:Traverse[N]): Future ~> FutureT[N, ?] =
    new (Future[?] ~> FutureT[N, ?]) {
      def apply[A](fa: Future[A]): FutureT[N, A] =
        FutureT[N, A](fa.map(M.pure))
    }

  implicit val id2Future: Id ~> Future =
    fromIdentity[Future]

  implicit val future2future: Future ~> Future =
    identity[Future]
}

trait FutureWriterMonads[W] extends WriterMonads[W] {

  implicit def futureWriterMonad(implicit ec:ExecutionContext): MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] =
    new MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] {
      def writer[A](aw: (W, A)): FutureWriter[W, A] = ???

      def listen[A](fa: FutureWriter[W, A]): FutureWriter[W, (W, A)] = ???

      def pass[A](fa: FutureWriter[W, ((W) => W, A)]): FutureWriter[W, A] =
        FutureT[Writer[W, ?], A](fa.value.map(f => writerMonad.pass(f)))

      def handleErrorWith[A](fa: FutureWriter[W, A])(f: (Throwable) => FutureWriter[W, A]): FutureWriter[W, A] = ???

      def raiseError[A](e: Throwable): FutureWriter[W, A] = ???

      def pure[A](x: A): FutureWriter[W, A] =
        FutureT.fast[Writer[W, ?], A](x)

      def flatMap[A, B](fa: FutureWriter[W, A])(f: (A) => FutureWriter[W, B]): FutureWriter[W, B] =
        fa.flatMap(f)
    }
}

trait FutureWriterTransforms[W] extends FutureWriterMonads[W] with FutureTransfoms with WriterTransforms[W] {

  implicit def id2FutureWriter(implicit ec:ExecutionContext):Id ~> FutureWriter[W, ?] =
    toFutureTransform[Id, Writer[W, ?]]

  implicit def writer2FutureWriter(implicit ec:ExecutionContext):Writer[W, ?] ~> FutureWriter[W, ?] =
    toFutureTransform[Writer[W, ?], Writer[W, ?]]

  implicit def future2FutureWriter(implicit ec:ExecutionContext):Future ~> FutureWriter[W, ?] =
    fromFutureTransform[Writer[W, ?]]

}

