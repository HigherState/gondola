package gondola.std

import cats.data.{Xor, XorT}
import cats._
import gondola.{WriterTransformation, ~>}

import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal


final case class FutureT[F[_], A](value:Future[F[A]])(implicit ec: ExecutionContext, M:Monad[F], T:Traverse[F]) {

  private implicit lazy val applicative:Applicative[FutureT[F, ?]] = FutureMonads.futureTApplicative[F]

  def map[B](f: A ⇒ B): FutureT[F, B] =
    value match {
      case FulfilledFuture(a) ⇒
        FutureT[F, B](FulfilledFuture(M.map(a)(f)))
      case e:ErrorFuture    ⇒
        FutureT[F, B](e)
      case _ ⇒
        FutureT[F, B](value.map(M.map(_)(f)))
    }

  def mapF[G[_]:Monad:Traverse, B](f: F[A] => G[B]):FutureT[G, B] =
    FutureT[G, B](value.map(f))

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

  private[std] def fastF[F[_], A](x:F[A])(implicit ec:ExecutionContext, M:Monad[F], T:Traverse[F]) =
    FutureT[F, A](FulfilledFuture(x))
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
    //FutureT(this).transformWith(f).value
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
    //FutureT(this).transformWith(f).value
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
        FutureT.fastF(transform(fa))
    }

  def fromFutureTTransform[M[_], N[_]](implicit M:Monad[N], T:Traverse[N], transform:M ~> N): FutureT[M, ?] ~> FutureT[N, ?] =
    new (FutureT[M, ?] ~> FutureT[N, ?]) {
      def apply[A](fa: FutureT[M, A]): FutureT[N, A] =
        fa.mapF(transform.apply)
    }

  def fromFutureTransform[N[_]](implicit ec: ExecutionContext, M:Monad[N], T:Traverse[N]): Future ~> FutureT[N, ?] =
    new (Future[?] ~> FutureT[N, ?]) {
      def apply[A](fa: Future[A]): FutureT[N, A] =
        FutureT[N, A](fa.map(M.pure))
    }

  implicit def id2Future(implicit ec: ExecutionContext): Id ~> Future =
    fromIdentity[Future]

  implicit def id2FutureT(implicit ec: ExecutionContext): Id ~> FutureT[Id, ?] =
    new (Id ~> FutureT[Id, ?]) {
      def apply[A](fa: Id[A]): FutureT[Id, A] = FutureT.fast(fa)
    }

  implicit val future2future: Future ~> Future =
    identity[Future]

  implicit val futureT2futureT: FutureT[Id, ?] ~> FutureT[Id, ?] =
    identity[FutureT[Id, ?]]
}

trait FutureWriterMonads[W] extends FutureMonads with WriterMonads[W] {

  implicit def futureWriterMonad(implicit ec:ExecutionContext): MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] =
    new MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] {
      def writer[A](aw: (W, A)): FutureWriter[W, A] =
        FutureT.fastF[Writer[W, ?], A](writerMonad.writer(aw))

      def listen[A](fa: FutureWriter[W, A]): FutureWriter[W, (W, A)] =
        FutureT[Writer[W, ?], (W, A)](fa.value.map(r => writerMonad.writer(r.run._1 -> r.run)))

      def pass[A](fa: FutureWriter[W, ((W) => W, A)]): FutureWriter[W, A] =
        FutureT[Writer[W, ?], A](fa.value.map(f => writerMonad.pass(f)))

      def handleErrorWith[A](fa: FutureWriter[W, A])(f: (Throwable) => FutureWriter[W, A]): FutureWriter[W, A] =
        ???

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

  implicit val futureT2FutureWriter:FutureT[Id, ?] ~> FutureWriter[W, ?] =
    fromFutureTTransform[Id, Writer[W, ?]]

  implicit val futureWriter2futureWriter =
    identity[FutureWriter[W, ?]]

  implicit val futureWriter2future:WriterTransformation[FutureWriter[W, ?], Future, W] =
    new WriterTransformation[FutureWriter[W, ?], Future, W] {
      def apply[A](fa: FutureWriter[W, A]): Future[(W, A)] =
        fa.mapF[Id, (W, A)](w => w.run).value
    }

}

trait FutureValidMonads[E] extends FutureMonads with ValidMonads[E] {

  implicit def futureValidMonad(implicit ec:ExecutionContext):MonadError[FutureValid[E, ?], E] =
    new MonadError[FutureValid[E, ?], E] {
      def raiseError[A](e: E): FutureValid[E, A] =
        FutureT.fastF[Valid[E,?], A](validMonad.raiseError[A](e))

      def handleErrorWith[A](fa: FutureValid[E, A])(f: (E) => FutureValid[E, A]): FutureValid[E, A] =
        FutureT[Valid[E, ?], A] {
          fa.value.flatMap[Valid[E, A]] {
            _.value match {
              case Xor.Left(e) => f(e).value
              case Xor.Right(a) => FulfilledFuture(validMonad.pure(a))
            }
          }
        }

      def pure[A](x: A): FutureValid[E, A] =
        FutureT.fast[Valid[E, ?], A](x)

      def flatMap[A, B](fa: FutureValid[E, A])(f: (A) => FutureValid[E, B]): FutureValid[E, B] =
        fa.flatMap(f)
    }
}

trait FutureValidTransforms[E] extends FutureValidMonads[E] with FutureTransfoms with ValidTransforms[E] {

  implicit def id2FutureValid(implicit ec:ExecutionContext):Id ~> FutureValid[E, ?] =
    toFutureTransform[Id, Valid[E, ?]]

  implicit def valid2FutureValid(implicit ec:ExecutionContext):Valid[E, ?] ~> FutureValid[E, ?] =
    toFutureTransform[Valid[E, ?], Valid[E, ?]]

  implicit def future2FutureValid(implicit ec:ExecutionContext):Future ~> FutureValid[E, ?] =
    fromFutureTransform[Valid[E, ?]]

  implicit val futureT2FutureValid:FutureT[Id, ?] ~> FutureValid[E, ?] =
    fromFutureTTransform[Id, Valid[E, ?]]

  implicit val futureValid2futureWriter =
    identity[FutureValid[E, ?]]
}

trait FutureWriterValidMonads[W,E] extends FutureValidMonads[E] with FutureWriterMonads[W] with WriterValidMonads[W, E] {

  implicit def futureWriterValidMonad(implicit ec:ExecutionContext):MonadWriter[FutureWriterValid[W, E, ?], W] with MonadError[FutureWriterValid[W, E, ?], E] =
    new MonadWriter[FutureWriterValid[W, E, ?], W] with MonadError[FutureWriterValid[W, E, ?], E] {
      def writer[A](aw: (W, A)): FutureWriterValid[W, E, A] =
        FutureT.fastF[WriterValid[W, E, ?], A](writerValidMonad.writer(aw))

      def listen[A](fa: FutureWriterValid[W, E, A]): FutureWriterValid[W, E, (W, A)] =
        FutureT[WriterValid[W, E, ?], (W, A)](fa.value.map(r => writerValidMonad.listen(r)))

      def pass[A](fa: FutureWriterValid[W, E, ((W) => W, A)]): FutureWriterValid[W, E, A] =
        FutureT[WriterValid[W, E, ?], A](fa.value.map(r => writerValidMonad.pass(r)))

      def handleErrorWith[A](fa: FutureWriterValid[W, E, A])(f: (E) => FutureWriterValid[W, E, A]): FutureWriterValid[W, E, A] =
        FutureT[WriterValid[W, E, ?], A] {
          fa.value.flatMap[WriterValid[W, E, A]] { r =>
            val r2 = r.run
            r2.value match {
              case Xor.Left(e) => f(e).value
              case Xor.Right(a) => FulfilledFuture(writerValidMonad.writer(a))
            }
          }
        }

      def raiseError[A](e: E): FutureWriterValid[W, E, A] =
        FutureT.fastF[WriterValid[W, E, ?], A](writerValidMonad.raiseError(e))

      def pure[A](x: A): FutureWriterValid[W, E, A] =
        FutureT.fast[WriterValid[W, E, ?], A](x)

      def flatMap[A, B](fa: FutureWriterValid[W, E, A])(f: (A) => FutureWriterValid[W, E, B]): FutureWriterValid[W, E, B] =
        fa.flatMap(f)
  }
}

trait FutureWriterValidTransforms[W, E] extends FutureWriterValidMonads[W, E] with FutureWriterTransforms[W] with FutureValidTransforms[E] with WriterValidTransforms[W, E] {

  implicit def id2FutureWriterValid(implicit ec:ExecutionContext):Id ~> FutureWriterValid[W, E, ?] =
    toFutureTransform[Id, WriterValid[W, E, ?]]

  implicit def future2FutureWriterValid(implicit ec:ExecutionContext):Future ~> FutureWriterValid[W, E, ?] =
    fromFutureTransform[WriterValid[W, E, ?]]

  implicit val futureT2FutureWriterValid:FutureT[Id, ?] ~> FutureWriterValid[W, E, ?] =
    fromFutureTTransform[Id, WriterValid[W, E, ?]]

  implicit def writer2FutureWriterValid(implicit ec:ExecutionContext):Writer[W, ?] ~> FutureWriterValid[W, E, ?] =
    toFutureTransform[Writer[W, ?], WriterValid[W, E, ?]]

  implicit def valid2FutureWriterValid(implicit ec:ExecutionContext):Valid[E, ?] ~> FutureWriterValid[W, E, ?] =
    toFutureTransform[Valid[E, ?], WriterValid[W, E, ?]]

  implicit val futureValid2FutureWriterValid:FutureValid[E, ?] ~> FutureWriterValid[W, E, ?] =
    fromFutureTTransform[Valid[E, ?], WriterValid[W, E, ?]]

  implicit val futureWriter2FutureWriterValid:FutureWriter[W, ?] ~> FutureWriterValid[W, E, ?] =
    fromFutureTTransform[Writer[W, ?], WriterValid[W, E, ?]]

  implicit def writerValid2FutureWriterValid(implicit ec:ExecutionContext):WriterValid[W, E, ?] ~> FutureWriterValid[W, E, ?] =
    toFutureTransform[WriterValid[W, E, ?], WriterValid[W, E, ?]]

  implicit val futureWriterValid2FutureWriterValid:FutureWriterValid[W, E, ?] ~> FutureWriterValid[W, E, ?] =
    identity[FutureWriterValid[W, E, ?]]

  implicit val futureWriterValid2FutureValid:WriterTransformation[FutureWriterValid[W, E, ?], FutureValid[E, ?], W] =
    new WriterTransformation[FutureWriterValid[W, E, ?], FutureValid[E, ?], W] {
      def apply[A](fa: FutureWriterValid[W, E, A]): FutureValid[E, (W, A)] =
        fa.mapF[Valid[E, ?], (W, A)](_.run)
    }
}



