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

  def flatMapF[G[_]:Monad:Traverse, B](f: F[A] => FutureT[G, B]):FutureT[G, B] =
    FutureT[G, B](value.flatMap(a => f(a).value))

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


trait FutureMonad {

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
    }
}

object FutureMonads
  extends FutureMonad
  with IdMonad

trait FutureTransformationOps {
  def toFutureTransform[M[_], N[_]](implicit ec: ExecutionContext, N:Monad[N], T:Traverse[N], T2:M ~> N): M ~> FutureT[N, ?] =
    new (M ~> FutureT[N, ?]) {
      def apply[A](fa: M[A]): FutureT[N, A] =
        FutureT.fastF(T2(fa))(ec, N, T)
    }

  def fromFutureTTransform[M[_], N[_]](implicit M:Monad[N], T:Traverse[N], T2:M ~> N): FutureT[M, ?] ~> FutureT[N, ?] =
    new (FutureT[M, ?] ~> FutureT[N, ?]) {
      def apply[A](fa: FutureT[M, A]): FutureT[N, A] =
        fa.mapF(T2.apply)
    }

  def fromFutureTransform[N[_]](implicit ec: ExecutionContext, N:Monad[N], T:Traverse[N]): Future ~> FutureT[N, ?] =
    new (Future[?] ~> FutureT[N, ?]) {
      def apply[A](fa: Future[A]): FutureT[N, A] =
        FutureT[N, A](fa.map(N.pure)(ec))(ec, N, T)
    }
}

object FutureTransformationOps extends FutureTransformationOps

trait FutureTransformations {

  implicit def id2Future(implicit ec: ExecutionContext, M:Monad[Future]): Id ~> Future =
    IdTransformationOps.fromIdentity[Future](M)

  implicit def id2FutureT(implicit ec: ExecutionContext, N:Monad[Id], T:Traverse[Id]): Id ~> FutureT[Id, ?] =
    new (Id ~> FutureT[Id, ?]) {
      def apply[A](fa: Id[A]): FutureT[Id, A] = FutureT.fast(fa)(ec, N, T)
    }

  implicit val future2future: Future ~> Future =
    IdTransformationOps.identity[Future]

  implicit val futureT2futureT: FutureT[Id, ?] ~> FutureT[Id, ?] =
    IdTransformationOps.identity[FutureT[Id, ?]]
}

object FutureTransfomations
  extends FutureTransformations
  with IdTransformations
  with FutureMonad
  with IdMonad

trait FutureWriterMonad {

  implicit def futureWriterMonad[W](implicit ec:ExecutionContext, MW:MonadWriter[Writer[W, ?], W], T:Traverse[Writer[W, ?]]): MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] =
    new MonadError[FutureWriter[W, ?], Throwable] with MonadWriter[FutureWriter[W, ?], W] {
      def writer[A](aw: (W, A)): FutureWriter[W, A] =
        FutureT.fastF[Writer[W, ?], A](MW.writer(aw))(ec, MW, T)

      def listen[A](fa: FutureWriter[W, A]): FutureWriter[W, (W, A)] =
        FutureT[Writer[W, ?], (W, A)](fa.value.map(r => MW.writer(r.run._1 -> r.run)))

      def pass[A](fa: FutureWriter[W, ((W) => W, A)]): FutureWriter[W, A] =
        FutureT[Writer[W, ?], A](fa.value.map(f => MW.pass(f)))(ec, MW, T)

      def handleErrorWith[A](fa: FutureWriter[W, A])(f: (Throwable) => FutureWriter[W, A]): FutureWriter[W, A] =
        ???

      def raiseError[A](e: Throwable): FutureWriter[W, A] = ???

      def pure[A](x: A): FutureWriter[W, A] =
        FutureT.fast[Writer[W, ?], A](x)

      def flatMap[A, B](fa: FutureWriter[W, A])(f: (A) => FutureWriter[W, B]): FutureWriter[W, B] =
        fa.flatMap(f)
    }
}

object FutureWriterMonads
  extends FutureWriterMonad
  with FutureMonad
  with WriterMonad
  with IdMonad

trait FutureWriterTransformations {

  implicit def id2FutureWriter[W](implicit ec:ExecutionContext, N:Monad[Writer[W, ?]], T:Traverse[Writer[W, ?]], T2:Id ~> Writer[W, ?]):Id ~> FutureWriter[W, ?] =
    FutureTransformationOps.toFutureTransform[Id, Writer[W, ?]](ec, N, T, T2)

  implicit def writer2FutureWriter[W](implicit ec:ExecutionContext, N:Monad[Writer[W, ?]], T:Traverse[Writer[W, ?]], T2:Writer[W, ?] ~> Writer[W, ?]):Writer[W, ?] ~> FutureWriter[W, ?] =
    FutureTransformationOps.toFutureTransform[Writer[W, ?], Writer[W, ?]](ec, N, T, T2)

  implicit def future2FutureWriter[W](implicit ec:ExecutionContext, N:Monad[Writer[W, ?]], T:Traverse[Writer[W, ?]]):Future ~> FutureWriter[W, ?] =
    FutureTransformationOps.fromFutureTransform[Writer[W, ?]](ec, N, T)

  implicit def futureT2FutureWriter[W](implicit M:Monad[Writer[W, ?]], T:Traverse[Writer[W, ?]], T2:Id ~> Writer[W, ?]):FutureT[Id, ?] ~> FutureWriter[W, ?] =
    FutureTransformationOps.fromFutureTTransform[Id, Writer[W, ?]](M, T, T2)

  implicit def futureWriter2futureWriter[W] =
    IdTransformationOps.identity[FutureWriter[W, ?]]

  implicit def futureWriter2future[W](implicit N:Monad[Writer[W, ?]], T:Traverse[Writer[W, ?]]):WriterTransformation[FutureWriter[W, ?], Future, W] =
    new WriterTransformation[FutureWriter[W, ?], Future, W] {
      def apply[A](fa: FutureWriter[W, A]): Future[(W, A)] =
        fa.mapF[Id, (W, A)](w => w.run).value
    }
}

object FutureWriterTransformations
  extends FutureWriterTransformations
  with WriterTransformations
  with FutureTransformations
  with FutureWriterMonad
  with FutureMonad
  with WriterMonad
  with IdMonad

trait FutureErrorMonad  {

  implicit def futureErrorMonad[E](implicit ec:ExecutionContext, ME:MonadError[Error[E, ?], E], T:Traverse[Error[E, ?]]):MonadError[FutureError[E, ?], E] =
    new MonadError[FutureError[E, ?], E] {
      def raiseError[A](e: E): FutureError[E, A] =
        FutureT.fastF[Error[E,?], A](ME.raiseError[A](e))(ec, ME, T)

      def handleErrorWith[A](fa: FutureError[E, A])(f: (E) => FutureError[E, A]): FutureError[E, A] =
        FutureT[Error[E, ?], A] {
          fa.value.flatMap[Error[E, A]] {
            case Xor.Left(e) => f(e).value
            case Xor.Right(a) => FulfilledFuture(ME.pure(a))
          }(ec)
        }(ec, ME, T)

      def pure[A](x: A): FutureError[E, A] =
        FutureT.fast[Error[E, ?], A](x)(ec, ME, T)

      def flatMap[A, B](fa: FutureError[E, A])(f: (A) => FutureError[E, B]): FutureError[E, B] =
        fa.flatMap(f)
    }
}

object FutureErrorMonads
  extends FutureErrorMonad
  with FutureMonad
  with ErrorMonad
  with IdMonad

trait FutureErrorTransformations {

  implicit def id2FutureError[E](implicit ec:ExecutionContext, N:Monad[Error[E, ?]], T:Traverse[Error[E, ?]], T2:Id ~> Error[E, ?]):Id ~> FutureError[E, ?] =
    FutureTransformationOps.toFutureTransform[Id, Error[E, ?]](ec, N, T, T2)

  implicit def error2FutureError[E](implicit ec:ExecutionContext, N:Monad[Error[E, ?]], T:Traverse[Error[E, ?]], T2:Error[E, ?] ~> Error[E, ?]):Error[E, ?] ~> FutureError[E, ?] =
    FutureTransformationOps.toFutureTransform[Error[E, ?], Error[E, ?]](ec, N, T, T2)

  implicit def future2FutureError[E](implicit ec:ExecutionContext, N:Monad[Error[E, ?]], T:Traverse[Error[E, ?]]):Future ~> FutureError[E, ?] =
    FutureTransformationOps.fromFutureTransform[Error[E, ?]](ec, N, T)

  implicit def futureT2FutureError[E](implicit M:Monad[Error[E, ?]], T:Traverse[Error[E, ?]], T2:Id ~> Error[E, ?]):FutureT[Id, ?] ~> FutureError[E, ?] =
    FutureTransformationOps.fromFutureTTransform[Id, Error[E, ?]](M, T, T2)

  implicit def futureError2futureWriter[E] =
    IdTransformationOps.identity[FutureError[E, ?]]
}

object FutureErrorTransformations
  extends FutureErrorTransformations
  with FutureTransformations
  with ErrorTransformations
  with IdTransformations
  with FutureErrorMonad
  with FutureMonad
  with ErrorMonad
  with IdMonad

trait FutureWriterErrorMonad {

  implicit def futureWriterErrorMonad[W, E]
  (implicit
   ec:ExecutionContext,
   MWE:MonadWriter[WriterError[W, E, ?], W] with MonadError[WriterError[W, E, ?], E],
   T:Traverse[WriterError[W, E, ?]]
  ):MonadWriter[FutureWriterError[W, E, ?], W] with MonadError[FutureWriterError[W, E, ?], E] =
    new MonadWriter[FutureWriterError[W, E, ?], W] with MonadError[FutureWriterError[W, E, ?], E] {
      def writer[A](aw: (W, A)): FutureWriterError[W, E, A] =
        FutureT.fastF[WriterError[W, E, ?], A](MWE.writer(aw))(ec, MWE, T)

      def listen[A](fa: FutureWriterError[W, E, A]): FutureWriterError[W, E, (W, A)] =
        FutureT[WriterError[W, E, ?], (W, A)](fa.value.map(r => MWE.listen(r))(ec))(ec, MWE, T)

      def pass[A](fa: FutureWriterError[W, E, ((W) => W, A)]): FutureWriterError[W, E, A] =
        FutureT[WriterError[W, E, ?], A](fa.value.map(r => MWE.pass(r))(ec))(ec, MWE, T)

      def handleErrorWith[A](fa: FutureWriterError[W, E, A])(f: (E) => FutureWriterError[W, E, A]): FutureWriterError[W, E, A] =
        FutureT[WriterError[W, E, ?], A] {
          fa.value.flatMap[WriterError[W, E, A]] { r =>
            r.run match {
              case Xor.Left(e) => f(e).value
              case Xor.Right(a) => FulfilledFuture(MWE.writer(a))
            }
          }(ec)
        }(ec, MWE, T)

      def raiseError[A](e: E): FutureWriterError[W, E, A] =
        FutureT.fastF[WriterError[W, E, ?], A](MWE.raiseError(e))(ec, MWE, T)

      def pure[A](x: A): FutureWriterError[W, E, A] =
        FutureT.fast[WriterError[W, E, ?], A](x)(ec, MWE, T)

      def flatMap[A, B](fa: FutureWriterError[W, E, A])(f: (A) => FutureWriterError[W, E, B]): FutureWriterError[W, E, B] =
        fa.flatMap(f)
  }
}

object FutureWriterErrorMonads
  extends FutureWriterErrorMonad
  with FutureWriterMonad
  with FutureErrorMonad
  with WriterErrorMonad
  with FutureMonad
  with WriterMonad
  with ErrorMonad
  with IdMonad

trait FutureWriterErrorTransformations {

  implicit def id2FutureWriterError[W, E](implicit ec:ExecutionContext, N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Id ~> WriterError[W, E, ?]):Id ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.toFutureTransform[Id, WriterError[W, E, ?]](ec, N, T, T2)

  implicit def future2FutureWriterError[W, E](implicit ec:ExecutionContext, N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]]):Future ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.fromFutureTransform[WriterError[W, E, ?]](ec, N, T)

  implicit def futureT2FutureWriterError[W, E](implicit N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Id ~> WriterError[W, E, ?]):FutureT[Id, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.fromFutureTTransform[Id, WriterError[W, E, ?]](N, T, T2)

  implicit def writer2FutureWriterError[W, E](implicit ec:ExecutionContext, N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Writer[W, ?] ~> WriterError[W, E, ?]):Writer[W, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.toFutureTransform[Writer[W, ?], WriterError[W, E, ?]](ec, N, T, T2)

  implicit def error2FutureWriterError[W, E](implicit ec:ExecutionContext, N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Error[E, ?] ~> WriterError[W, E, ?]):Error[E, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.toFutureTransform[Error[E, ?], WriterError[W, E, ?]](ec, N, T, T2)

  implicit def futureError2FutureWriterError[W, E](implicit N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Error[E, ?] ~> WriterError[W, E, ?]):FutureError[E, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.fromFutureTTransform[Error[E, ?], WriterError[W, E, ?]](N, T, T2)

  implicit def futureWriter2FutureWriterError[W, E](implicit N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:Writer[W, ?] ~> WriterError[W, E, ?]):FutureWriter[W, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.fromFutureTTransform[Writer[W, ?], WriterError[W, E, ?]](N, T, T2)

  implicit def writerError2FutureWriterError[W, E](implicit ec:ExecutionContext, N:Monad[WriterError[W, E, ?]], T:Traverse[WriterError[W, E, ?]], T2:WriterError[W, E, ?] ~> WriterError[W, E, ?]):WriterError[W, E, ?] ~> FutureWriterError[W, E, ?] =
    FutureTransformationOps.toFutureTransform[WriterError[W, E, ?], WriterError[W, E, ?]](ec, N, T, T2)

  implicit def futureWriterError2FutureWriterError[W, E]:FutureWriterError[W, E, ?] ~> FutureWriterError[W, E, ?] =
    IdTransformationOps.identity[FutureWriterError[W, E, ?]]

  implicit def futureWriterError2FutureError[W, E](implicit N:Monad[Error[E, ?]], T:Traverse[Error[E, ?]]):WriterTransformation[FutureWriterError[W, E, ?], FutureError[E, ?], W] =
    new WriterTransformation[FutureWriterError[W, E, ?], FutureError[E, ?], W] {
      def apply[A](fa: FutureWriterError[W, E, A]): FutureError[E, (W, A)] =
        fa.mapF[Error[E, ?], (W, A)](_.run)(N, T)
    }
}

object FutureWriterErrorTransformations
  extends FutureWriterErrorTransformations
    with FutureErrorTransformations
    with FutureTransformations
    with ErrorTransformations
    with IdTransformations
    with FutureWriterErrorMonad
    with FutureErrorMonad
    with FutureWriterMonad
    with FutureMonad
    with WriterMonad
    with ErrorMonad
    with IdMonad

