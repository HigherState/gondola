package gondola.test

import cats.{Eval, MonadError, MonadWriter}
import cats.data.{EitherT, NonEmptyList, WriterT}
import gondola.std._
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.concurrent.Await

trait MonadMatchers {
  import concurrent.duration._

  def invalidate:Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        MatchResult(getError(left).isDefined, "Expected invalid value.", "Unexpected invalid value")
      }
    }

  def validate:Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        MatchResult(getValue(left).isDefined, "Expected valid value.", "Unexpected valid value")
      }
    }

  def errorWith[E](error:E):Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult =
        MatchResult(getError(left).contains(error), s"Expected error '$error'.", s"Did not expect error '$error'")
    }

  def containError[E](error:E):Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val b = getError(left).collect {
          case n:NonEmptyList[_]@unchecked => n.head == error || n.tail.contains(error)
          case s:Seq[_]@unchecked => s.contains(error)
        }.getOrElse(false)
        MatchResult(b, s"Expected error '$error'.", s"Did not expect error '$error'")
      }
    }

  def haveLog[W](log:W):Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult =
        MatchResult(getLog(left).contains(log), s"Expected log '$log'.", s"Did not expect log '$log'")
    }

  def containLogEntry[W](entry:W):Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val b = getLog(left).collect {
          case s:Seq[_]@unchecked => s.contains(entry)
        }.getOrElse(false)
        MatchResult(b, s"Expected log entry '$entry'.", s"Did not expect log entry '$entry'")
      }
    }

  def haveValue[A](a:A):Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult =
        getValue(left).fold{
          MatchResult(false, s"Expected value '$a', no value found.", s"Did not expect value '$a'")
        }{t =>
          MatchResult(t == a, s"Expected value '$a', found '$t'.", s"Did not expect log '$a'")
        }
    }

  def getMonadError[M[_], E, A](m:M[A])(implicit M:MonadError[M, E]) =
    getError(m).map(_.asInstanceOf[E])

  def getMonadLog[M[_], W, A](m:M[A])(implicit M:MonadWriter[M, W]) =
    getLog(m).map(_.asInstanceOf[W])

  def getMonadValue[M[_], A](m:M[A]) =
    getValue(m).map(_.asInstanceOf[A])

  private def getError(a:Any):Option[Any] =
    a match {
      case Left(x) => Some(x)
      case x:EitherT[_,_,_] => getError(x.value)
      case w:WriterT[_, _, _]@unchecked =>
        val r = w.run
        r match {
          case Left(e) => Some(e)
          case Right((w, a)) => None
          case _ => None
        }
      case f:FutureT[_,_]@unchecked =>
        getError(Await.result(f.value, 5.seconds))
      case _ => None
    }

  private def getLog(a:Any):Option[Any] =
    a match {
      case w:WriterT[_, _, _] =>
        w.run match {
          case Right((w, a)) => Some(w)
          case e:Eval[(_, _)]@unchecked => Some(e.value._1)
          case (w, a) => Some(w)
        }
      case f:FutureT[_, _]@unchecked =>
        getLog(Await.result(f.value, 5.seconds))
    }

  private def getValue(a:Any):Option[Any] =
    a match {
      case w:WriterT[_, _, _] =>
        val r = w.run
        r match {
          case Right((w, a)) => getValue(a)
          case (_, a) => Some(a)
          case _ => None
        }
      case Right(x) => getValue(x)
      case Left(_) => None
      case f:FutureT[_, _]@unchecked =>
        getLog(Await.result(f.value, 5.seconds))
      case e:Eval[_] => Some(e.value)
      case a => Some(a)
    }
}

//final class FailWord {
//
//  def apply(right: DObject): Matcher[Valid[_]] =
//    new Matcher[Any] {
//      def apply(left: Any): MatchResult = {
//        val result = left match {
//          case Invalid(h) =>
//            h.toList.contains(right)
//          case _ => false
//        }
//        val message =
//          if (result) ""
//          else left match {
//            case Invalid(h) =>
//              val dif = h.toList.map(f => f.diff(right)).filter(_.size > 0).sortBy(_.size).headOption
//              dif.fold("Bad Diff in FailWord, FIX ME"){d =>
//                "unexpected failure " + d.toString
//              }
//            case _ =>
//              "expected a failure"
//          }
//
//        MatchResult(
//          result,
//          message,
//          "should not have failed with " + right
//        )
//      }
//      override def toString: String = "failWith (" + right.toString + ")"
//    }
//
//  def apply():Matcher[Any] =
//    new Matcher[Any] {
//      def apply(left: Any): MatchResult = {
//        val isFailure = left match {
//          case Invalid(_) =>
//            true
//          case _ =>
//            false
//        }
//        MatchResult(
//          isFailure,
//          "should have failed",
//          "should have succeeded"
//        )
//      }
//    }
//}
//
//final class PubValidWord {
//
//  def apply(right: Event*):Matcher[PubValid[_]] =
//    new Matcher[PubValid[_]] {
//      def apply(left: PubValid[_]): MatchResult = {
//        val l = left.run.toOption.map(_._1).getOrElse(Vector.empty)
//        val required =  right.filter(e => !l.contains(e))
//        MatchResult(
//          required.isEmpty,
//          "should contain all of " + required.mkString(","),
//          "should not have contained any of " + (right.toSet -- required).mkString(",")
//        )
//      }
//      override def toString: String = "failWith (" + right.toString + ")"
//    }
//}
//
//final class PubWord {
//
//  def apply(right: Event*):Matcher[Pub[_]] =
//    new Matcher[Pub[_]] {
//      def apply(left: Pub[_]): MatchResult = {
//        val l = left.run._1
//        val required =  right.filter(e => !l.contains(e))
//        MatchResult(
//          required.isEmpty,
//          "should contain all of " + required.mkString(","),
//          "should not have contained any of " + (right.toSet -- required).mkString(",")
//        )
//      }
//      override def toString: String = "failWith (" + right.toString + ")"
//    }
//}
//
//final class SucceedWord {
//  def apply(right: Any): Matcher[Valid[_]] =
//    new BeWord().apply(Valid(right))
//
//  def apply():Matcher[Valid[_]] =
//    new Matcher[Valid[_]] {
//      def apply(left: Valid[_]): MatchResult = {
//        val isSuccess = left.isRight
//        MatchResult(
//          isSuccess,
//          "should have succeeded",
//          "should have failed"
//        )
//      }
//
//    }
//}
//
