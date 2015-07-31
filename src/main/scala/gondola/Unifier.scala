package gondola

import shapeless._
import scala.language.higherKinds

/**
 * Created by jamie.pullar on 28/07/2015.
 */
//trait Unifier[D[_], R[_], L <: HList] extends (D ~> R)
//
//object Example {
//
//  sealed trait M[Y]
//
//  sealed trait C[Y] extends M[Y]
//
//  sealed trait Q[Y] extends M[Y]
//
//  val cid = new (C ~> Id) {
//    override def apply[A](fa: C[A]): Id[A] = ???
//  }
//
//  val qid = new (Q ~> Id) {
//    override def apply[A](fa: Q[A]): Id[A] = ???
//  }
//}
//
//object Unify {
//  import Example._
//
//
//  implicit def uniCNCons[D[_], R[_], H[_], T <: HList]
//    (implicit
//      coproducts:Lazy[],
//      uniT: Lazy[Unifier[D, R, T]]
//      ) =
//      new Unifier[D, R, (H ~> R) :: T] {
//
//        def apply[A](fa: R[A]): R[A] =
//    }
//
//
//  def unify[C <: Coproduct, H <: HList](h:H)(implicit unifier:Unifier[H]):Unifier
//
//  unify(cid :: qid :: HNil)
//}
