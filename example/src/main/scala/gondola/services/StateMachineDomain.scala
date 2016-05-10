package gondola.services

import java.util.UUID

import cats.data._
import gondola.{Ack, ~>, ~~>}

trait StateMachineDomain[A] extends Serializable

case object GetState extends StateMachineDomain[Int]

case object MoveToOne extends StateMachineDomain[Ack]

case object MoveToTwo extends StateMachineDomain[Ack]

case object MoveToThree extends StateMachineDomain[Ack]


object StateMachineService {

  def apply:StateMachineDomain ~> Become[StateMachineDomain, ?] =
    new (StateMachineDomain ~> Become[StateMachineDomain, ?]) {

      def one[A]: Function[SessionDomain[A], Become[StateMachineDomain, A]] = {
        case GetState =>
          State{ }
      }

      def two[A]: Function[SessionDomain[A], Become[StateMachineDomain, A]] = {

      }

      def three[A]: Function[SessionDomain[A], Become[StateMachineDomain, A]] = {

      }
    }
}

