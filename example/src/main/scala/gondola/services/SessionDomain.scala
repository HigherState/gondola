package gondola.services

import java.util.UUID
import cats.data.State
import gondola.std.Id
import gondola.{~>, ~~>, Acknowledged, Ack}


sealed trait SessionDomain[Y] extends Serializable

final case class Entry(user:String) extends SessionDomain[UUID]

final case class Evict(token:UUID) extends SessionDomain[Ack]

final case class Retrieve(token:UUID) extends SessionDomain[Option[String]]



class SimpleSessionService {

  private var nastyState = Map.empty[UUID, String]

  def handle[Y]:Function[SessionDomain[Y], Y] = {

    case Entry(user) =>
      nastyState = nastyState.filterNot(_._2 == user)
      val newToken = UUID.randomUUID()
      nastyState += (newToken -> user)
      newToken

    case Evict(token) =>
      nastyState -= token
      Acknowledged

    case Retrieve(token) =>
      nastyState.get(token)

  }
}









class StateSessionService {

  def handle[Y]:Function[SessionDomain[Y], State[Map[UUID, String], Y]] = {
    case Entry(user) =>
      State{state:Map[UUID,String] =>
        val newToken = UUID.randomUUID()
        val newState = state.filterNot(_._2 == user) + (newToken -> user)
        newState -> newToken
      }

    case Evict(token) =>
      State { state: Map[UUID, String] =>
        val newState = state - token
        newState -> Acknowledged
      }

    case Retrieve(token) =>
      State { state: Map[UUID, String] =>
        state -> state.get(token)
      }
  }
}








object SessionService {

  def apply:SessionDomain ~> State[Map[UUID, String], ?] =
    new (SessionDomain ~~> State[Map[UUID, String], ?]) {

      def handle[A]: Function[SessionDomain[A], State[Map[UUID, String], A]] = {
        case Entry(user) =>
          State { current:Map[UUID,String] =>
            val newToken = UUID.randomUUID()
            val newState = current.filterNot(_._2 == user) + (newToken -> user)
            newState -> newToken
          }

        case Evict(token) =>
          State { current: Map[UUID, String] =>
            val newState = current - token
            newState -> Acknowledged
          }

        case Retrieve(token) =>
          State { current: Map[UUID, String] =>
            current -> current.get(token)
          }
      }
    }
}




class StateFold[T](var current:T) extends (State[T, ?] ~> Id) {

  def apply[A](fa: State[T, A]): Id[A] = {
    val (newState, a) = fa.run(current).value
    current = newState
    a
  }
}