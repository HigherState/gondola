package gondola.services

import java.util.UUID
import gondola.{Acknowledged, Ack}


sealed trait SessionDomain[Y]

final case class Entry(user:String) extends SessionDomain[UUID]

final case class Evict(token:UUID) extends SessionDomain[Ack]

final case class Get(token:UUID) extends SessionDomain[Option[String]]



class SimpleSessionService {

  private var nastyState = Map.empty[UUID, String]

  def handle[Y]:Function[SessionDomain[Y], Y] = {

    case Entry(user) =>
      nastyState = nastyState.filter(_._2 == user)
      val newToken = UUID.randomUUID()
      nastyState += (newToken -> user)
      newToken

    case Evict(token) =>
      nastyState -= token
      Acknowledged

    case Get(token) =>
      nastyState.get(token)

  }
}