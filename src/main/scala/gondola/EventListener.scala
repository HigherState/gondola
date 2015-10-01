package gondola

import scala.language.higherKinds

trait Event

trait EventListener[E <: Event] {

  def handle:PartialFunction[E, Ack]
}

trait EventListenerN[M[+_], E <: Event] {

  def handle:PartialFunction[E, M[Ack]]
}
