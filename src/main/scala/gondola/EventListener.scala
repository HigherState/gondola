package gondola

trait Event

sealed trait Listener

trait EventListener[E <: Event] extends Listener {

  def handle:PartialFunction[E, Ack]
}

trait EventListenerN[M[_], E <: Event] extends Listener {

  def handle:PartialFunction[E, M[Ack]]
}
