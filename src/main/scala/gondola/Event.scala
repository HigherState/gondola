package gondola

trait Event

sealed trait Listener

trait EventListener[E <: Event] extends Listener {
  def handle:PartialFunction[E, Unit]
}

trait ServiceListener[Out[+_], E <: Event] extends Listener {
  def handle:PartialFunction[E, Out[Acknowledged]]
}

