package gondola

sealed trait Ack extends Serializable

object Acknowledged extends Ack