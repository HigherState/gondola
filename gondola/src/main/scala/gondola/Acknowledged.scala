package gondola

trait Ack extends Serializable {
  override def toString: String = "Acknowledged"
}