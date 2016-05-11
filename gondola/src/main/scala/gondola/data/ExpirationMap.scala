package gondola.data

import scala.annotation.tailrec

trait ExpiryManager[T] {

  def getCurrent:T

  def isExpired(value:T, current:T):Boolean

  def isExpired(value:T):Boolean =
    isExpired(value, getCurrent)

  def age(value:T, current:T):T

  def newExpiry():T
}

class SystemCurrentExpiry(interval:Long) extends ExpiryManager[Long] {
  def getCurrent: Long =
    System.currentTimeMillis()

  def isExpired(value: Long, current: Long): Boolean =
    value < current

  def age(value: Long, current: Long): Long =
    current - value

  def newExpiry(): Long =
    getCurrent + interval
}

object ExpirationMap {

  def empty[A, B, T](implicit E:ExpiryManager[T]):ExpirationMap[A, B, T] =
    ExpirationMap(Vector.empty[(A, T)], Map.empty[A, (B,T)])

  def apply[A,B, T](pairs:(A, B)*)(implicit E:ExpiryManager[T]):ExpirationMap[A, B, T] =
    empty[A, B, T] ++ pairs

}


case class ExpirationMap[A, +B, T](expirationQueue:Vector[(A, T)], keyValueExpiry:Map[A,(B,T)])(implicit E:ExpiryManager[T]) extends Map[A, B] {

  def get(key:A):Option[B] =
    keyValueExpiry.get(key) collect {
      case (value, expiry) if !E.isExpired(expiry) => value
    }

  def getUpdate(key:A): Option[(B, ExpirationMap[A, B, T])] = {
    val current = E.getCurrent
    keyValueExpiry.get(key) collect {
      case kv@(value, expiry) if !E.isExpired(expiry, current) =>
        value -> copy(keyValueExpiry = keyValueExpiry + (key -> kv))
    }
  }

  def update(key:A): ExpirationMap[A, B, T] = {
    val current = E.getCurrent
    keyValueExpiry.get(key) collect {
      case kv@(value, expiry) if !E.isExpired(expiry, current) =>
        copy(keyValueExpiry = keyValueExpiry + (key -> kv))
    } getOrElse this
  }

  def iterator: Iterator[(A, B)] = {
    val current = E.getCurrent
    keyValueExpiry.iterator
      .filterNot(p => E.isExpired(p._2._2,current))
      .map(p => (p._1, p._2._1))
  }

  def toMap:Map[A, B] =
    iterator.toMap


  override def empty: Map[A, B] = ExpirationMap.empty[A, B, T]

  override def filter(pred: ((A, B)) => Boolean): ExpirationMap[A, B, T] = {
    val current = E.getCurrent
    ExpirationMap(expirationQueue, keyValueExpiry.filter(p => !E.isExpired(p._2._2, current) && pred(p._1 -> p._2._1)))
  }

  def +[B1 >: B](kv: (A, B1)): ExpirationMap[A, B1, T] = {
    val expire = E.newExpiry()
    val current = E.getCurrent
    val cleared = clearedExpired(current, expirationQueue, keyValueExpiry)
    val newQueue =
      if (cleared.keyValueExpiry.contains(kv._1)) cleared.expirationQueue
      else cleared.expirationQueue :+ (kv._1 -> expire)
    val newMap = cleared.keyValueExpiry + (kv._1 -> (kv._2 -> expire))
    ExpirationMap(newQueue, newMap)
  }

  def ++[B1 >: B](kvs: Iterable[(A, B1)]): ExpirationMap[A, B1, T] = {
    val current = E.getCurrent
    val expire = E.newExpiry()
    val cleared = clearedExpired(current, expirationQueue, keyValueExpiry)
    val newItems = kvs.filter(p => !cleared.keyValueExpiry.contains(p._1)).map(p => p._1 -> expire).toList
    val newQueue = cleared.expirationQueue ++ newItems
    val newMap = cleared.keyValueExpiry ++ kvs.map(p => p._1 -> (p._2 -> expire))
    ExpirationMap(newQueue, newMap)
  }

  def -(key: A): ExpirationMap[A, B, T] = {
    val current = E.getCurrent
    val cleared = clearedExpired(current, expirationQueue, keyValueExpiry)
    if (cleared.keyValueExpiry.contains(key)) ExpirationMap(cleared.expirationQueue, cleared.keyValueExpiry - key)
    else cleared
  }

  def --(keys:Iterable[A]): ExpirationMap[A, B, T] = {
    val current = E.getCurrent
    val cleared = clearedExpired(current, expirationQueue, keyValueExpiry)
    ExpirationMap(cleared.expirationQueue, cleared.keyValueExpiry -- keys)
  }

  @tailrec private def clearedExpired[C >: B](current:T, expirationQueue:Vector[(A, T)], keyValueExpiry:Map[A,(C, T)]):ExpirationMap[A, C, T] =
    expirationQueue match {
      case (key, expiry) +: tail if E.isExpired(expiry,current) =>
        keyValueExpiry.get(key) match {
          case None =>
            clearedExpired(current, tail, keyValueExpiry)
          case Some((_, kvExpiry)) if E.isExpired(kvExpiry,current) =>
            clearedExpired(current, tail, keyValueExpiry - key)
          case Some((_, kvExpiry)) =>
            clearedExpired(current, tail :+ (key -> kvExpiry), keyValueExpiry)
        }
      case _ => ExpirationMap(expirationQueue, keyValueExpiry)
    }


  def length:Int = {
    val current = E.getCurrent
    keyValueExpiry.count(p => !E.isExpired(p._2._2, current))
  }

  override def keys:Iterable[A] = {
    val current = E.getCurrent
    keyValueExpiry.withFilter(p => !E.isExpired(p._2._2,current)).map(_._1)
  }

  def withTime:Iterable[(A, B, T)] = {
    val current = E.getCurrent
    keyValueExpiry.withFilter(p => !E.isExpired(p._2._2, current)).map(p => (p._1, p._2._1, E.age(p._2._2, current)))
  }
}