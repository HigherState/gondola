package gondola.services

import gondola.Ack

sealed trait KeyValueDomain[Key, Value, Y] extends Serializable

sealed trait KeyValueCommand[Key, Value, Y] extends KeyValueDomain[Key, Value, Y]
sealed trait KeyValueQuery[Key, Value, Y] extends KeyValueDomain[Key, Value, Y]


final case class Add[Key, Value](kv:(Key, Value)) extends KeyValueCommand[Key, Value, Ack]

final case class Remove[Key, Value](key:Key) extends KeyValueCommand[Key, Value, Ack]


final case class Contains[Key, Value](key:Key) extends KeyValueQuery[Key, Value, Boolean]

final case class Get[Key, Value](key:Key) extends KeyValueQuery[Key, Value, Option[Value]]

final case class Iterator[Key,Value]() extends KeyValueQuery[Key, Value, TraversableOnce[(Key, Value)]]

final case class Values[Key,Value]() extends KeyValueQuery[Key, Value, TraversableOnce[Value]]

