package gondola.repository

import gondola.{Acknowledged, Domain}

sealed trait KeyValueDomain[Y, Key, Value] extends Domain[Y]

sealed trait KeyValueCommand[Y, Key, Value] extends KeyValueDomain[Y, Key, Value]

case class Add[Key, Value](kv:(Key, Value)) extends KeyValueCommand[Acknowledged, Key, Value]

case class Remove[Key, Value](key:Key) extends KeyValueCommand[Acknowledged, Key, Value]



sealed trait KeyValueQuery[Y, Key, Value] extends KeyValueDomain[Y, Key, Value]

final case class Contains[Key, Value](key:Key) extends KeyValueQuery[Boolean, Key, Value]

final case class Get[Key, Value](key:Key) extends KeyValueQuery[Option[Value], Key, Value]

final case class Iterator[Key,Value]() extends KeyValueQuery[TraversableOnce[(Key, Value)], Key, Value]

final case class Values[Key,Value]() extends KeyValueQuery[TraversableOnce[Value], Key, Value]

