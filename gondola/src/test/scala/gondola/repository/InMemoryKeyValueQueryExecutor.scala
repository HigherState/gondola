package gondola.repository

import gondola._
import cats.data.Reader

object InMemoryKeyValueQueryExecutor {

  def apply[Key, Value]:KvQ[Key, Value, ?] ~> Reader[Map[Key, Value], ?] =
    new (KvQ[Key, Value, ?] ~~> Reader[Map[Key, Value], ?]) {

      def handle[T] = {
        case Contains(key) =>
          Reader[Map[Key, Value], T](_.contains(key))

        case Get(key) =>
          Reader[Map[Key, Value], T](_.get(key))

        case Iterator() =>
          Reader[Map[Key, Value], T](_.iterator)

        case Values() =>
          Reader[Map[Key, Value], T](_.values)
      }
    }

}
