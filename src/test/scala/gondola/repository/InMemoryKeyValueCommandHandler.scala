package gondola.repository

import cats.data.State
import gondola._

object InMemoryKeyValueCommandHandler {

  def apply[Key, Value]:KvC[Key,Value, ?] ~> State[Map[Key, Value], ?] =
    new (KvC[Key,Value, ?] ~~> State[Map[Key, Value], ?]) {

      import Monad._

      def handle[T] = {
        case Add(kv) =>
          State[Map[Key, Value], Ack](s => ((s + kv), Acknowledged))
        case Remove(key:Key) =>
          State[Map[Key, Value], Ack](s => ((s - key),Acknowledged))
      }
    }
}

