package gondola.repository

import java.util.concurrent.atomic.AtomicReference

import cats.~>
import gondola._

object InMemoryKeyValueCommandHandler {

  def apply[M[_]:Monad, Key, Value](state:AtomicReference[Map[Key, Value]]):KvC[Key,Value, ?] ~> M =
    new (KvC[Key,Value, ?] ~~> M) {

      import Monad._

      def handle[T] = {
        case Add(kv) =>
          state.set(state.get() + kv)
          acknowledged
        case Remove(key) =>
          state.set(state.get() - key)
          acknowledged
      }
    }
}

