package gondola.repository

import gondola._
import java.util.concurrent.atomic.AtomicReference

import cats.~>

object InMemoryKeyValueQueryExecutor {

  def apply[M[_]:Monad, Key, Value](state:AtomicReference[Map[Key, Value]]):KvQ[Key, Value, ?] ~> M =
  new (KvQ[Key, Value, ?] ~~> M) {
    import Monad._

    def handle[T] = {
      case Contains(key) =>
        pure(state.get().contains(key))

      case Get(key) =>
        pure(state.get().get(key))

      case Iterator() =>
        pure(state.get().iterator)

      case Values() =>
        pure(state.get().valuesIterator)
    }
  }

}
