package gondola.repository

import gondola._
import java.util.concurrent.atomic.AtomicReference

object InMemoryKeyValueQueryExecutor {

  def apply[M[_]:Monad, Key, Value](state:AtomicReference[Map[Key, Value]]) =
  new (KvQ[Key, Value]#I ~~> M) {
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
