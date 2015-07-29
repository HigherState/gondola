package gondola.repository

import gondola._
import java.util.concurrent.atomic.AtomicReference

object InMemoryKeyValueQueryExecutor {

  def apply[Out[+_]:Monad, Key, Value](state:AtomicReference[Map[Key, Value]]) =
  new (KvQ[Key, Value]#I ~~> Out) {
    import Monad._

    def handle[T] = {
      case Contains(key) =>
        point(state.get().contains(key))

      case Get(key) =>
        point(state.get().get(key))

      case Iterator() =>
        point(state.get().iterator)

      case Values() =>
        point(state.get().valuesIterator)
    }
  }

}
