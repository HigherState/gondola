package gondola.repository

import java.util.concurrent.atomic.AtomicReference
import gondola._

class InMemoryKeyValueCommandHandler[Out[+_]:Monad, Key, Value](state:AtomicReference[Map[Key, Value]])
  extends (KvC[Key,Value]#I ~~> Out) {

  import Monad._

  def handle[T] = {
    case Add(kv) =>
      state.set(state.get() + kv)
      point(Acknowledged)
    case Remove(key) =>
      state.set(state.get() - key)
      point(Acknowledged)
  }
}

