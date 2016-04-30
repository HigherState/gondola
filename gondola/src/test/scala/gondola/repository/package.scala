package gondola

import gondola.services.{KeyValueQuery, KeyValueDomain, KeyValueCommand}

package object repository {
  trait TF {
    type I[Y]
  }
  type KvC[Key, Value, A] = KeyValueCommand[A, Key, Value]
  type KvQ[Key, Value, A] = KeyValueQuery[A, Key, Value]
  type KvD[Key, Value, A] = KeyValueDomain[A, Key, Value]
}
