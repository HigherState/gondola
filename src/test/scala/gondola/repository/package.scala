package gondola

package object repository {
  trait TF {
    type I[Y]
  }
  type KvC[Key, Value] = TF { type I[Y] = KeyValueCommand[Y, Key, Value] }
  type KvQ[Key, Value] = TF { type I[Y] = KeyValueQuery[Y, Key, Value] }
  type KvD[Key, Value] = TF { type I[Y] = KeyValueDomain[Y, Key, Value] }
}
