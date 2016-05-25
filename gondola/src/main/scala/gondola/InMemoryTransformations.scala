package gondola

import std._

object InMemoryTransformations {

  def state2id[S](init:S)(implicit T: StateTransformation[State[S,?], Id, S]):State[S, ?] ~> Id =
    new (State[S, ?] ~> Id) {
      private var state = init

      def apply[A](fa: State[S, A]): Id[A] =
        synchronized {
          val (ns, a) = T(fa, state)
          state = ns
          a
        }
    }

  def stateError2Error[S, E](init:S)(implicit T: StateTransformation[StateError[S, E, ?], Error[E, ?], S]):StateError[S, E, ?] ~> Error[E, ?] =
    new (StateError[S, E, ?] ~> Error[E, ?]) {
      private var state = init

      def apply[A](fa: StateError[S, E, A]): Error[E, A] =
        synchronized {
          T(fa, state).map{ p =>
            state = p._1
            p._2
          }
        }
    }
}
