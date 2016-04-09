package gondola

//import cats.Monoid
//
//case class Writer[L, T](log:L, value:T)(implicit m:Monoid[L]) {
//  def flatMap[S](f: T => Writer[L, S]) = {
//    val Writer(l, result) = f(value)
//    Writer(m.combine(log, l), result)
//  }
//
//  def map[S](f: T => S) =
//    Writer[L, S](log, f(value))
//}
//
//object Writer {
//  def zero[L, T](t:T)(implicit monoid:Monoid[L]) =
//    Writer(monoid.empty, t)
//}
