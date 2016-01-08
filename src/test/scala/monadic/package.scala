import scalaz.Alpha.T

package object monadic {

  // Some unrelated types (means no inheritance and no implicit conversions
  type A = Int
  type B = String
  type C = None.type

  def some[T]: T = ???

  def someFunction[T]: T = ???
}
