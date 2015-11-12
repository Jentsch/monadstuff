package monadic

import org.scalatest.{FlatSpec, Matchers}

class AutoMonadTest extends FlatSpec with Matchers {

  "flatMap" should "use the flatMap of the inner monad" in {
    """
       import scalaz._; import Scalaz._

       monadic.AutoMonad(??? : List[Int]).
         flatMap{ i: Int => List(i, i)}.
         get : List[Int]
    """ should compile
  }

  it should "step over an outer functor" in {
    """
       import scalaz._; import Scalaz._

        monadic.AutoMonad(??? : List[Option[Int]]).
          flatMap{ i: Int => Option(i * 2)}.
          get : List[Option[Int]]
    """ should compile
  }

  it should "rewrap an inner functor" in {
    """
       import scalaz._; import Scalaz._

        monadic.AutoMonad(??? : List[Option[Int]]).
          flatMap{ i: Int => List(i * 2)}.
          get : List[Option[Int]]
    """ should compile
  }

  "merge" should "accept Raise definitions" in {
    """
       import scalaz._; import Scalaz._

        monadic.AutoMonad(??? : Option[Int]).
          merge{ i: Int => Right(i * 2): Raise.Error[Int]}.
          get : Option[Int]
    """ should compile
  }

  it should "map on an outer functor" in {
    """
       import scalaz._; import Scalaz._

        monadic.AutoMonad(??? : List[Option[Int]]).
          merge{ i: Int => Right(i * 2): Raise.Error[Int]}.
          get : List[Option[Int]]
    """ should compile
  }

  it should "flat map on an outer monad" in {
    """
       import scalaz._; import Scalaz._

        monadic.AutoMonad(??? : List[Option[Int]]).
          merge{ i: Int => Right(i * 2): Raise.Error[Int]}.
          get : List[Option[Int]]
    """ should compile
  }

}
