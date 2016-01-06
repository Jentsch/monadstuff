package monadic

import org.scalatest.{FlatSpec, Matchers}
import monadic.Syntax._
import scalaz._
import Scalaz._

class AutoMonadTest extends FlatSpec with Matchers {

  type A = Int


  "flatMap" should "use the flatMap of the inner monad" in {
    """
       (??? : List[A]).
         selectiveFlatMap{ _: A => ??? : List[A] }
         : List[A]
    """ should compile
  }

  it should "step over an outer functor" in {
    """
       (??? : List[Option[A]]).
         selectiveFlatMap{ _: A => ??? : Option[A] }
         : List[Option[A]]
    """ should compile
  }

  it should "rewrap an inner functor" in {
    """
       (??? : List[Option[A]]).
         selectiveFlatMap{ _: A => ??? : List[A] }
         : List[Option[A]]
    """ should compile
  }

  "merge" should "accept Raise definitions" in {
    """
       (??? : Option[A]).
         selectiveMerge{ _: A => ??? : Raise.Error[A] }
         : Option[A]
    """ should compile
  }

  it should "map on an outer functor" in {
    """
       (??? : List[Option[A]]).
         selectiveMerge{ _: A => ??? : Raise.Error[A] }
         : List[Option[A]]
    """ should compile
  }

  it should "flat map on an outer monad" in {
    """
       (??? : List[Option[A]]).
         selectiveMerge{ _: A => ??? : Raise.Error[A] }
         : List[Option[A]]
    """ should compile
  }

}
