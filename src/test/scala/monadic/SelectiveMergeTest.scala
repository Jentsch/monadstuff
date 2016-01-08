package monadic

import org.scalatest.{Matchers, FlatSpec}
import scalaz._
import Scalaz._
import monadic.Syntax._

class SelectiveMergeTest extends FlatSpec with Matchers {

  type Error[T] = Either[Any, T]

  implicit def RaiseErrorToOption: Raise[Error, Option] = ???


  "merge" should "accept Raise definitions" in {
    """
       some[Option[A]].
         selectiveMerge{ someFunction [A => Error[A]] }
         : Option[A]
    """ should compile
  }

  it should "map on an outer functor" in {
    """
       some[List[Option[A]]].
         selectiveMerge{ someFunction[A => Error[A]] }
         : List[Option[A]]
    """ should compile
  }

  it should "flat map on an outer monad" in {
    """
       some[List[Option[A]]].
         selectiveMerge{ someFunction[A => Error[A]] }
         : List[Option[A]]
    """ should compile
  }

}
