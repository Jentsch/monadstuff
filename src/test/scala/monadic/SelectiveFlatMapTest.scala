package monadic

import org.scalatest.{FlatSpec, Matchers}
import monadic.Syntax._
import scalaz._
import Scalaz._

class SelectiveFlatMapTest extends FlatSpec with Matchers {


  "flatMap" should "use the flatMap of the inner monad" in {
    """
       some[List[A]].
         selectiveFlatMap{ someFunction[A => List[A]] }
         : List[A]
    """ should compile
  }

  it should "behave like map" in {
    """
       some[A].
         selectiveFlatMap{ someFunction[A => B] }
         : B

       some[List[A]].
         selectiveFlatMap{ someFunction[A => B] }
         : List[B]
    """ should compile
  }

  it should "step over an outer functor" in {
    """
       some[List[Option[A]]].
         selectiveFlatMap{ someFunction[A => Option[A]] }
         : List[Option[A]]
    """ should compile
  }

  it should "rewrap an inner functor" in {
    """
       some[List[Option[A]]].
         selectiveFlatMap{ someFunction[A => List[A]] }
         : List[Option[A]]
    """ should compile

    // concrete example
    val example = List(Some(1), Some(2), None).
      selectiveFlatMap { i: Int => List(i, i * 2) }

    example shouldBe List(Some(1), Some(2), Some(2), Some(4), None)
  }

  "the counterexample" should "not compile" in {
    """
       List(1).selectiveFlatMap(i => List(i, i * 2)
    """ shouldNot compile

    // it shouldn't compile because `i` has not type annotation
  }

}
