package monadic

import org.scalatest.{Matchers, FlatSpec}
import monadic.Syntax._

class MultiFunctorStrategyTest extends FlatSpec with Matchers {

  "the examples" should "be correct" in {
    val result = ('hello, "world").
      selectiveMap { world: String => world.length }
    //                        ^- the type defines static that we map the second element
    result === ('hello, 5)
  }

  "tuples" should "map on the first argument" in {
    """
       some[(A, B)].
         selectiveMap{ someFunction[A => C] }
         : (C, B)
    """ should compile
  }

  it should "map on the second argument" in {
    """
       some[(A, B)].
         selectiveMap{ someFunction[B => C] }
         : (A, C)
    """ should compile
  }

  "either" should "map on left" in {
    """
       some[Either[A, B]].
         selectiveMap{ someFunction[A => C] }
         : Either[C, B]
    """ should compile
  }

  it should "map on right" in {
    """
       some[Either[A, B]].
         selectiveMap{ someFunction[B => C] }
         : Either[A, C]
    """ should compile
  }
}
