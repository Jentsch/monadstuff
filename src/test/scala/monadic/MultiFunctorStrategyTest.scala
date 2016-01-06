package monadic

import org.scalatest.{Matchers, FlatSpec}
import monadic.Syntax._

class MultiFunctorStrategyTest extends FlatSpec with Matchers {

  "the examples" should "be correct" in {
    val result = AutoFunctor(('hello, "world")).
      map { world: String => world.length }.get
    //            ^- the type defines static that we map the second element
    result ===('hello, 5)
  }

  "tuples" should "map on the first argument" in {
    """
       (??? : (Int, String)).
         selectiveMap{ i: Int => i.toDouble }
         : (Double, String)
    """ should compile
  }

  it should "map on the second argument" in {
    """
       (??? : (Int, String)).
         selectiveMap{ s: String => s.toDouble }
         : (Int, Double)
    """ should compile
  }

  "either" should "map on left" in {
    """
       (??? : Either[Int, String]).
         selectiveMap{ i: Int => i.toDouble }
         : Either[Double, String]
    """ should compile
  }

  it should "map on right" in {
    """
       (??? : Either[Int, String]).
         selectiveMap{ s: String => s.toDouble }
         : Either[Int, Double]
    """ should compile
  }
}
