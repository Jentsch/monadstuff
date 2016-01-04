package monadic

import org.scalatest.{Matchers, FlatSpec}

class MultiFunctorStrategyTest extends FlatSpec with Matchers {

  "the examples" should "be correct" in {
    val result = AutoFunctor(('hello, "world")).
      map { world: String => world.length }.get
    //            ^- the type defines static that we map the second element
    result ===('hello, 5)
  }

  "tuples" should "map on the first argument" in {
    """
       AutoFunctor(??? : (Int, String)).
         map{ i: Int => i.toDouble }.
         get : (Double, String)
    """ should compile
  }

  it should "map on the second argument" in {
    """
       AutoFunctor(??? : (Int, String)).
         map{ s: String => s.toDouble }.
         get : (Int, Double)
    """ should compile
  }

  "either" should "map on left" in {
    """
       AutoFunctor(??? : Either[Int, String]).
         map{ i: Int => i.toDouble }.
         get : Either[Double, String]
    """ should compile
  }

  it should "map on right" in {
    """
       AutoFunctor(??? : Either[Int, String]).
         map{ s: String => s.toDouble }.
         get : Either[Int, Double]
    """ should compile
  }
}
