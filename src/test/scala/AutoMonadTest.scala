import org.scalatest.{Matchers, FlatSpec}

class AutoMonadTest extends FlatSpec with Matchers {

  "flatMap" should "use the flatMap of the inner monad" in {
    """
       import scalaz._; import Scalaz._

       AutoMonad(??? : List[Int]).
         flatMap{ i: Int => List(i, i)}.
         get : List[Int]
    """ should compile
  }

  it should "step over an outer functor" in {
    """
       import scalaz._; import Scalaz._

        AutoMonad(??? : List[Option[Int]]).
          flatMap{ i: Int => Option(i * 2)}.
          get : List[Option[Int]]
    """ should compile
  }

  it should "rewrap an inner functor" in {
    """
       import scalaz._; import Scalaz._

        AutoMonad(??? : List[Option[Int]]).
          flatMap{ i: Int => List(i * 2)}.
          get : List[Option[Int]]
    """ should compile
  }

}
