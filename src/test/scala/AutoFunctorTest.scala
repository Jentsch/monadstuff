import org.scalatest.{FlatSpec, Matchers}

class AutoFunctorTest extends FlatSpec with Matchers {


  "AutoFunctors map" should "behave the same like a normal map" in {

    """
       import scalaz._; import Scalaz._

       AutoFunctor(List(1, 2, 3)).map{ i: Int => i * 2 }.
         get : List[Int]
    """ should compile
  }

  it should "unwrap nested functors" in {
    """
       import scalaz._; import Scalaz._

       AutoFunctor(List(Some(1), Some(2), None)).map{ i: Int => i * 2 }.
         get : List[Option[Int]]
    """ should compile
  }

  it should "catch functors in between" in {
    """
       import scalaz._; import Scalaz._
       import AutoFunctorStrategy._

       AutoFunctor(List(Some(1), Some(2), None)).
         map{ i: Option[Int] => i.getOrElse(3) }(wrap(implicitly[Functor[List]], implicitly[AutoFunctorStrategy[Option[Int], Option[Int], Int, Int]])).
         get : List[Int]
    """ should compile
  }
}
