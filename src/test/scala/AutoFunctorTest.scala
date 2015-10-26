import org.scalatest.{FlatSpec, Matchers}

class AutoFunctorTest extends FlatSpec with Matchers {

  "AutoFunctors map" should "behave like a Some functor without any inner monad" in {
    val mapped = AutoFunctor("string").map{ (str: String) => str.length } 
    
    mapped === AutoFunctor(6)
  } 
  
  it should "behave the same like a normal map on highest nested functor" in {
    """
       import scalaz._; import Scalaz._

       AutoFunctor(List(1, 2, 3)).
         map{ i: Int => i * 2 }.
         get : List[Int]
    """ should compile
    
    """
       import scalaz._; import Scalaz._
       
       AutoFunctor(List("1", "2", "3")).
         map{ str: String => str.toInt }.
         get : List[Int]
    """ should compile
  }
  
  it should "use Scalaz" in {
    """
       import scalaz._; import Scalaz._

       AutoFunctor(List(1, 2, 3)).
         map{ i: Int => i * 2 }.
         get : List[Int]
    """ should compile
    
    """
       AutoFunctor(List(1, 2, 3)).
         map{ i: Int => i * 2 }.
         get : List[Int]
    """ shouldNot compile
    
    """
       import scalaz._; import Scalaz._

       AutoFunctor(NonEmptyList(1, 2)).
         map{ i: Int => i * 2 }.
         get : NonEmptyList[Int]
    """ should compile
  }
  
  it should "catch functors in between" in {
    """
       AutoFunctor(List(Some(1), Some(2), None)).
         map{ i: Option[Int] => i.getOrElse(3) }.
         get : List[Int]
    """ should compile
  }

  it should "unwrap nested functors" in {
    """
       import scalaz._; import Scalaz._

       AutoFunctor(List(Some(1), Some(2), None)).
         map{ i: Int => i * 2 }.
         get : List[Option[Int]]
    """ should compile
  }

}
