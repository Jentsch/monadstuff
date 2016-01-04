package monadic

import org.scalatest.{FlatSpec, Matchers}

class AutoFunctorTest extends FlatSpec with Matchers {
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

  it should "not map over disconnected functors" in {
    """
       import scalaz._; import Scalaz._

       AutoFunctor(Some(List(1, 2, 3))).
         map{ i: Option[Int] => i.getOrElse(3) }.
         get : List[Int]
    """ shouldNot compile
  }

  it should "catch functors in between" in {
    """
       import scalaz._; import Scalaz._

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

  it should "handle complexe cases" in {
    """
       import scalaz._; import Scalaz._
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       AutoFunctor(Future{???}: Future[List[Option[NonEmptyList[Int]]]]).
         map{ i: Int => i * 2}.
         get : Future[List[Option[NonEmptyList[Int]]]]
    """ should compile

    """
       import scalaz._; import Scalaz._
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       AutoFunctor(Future{???}: Future[List[Option[NonEmptyList[Int]]]]).
         map{ i: NonEmptyList[Int] => i.size }.
         get : Future[List[Option[Int]]]
    """ should compile
  }

  it should "accepts generic functions" in {
    """
       import scalaz._; import Scalaz._
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       def head[X](list: List[X]): X = ???

       AutoFunctor(Future{???}: Future[List[String]]).
         map(head[String]).
         get : Future[String]
    """ should compile
  }

}
