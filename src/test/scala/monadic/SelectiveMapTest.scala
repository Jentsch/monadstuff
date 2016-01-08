package monadic

import org.scalatest.{FlatSpec, Matchers}
import scalaz._
import Scalaz._
import monadic.Syntax._

class SelectiveMapTest extends FlatSpec with Matchers {
  type A = Int
  type B = String

  it should "behave the same like a normal map on highest nested functor" in {
    """
       (??? : List[A]).
         selectiveMap{ _: A => ??? : A }
         : List[A]
    """ should compile

    """
       (??? : List[A]).
         selectiveMap{ _: A => ??? : B }
         : List[B]
    """ should compile
  }

  it should "not map over disconnected functors" in {
    """
       (??? : Option[List[A]]).
         selectiveMap{ _: Option[A] => ??? : A }
         : List[A]
    """ shouldNot compile
  }

  it should "catch functors in between" in {
    """
       (??? : List[Option[A]]).
         selectiveMap{ _: Option[A] => ??? : A }
         : List[A]
    """ should compile
  }

  it should "unwrap nested functors" in {
    """
       (??? : List[Option[A]]).
         selectiveMap{ _: A => ??? : A }
         : List[Option[A]]
    """ should compile
  }

  it should "handle complex cases" in {
    """
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       (??? : Future[List[Option[NonEmptyList[A]]]]).
         selectiveMap{ _: A => ??? : A }
         : Future[List[Option[NonEmptyList[A]]]]
    """ should compile

    """
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       (??? : Future[List[Option[NonEmptyList[A]]]]).
         selectiveMap{ _: NonEmptyList[A] => ??? : A }
         : Future[List[Option[A]]]
    """ should compile
  }

  it should "accepts generic functions" in {
    """
       import scala.concurrent.Future
       import scala.concurrent.ExecutionContext.Implicits.global

       def head[X](list: List[X]): X = ???

       (??? : Future[List[B]]).
         selectiveMap(head[B])
         : Future[B]
    """ should compile
  }

}