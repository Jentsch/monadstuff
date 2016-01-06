
import org.scalatest.{Matchers, FlatSpec}

class Readme extends FlatSpec with Matchers {

  "the examples inside the readme" should "be correct" in {
    """
      import scalaz._
      import Scalaz._
      import monadic.Syntax._

      val data: Option[List[String]] = ???
      val parsed: Option[List[Int]] =
        data.selectiveMap { str: String => str.toInt }
      val sum: Option[Int] =
        parsed.selectiveMap { list: List[Int] => list.sum }

      println(sum)
    """ should compile
  }

  it should "produce the correct result" in {
    import scalaz._
    import Scalaz._
    import monadic.Syntax._

    val data: Option[List[String]] = Some("1" :: "2" :: Nil)
    val parsed: Option[List[Int]] =
      data.selectiveMap { str: String => str.toInt }
    val sum: Option[Int] =
      parsed.selectiveMap { list: List[Int] => list.sum }

    sum shouldBe Some(3)
  }
}
