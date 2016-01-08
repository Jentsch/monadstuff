 monadstuff
============

Some experiments with implicit search and functors and monads to reduce
monad boilerplaid.

 How to use
------------

This library provides three methods: ```selectiveMap```,
```selectiveFlatMap```, and ```selectiveMerge```. Enable

```scala
import scalaz._
import Scalaz._
import monadic.Syntax._

val data: Option[List[String]] = Some("1" :: "2" :: Nil)

val parsed: Option[List[Int]] =
  data.selectiveMap { str: String => str.toInt }
val sum: Option[Int] =
  parsed.selectiveMap { list: List[Int] => list.sum }

println(sum)
```