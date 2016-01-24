 monadstuff
============

Some experiments with implicit search and functors and monads to reduce
monad boilerplaid.

 How to use
------------

This library provides three methods: `selectiveMap`,
`selectiveFlatMap`, and `selectiveMerge`. Enable them by
importing `monadic.Syntax._`.

`selectiveMap`
--------------

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

`selectiveFlatMap`
------------------
`selectiveFlatMap` works like `selectiveMap` until the given function
returns a monad that's also part of the original data structure. In
this case a flat map will be issued.

```scala
import scalaz._
import Scalaz._
import monadic.Syntax._

val data: List[Option[String]] = Some("1") :: Some("2") :: None :: Nil

val pairs: List[Option[Int]] =
  data.selectiveFlatMap { str: String => List(str.toInt, str.length) }

println(pairs)
```

prints:
```
List(Some(1), Some(1), Some(2), Some(1), None)
```


`selectiveMerge`
----------------
Like `selectiveFlatMap` but considers additional rules to
flatten monades into each other.
