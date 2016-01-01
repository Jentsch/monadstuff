package monadic

import scala.language.higherKinds

/**
  * Allows to automagically get the right functor transformer for the given function.
  *
  * E.g. you have a ```val list = List(Some(1), None, Some(3))``` and want to increment all integers.
  * This could be done manually:
  * {{{
  *   list.map(option => option.map(int => int * 2))
  *   // or shorter
  *   list.map(_.map(_ * 2))
  * }}}
  * Or by constructing a MonadTransformer (see
  * [[http://underscore.io/blog/posts/2013/12/20/scalaz-monad-transformers.html]])
  *
  * This class offers a third way which is more close to the description above:
  * {{{
  *   AutoFunctor(list).map(i: Int => i + 1)
  *   // ^- introduce magic     ^       ^- Increment
  *   //                        |- type signature is necessary to select the wanted level
  * }}}
  *
  * This works for arbitrary deeply nested functors (think of ```Future[List[Option[Int]]]```) and also for selection
  * that aren't the most inner functor (In the example above ```.map(o: Option[Int] => o.getOrElse(0))``` would behave
  * like the map directly on the list.)
  *
  *
  * @param data item within the auto functor
  * @tparam Shape type of data
  */
case class AutoFunctor[Shape](data: Shape) {
  /**
   * map : ```A[B[...X]] -> (X -> Y) -> A[B[...Y]]```, ```X``` could be a functor.
   * 
   * Requires that the type of ```f``` is explicitly given.
   */
  def map[I, O, SO](f: I => O)(implicit strategy: AutoFunctorStrategy[Shape, I, O]): AutoFunctor[strategy.SO] =
    new AutoFunctor(strategy.map(data)(f))

  /**
    * Returns the data.
    *
    * {{{
    * val result = AutoFunctor(Some(1))
    *   .map(i: Int => i * 2)
    *   // more computations ...
    *   .get
    *
    * result should be(Some(1))
    * }}}
    */
  def get: Shape = data
}
