
import scala.language.higherKinds

/**
  * Allows to automagically get the right functor transformer (?) for the given function.
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
    new AutoFunctor(strategy.map(data, f))
}