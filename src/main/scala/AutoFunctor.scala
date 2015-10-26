
import scala.language.higherKinds

case class AutoFunctor[SI](get: SI) {
  /**
   * map : ```A[B[...X]] -> (X -> Y) -> A[B[...Y]]```, ```X``` could be a functor.
   * 
   * Requires that the type of ```f``` is explicitly given.
   */
  def map[I, O, SO](f: I => O)(implicit strategy: AutoFunctorStrategy[SI, I, O, SO]) =
    new AutoFunctor[SO](strategy.map(get, f))
}