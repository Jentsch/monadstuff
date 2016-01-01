package monadic

final case class AutoMonad[Shape](data: Shape) {

  /**
    * map : ```A[B[...X]] -> (X -> Y) -> A[B[...Y]]```, ```X``` could still be a functor.
    *
    * Requires that the type of ```f``` is explicitly given.
    */
  def map[I, O](f: I => O)(implicit strategy: AutoFunctorStrategy[Shape, I, O]): AutoMonad[strategy.SO] =
    AutoMonad(strategy.map(data)(f))

  def flatMap[I, O](f: I => O)(implicit strategy: AutoMonadStrategy[Shape, I, O]): AutoMonad[strategy.SO] =
    AutoMonad(strategy.flatMap(data)(f))
  
  def merge[I, O](f: I => O)(implicit strategy: MergeStrategy[Shape, I, O]): AutoMonad[strategy.SO] =
    AutoMonad(strategy.merge(data)(f))

  def get: Shape = data
}
