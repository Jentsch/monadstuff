package monadic

import scala.language.higherKinds
import scalaz.Functor
import scalaz.Scalaz._

/**
  * This traits helps the map Method of the AutoFunctor. Aside testing there
  * should be no reason to construct this objects manually.
  *
  * @tparam InputShape Requested stack of functor and input variables
  * @tparam I Parameter of the function (in)
  * @tparam O Return type of the function (out)
  */
trait AutoFunctorStrategy[InputShape, I, O] {
  /**
    * Returned stack of functor and output variables
    */
  type SO

  def map(data: InputShape)(f: I => O): SO
}


/**
  * This traits helps the map Method of the AutoFunctor.
  *
  * Aside testing there should be no reason to construct this objects manually.
  */
object AutoFunctorStrategy {

  /**
    * End of the recursion if the input type of ```f``` matches the input shape.
    *
    * @tparam I input type
    * @tparam O output type
    */
  implicit def direct[I, O] =
    new AutoFunctorStrategy[I, I, O] {
      override type SO = O

      override def map(data: I)(f: I => O) = f(data)
    }

  /**
    * One recursion step
    *
    * @param inner Strategy to unwrap the remaining layers
    * @tparam F type of outer Functor
    * @tparam ISI inner input shape
    * @tparam I input type
    * @tparam O output type
    */
  implicit def recursive[F[_] : Functor, ISI, I, O](implicit inner: AutoFunctorStrategy[ISI, I, O]) =
    new AutoFunctorStrategy[F[ISI], I, O] {
      override type SO = F[inner.SO]

      override def map(data: F[ISI])(f: I => O): SO =
        data.map(inner.map(_)(f))
    }

}
