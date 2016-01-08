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
trait SelectiveMapStrategy[InputShape, I, O] {
  /**
    * Returned stack of functor and output variables
    */
  type ReturnType

  def map(data: InputShape)(f: I => O): ReturnType
}


/**
  * This traits helps the map Method of the AutoFunctor.
  *
  * Aside testing there should be no reason to construct this objects manually.
  */
object SelectiveMapStrategy extends MultiFunctorStrategy {

  /**
    * End of the recursion if the input type of ```f``` matches the input shape.
    *
    * @tparam I input type
    * @tparam O output type
    */
  implicit def direct[I, O] =
    new SelectiveMapStrategy[I, I, O] {
      override type ReturnType = O

      override def map(data: I)(f: I => O): ReturnType =
        f(data)
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
  implicit def recursive[F[_] : Functor, ISI, I, O](implicit inner: SelectiveMapStrategy[ISI, I, O]) =
    new SelectiveMapStrategy[F[ISI], I, O] {
      override type ReturnType = F[inner.ReturnType]

      override def map(data: F[ISI])(f: I => O): ReturnType =
        data.map(inner.map(_)(f))
    }

}
