
import scala.language.higherKinds
import scalaz.Functor

/**
  * This traits helps the map Method of the AutoFunctor. Aside testing there
  * should be no reason to construct this objects manually.
  *
  * @tparam SI Requested accepting type
  * @tparam I Parameter of the function (in)
  * @tparam O Return type of the function (out)
  */
trait AutoFunctorStrategy[SI, I, O] {
  /**
    * Shape of the return type.
    */
  type SO

  def map(data: SI, f: I => O): SO
}


/**
  * This traits helps the map Method of the AutoFunctor. Aside testing there
  * should be no reason to construct this objects manually.
  */
object AutoFunctorStrategy {

  /**
    * End of the recursion.
    *
    * @tparam I input type
    * @tparam O output type
    */
  implicit def direct[I, O] =
    new AutoFunctorStrategy[I, I, O] {
      type SO = O

      def map(data: I, f: I => O) = f(data)
    }

  /**
    * One recursion step
    *
    * @param outer Functor to unwrap one layer of functors
    * @param inner Strategy to unwrap the remaining layers
    * @tparam F type of outer Functor
    * @tparam IIS inner input shape
    * @tparam I input type
    * @tparam O output type
    */
  implicit def recursive[F[_], IIS, I, O](implicit outer: Functor[F], inner: AutoFunctorStrategy[IIS, I, O]) =
    new AutoFunctorStrategy[F[IIS], I, O] {
      override type SO = F[inner.SO]

      override def map(data: F[IIS], f: I => O): SO = {
        outer.map(data) { (unwrapped: IIS) =>
          inner.map(unwrapped, f)
        }
      }
    }


}
