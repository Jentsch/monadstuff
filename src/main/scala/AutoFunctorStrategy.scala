
import scala.language.higherKinds
import scalaz.Functor

/**
  *
  * @tparam I Parameter of the function (in)
  * @tparam O Return type of the function (out)
  */
trait AutoFunctorStrategy[SI, I, O] {
  type SO

  def map(data: SI, f: I => O): SO
}

object AutoFunctorStrategy {

  implicit def direct[I, O] =
    new AutoFunctorStrategy[I, I, O] {
      type SO = O

      def map(data: I, f: I => O) = f(data)
    }

  implicit def recursive[F[_], ISI, I, O](implicit outer: Functor[F], inner: AutoFunctorStrategy[ISI, I, O]) =
    new AutoFunctorStrategy[F[ISI], I, O] {
      override type SO = F[inner.SO]

      override def map(data: F[ISI], f: I => O): SO = {
        outer.map(data) { (unwrapped: ISI) =>
          inner.map(unwrapped, f)
        }
      }
    }


}
