
import scala.language.higherKinds
import scalaz.Functor
import scalaz.Scalaz._

/**
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

object AutoFunctorStrategy {

  implicit def direct[I, O] =
    new AutoFunctorStrategy[I, I, O] {
      override type SO = O

      override def map(data: I)(f: I => O) = f(data)
    }

  implicit def recursive[F[_] : Functor, ISI, I, O](implicit inner: AutoFunctorStrategy[ISI, I, O]) =
    new AutoFunctorStrategy[F[ISI], I, O] {
      override type SO = F[inner.SO]

      override def map(data: F[ISI])(f: I => O): SO =
        data.map(inner.map(_)(f))
    }

}
