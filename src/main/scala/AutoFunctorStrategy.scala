
import scala.language.higherKinds
import scalaz.Functor

trait AutoFunctorStrategy[-SI, +I, -O, +SO] {
  def map(data: SI, f: I => O): SO
}

object AutoFunctorStrategy {
  implicit def endMap[I, O]: AutoFunctorStrategy[I, I, O, O] =
    new AutoFunctorStrategy[I, I, O, O] {
      override def map(data: I, f: I => O): O = f(data)
    }

  implicit def wrap[SI, I, O, SO, F[_]]
  (implicit outer: Functor[F],
   inner: AutoFunctorStrategy[SI, I, O, SO]): AutoFunctorStrategy[F[SI], I, O, F[SO]] =
    new AutoFunctorStrategy[F[SI], I, O, F[SO]] {
      override def map(data: F[SI], f: (I) => O): F[SO] = outer.map(data)(inner.map(_, f))
    }
}
