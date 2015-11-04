
import scala.language.higherKinds
import scalaz.Functor

trait AutoFunctorStrategy[-SI, +I, -O, +SO] {
  def map(data: SI, f: I => O): SO
}

object AutoFunctorStrategy {

  implicit def end[I, O, F[_]](implicit inner: Functor[F]): AutoFunctorStrategy[F[I], I, O, F[O]] =
    new AutoFunctorStrategy[F[I], I, O, F[O]] {
      override def map(data: F[I], f: I => O): F[O] =
        inner.map(data)(f)
    }

  implicit def wrap[SI, I, O, SO, F[_]](implicit f1: Functor[F], inner: AutoFunctorStrategy[SI, I, O, SO]): AutoFunctorStrategy[F[SI], I, O, F[SO]] =
    new AutoFunctorStrategy[F[SI], I, O, F[SO]] {
      override def map(data: F[SI], f: I => O): F[SO] =
        f1.map(data)(inner.map(_, f))
    }
}
