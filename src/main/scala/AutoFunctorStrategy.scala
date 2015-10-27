
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

  implicit def liLiI[I, O, F[_]](implicit inner: Functor[F]): AutoFunctorStrategy[F[I], I, O, F[O]] =
    new AutoFunctorStrategy[F[I], I, O, F[O]] {
      override def map(data: F[I], f: I => O): F[O] =
        inner.map(data)(f)
    }

  implicit def loiII[I, O, F1[_], F2[_]](implicit f1: Functor[F1], f2: Functor[F2]): AutoFunctorStrategy[F1[F2[I]], I, O, F1[F2[O]]] =
    new AutoFunctorStrategy[F1[F2[I]], I, O, F1[F2[O]]] {
      override def map(data: F1[F2[I]], f: I => O): F1[F2[O]] =
        f1.map(data)(f2.map(_)(f))
    }

  /* implicit def wrap[SI, I, O, SO, F[_]]
  (implicit outer: Functor[F],
   inner: AutoFunctorStrategy[SI, I, O, SO]): AutoFunctorStrategy[F[SI], I, O, F[SO]] =
    new AutoFunctorStrategy[F[SI], I, O, F[SO]] {
      override def map(data: F[SI], f: (I) => O): F[SO] = outer.map(data)(inner.map(_, f))
    } */
}
