
import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.{Bind, Functor}

trait MergeStrategy[S, I, O] {
  type SO

  def merge(data: S)(f: I => O): SO
}

object MergeStrategy {
  implicit def some[F[_] : Functor, B[_] : Bind, RI[_], I, O](implicit raise: Raise[RI, B]) =
    new MergeStrategy[F[B[I]], I, RI[O]] {
      override type SO = F[B[O]]

      override def merge(data: F[B[I]])(f: (I) => RI[O]): SO =
        data.map(_.flatMap(d => raise.raise(f(d))))
    }

  implicit def someSimpler[B[_] : Bind, I, O, R[_]](implicit raise: Raise[R, B]) =
    new MergeStrategy[B[I], I, R[O]] {
      override type SO = B[O]

      override def merge(data: B[I])(f: (I) => R[O]): SO =
        data.flatMap(d => raise.raise(f(d)))
    }
}

trait Raise[I[_], O[_]] {
  def raise[D](in: I[D]): O[D]
}

object Raise {
  type Error[T] = Either[Any, T]

  implicit object RaiseEitherToOption extends Raise[Error, Option] {
    override def raise[D](in: Error[D]): Option[D] =
      in.right.toOption
  }

}