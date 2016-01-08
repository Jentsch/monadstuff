package monadic

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz.{Bind, Functor}

/**
  *
  * @see [[monadic.SelectiveMethods#selectiveMerge]]
  * @tparam S shape/type of the input data structure
  */
trait SelectiveMergeStrategy[S, I, O] {
  /**
    * Return type of this function. Computed by the implicit resolution process.
    */
  type ReturnType

  def merge(data: S)(f: I => O): ReturnType
}

object SelectiveMergeStrategy {
  implicit def some[F[_] : Functor, B[_] : Bind, RI[_], I, O](implicit raise: Raise[RI, B]) =
    new SelectiveMergeStrategy[F[B[I]], I, RI[O]] {
      override type ReturnType = F[B[O]]

      override def merge(data: F[B[I]])(f: (I) => RI[O]): ReturnType =
        data.map(_.flatMap(d => raise.raise(f(d))))
    }

  implicit def someSimpler[B[_] : Bind, I, O, R[_]](implicit raise: Raise[R, B]) =
    new SelectiveMergeStrategy[B[I], I, R[O]] {
      override type ReturnType = B[O]

      override def merge(data: B[I])(f: (I) => R[O]): ReturnType =
        data.flatMap(d => raise.raise(f(d)))
    }
}

/**
  * A raise describes a implicit conversion from on functor ```I``` to an other functor ```O``` in a
  * ```selectiveMerge```. In difference to normal implicit conversions this will always applied.
  *
  * @see monadic.Syntax
  * @tparam I
  * @tparam O
  */
trait Raise[I[_], O[_]] {
  def raise[D](in: I[D]): O[D]
}
