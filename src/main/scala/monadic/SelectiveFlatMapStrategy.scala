package monadic

import scala.language.higherKinds
import scalaz.Scalaz._
import scalaz._

trait SelectiveFlatMapStrategy[SI, I, O] {
  type ReturnType

  def flatMap(data: SI)(f: I => O): ReturnType
}

object SelectiveFlatMapStrategy extends MapFallback {
  /**
    * End of recursive search for a AutoMonadStrategy
    * @tparam I accepting type
    * @tparam O returning type
    */
  implicit def direct[I, O] =
    new SelectiveFlatMapStrategy[I, I, O] {
      override type ReturnType = O

      override def flatMap(data: I)(f: (I) => O): ReturnType =
        f(data)
    }

  implicit def B[B[_] : Bind, I, O] =
    new SelectiveFlatMapStrategy[B[I], I, B[O]] {
      override type ReturnType = B[O]

      override def flatMap(data: B[I])(f: (I) => B[O]): ReturnType =
        data.flatMap(f)
    }

  implicit def BT[B[_] : Monad, F[_] : Traverse, I, O] =
    new SelectiveFlatMapStrategy[B[F[I]], I, B[O]] {
      override type ReturnType = B[F[O]]

      override def flatMap(data: B[F[I]])(f: (I) => B[O]): ReturnType =
        implicitly[Bind[B]].bind(data)(_.map(f).sequence)
    }

  implicit def BTT[B[_] : Monad, T1[_] : Traverse, T2[_] : Traverse, I, O] =
    new SelectiveFlatMapStrategy[B[T1[T2[I]]], I, B[O]] {
      override type ReturnType = B[T1[T2[O]]]

      override def flatMap(data: B[T1[T2[I]]])(f: (I) => B[O]): ReturnType =
        implicitly[Bind[B]].bind(data)(_.map(_.map(f).sequence).sequence)
    }

  //  implicit def bind[B[_], IIS, I, O](implicit outer: Bind[B], inner: AutoMonadStrategy[IIS, I, B[O]]{type SO = B[O]}) =
  //    new AutoMonadStrategy[B[IIS], I, B[O]] {
  //      override type SO = inner.SO
  //
  //      override def flatMap(data: B[IIS])(f: (I) => B[O]): SO = {
  //        outer.join(outer.map(data)(d => inner.flatMap(d)(f)))
  //        //        outer.bind(data)(f)
  //      }
  //    }

}

trait MapFallback {
  implicit def map[F[_], IIS, I, O](implicit outer: Functor[F], inner: SelectiveFlatMapStrategy[IIS, I, O]) =
    new SelectiveFlatMapStrategy[F[IIS], I, O] {
      override type ReturnType = F[inner.ReturnType]

      override def flatMap(data: F[IIS])(f: (I) => O): ReturnType =
        outer.map(data)(inner.flatMap(_)(f))
    }

}