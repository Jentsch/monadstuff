
import scala.language.higherKinds
import scalaz._
import Scalaz._

trait AutoMonadStrategy[SI, I, O] {
  type SO

  def flatMap(data: SI)(f: I => O): SO
}

object AutoMonadStrategy {
  /**
    * End of recursive search for a AutoMonadStrategy
    * @tparam I accepting type
    * @tparam O returning type
    */
  implicit def direct[I, O] =
    new AutoMonadStrategy[I, I, O] {
      override type SO = O

      override def flatMap(data: I)(f: (I) => O): SO =
        f(data)
    }

  implicit def B[B[_] : Bind, I, O] =
    new AutoMonadStrategy[B[I], I, B[O]] {
      override type SO = B[O]

      override def flatMap(data: B[I])(f: (I) => B[O]): SO =
        data.flatMap(f)
    }

  implicit def FB[F[_] : Functor, B[_] : Bind, I, O] =
    new AutoMonadStrategy[F[B[I]], I, B[O]] {
      override type SO = F[B[O]]

      override def flatMap(data: F[B[I]])(f: (I) => B[O]): SO =
        data.map(_.flatMap(f))
    }

  implicit def BF[B[_] : Applicative : Monad, F[_] : Traverse, I, O] =
    new AutoMonadStrategy[B[F[I]], I, B[O]] {
      override type SO = B[F[O]]

      override def flatMap(data: B[F[I]])(f: (I) => B[O]): SO =
        implicitly[Bind[B]].bind(data)(_.map(f).sequence)
    }

  implicit def FBF[F1[_] : Functor, B[_] : Monad, F2[_] : Traverse, I, O] =
    new AutoMonadStrategy[F1[B[F2[I]]], I, B[O]] {
      override type SO = F1[B[F2[O]]]

      override def flatMap(data: F1[B[F2[I]]])(f: (I) => B[O]): SO =
        data.map(d => implicitly[Bind[B]].bind(d)(_.map(f).sequence))
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
  implicit def map[F[_], IIS, I, O](implicit outer: Functor[F], inner: AutoMonadStrategy[IIS, I, O]) =
    new AutoMonadStrategy[F[IIS], I, O] {
      override type SO = F[inner.SO]

      override def flatMap(data: F[IIS])(f: (I) => O): SO =
        outer.map(data)(inner.flatMap(_)(f))
    }

}