
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
    new AutoFunctorStrategy[F[I],I,O,F[O]] {
      override def map(data: F[I], f: I => O): F[O] =
        inner.map(data)(f)
    }
  
  implicit object loiII extends AutoFunctorStrategy[List[Option[Int]],Int,Int,List[Option[Int]]] {
    override def map(data: List[Option[Int]], f: Int => Int): List[Option[Int]] =
      data.map(_.map(f))
  }
  
  implicit def loiOiI[I, O] = new AutoFunctorStrategy[List[Option[I]],Option[I], O,List[O]] {
    override def map(data: List[Option[I]], f: Option[I] => O): List[O] =
      data.map(f)
  }
  /* implicit def wrap[SI, I, O, SO, F[_]]
  (implicit outer: Functor[F],
   inner: AutoFunctorStrategy[SI, I, O, SO]): AutoFunctorStrategy[F[SI], I, O, F[SO]] =
    new AutoFunctorStrategy[F[SI], I, O, F[SO]] {
      override def map(data: F[SI], f: (I) => O): F[SO] = outer.map(data)(inner.map(_, f))
    } */
}
