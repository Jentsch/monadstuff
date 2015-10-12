
import scala.language.higherKinds

case class AutoFunctor[SI](get: SI) {
  def map[I, O, SO](f: I => O)(implicit strategy: AutoFunctorStrategy[SI, I, O, SO]) =
    new AutoFunctor[SO](strategy.map(get, f))
}