package monadic


object Syntax {

  implicit final class SelectiveMethods[Data](val data: Data) extends AnyVal {
    def selectiveMap[I, O](f: I => O)(implicit strategy: AutoFunctorStrategy[Data, I, O]): strategy.SO =
      strategy.map(data)(f)

    def selectiveFlatMap[I, O](f: I => O)(implicit strategy: AutoMonadStrategy[Data, I, O]): strategy.SO =
      strategy.flatMap(data)(f)

    def selectiveMerge[I, O](f: I => O)(implicit strategy: MergeStrategy[Data, I, O]): strategy.SO =
      strategy.merge(data)(f)
  }

}
