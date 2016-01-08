package monadic

final class SelectiveMethods[Data](val data: Data) extends AnyVal {
  def selectiveMap[I, O](f: I => O)(implicit strategy: SelectiveMapStrategy[Data, I, O]): strategy.ReturnType =
    strategy.map(data)(f)

  /**
    * Flattens all monads in the result of {{{f}}} and pulls intermediate functors into them.
    *
    * The type of ```f``` has to be explicitly declared. E.g. {{{ List(1).selectiveFlatMap(i => List(i, i * 2) }}}
    * will not work.
    *
    * @param f
    * @param strategy implicitly constructed transformer. Determinate the result type.
    * @tparam I input type of ```f```
    * @tparam O output type of ```f```
    * @return
    */
  def selectiveFlatMap[I, O](f: I => O)(implicit strategy: SelectiveFlatMapStrategy[Data, I, O]): strategy.ReturnType =
    strategy.flatMap(data)(f)

  /**
    * Like {{{selectiveFlatMap}}} but also flattens different kinds of monads into each other if there is a {{{Raise}}}
    * defined.
    *
    * @see [[monadic.Raise]]
    * @param f
    * @param strategy implicitly constructed transformer. Determinate the result type.
    * @tparam I input type of ```f```
    * @tparam O output type of ```f```
    * @return
    */
  def selectiveMerge[I, O](f: I => O)(implicit strategy: SelectiveMergeStrategy[Data, I, O]): strategy.ReturnType =
    strategy.merge(data)(f)
}