package monadic

/**
  * Strategies for data structures with multiple reasonable functor implementations.
  * E.g. a map an tuple could be deal with the first or the second element in tuple. Here we use types to distinguish
  * between both options.
  *
  * Example:
  * {{{
  *   val result = ('hello, "world").
  *     selectiveMap { world: String => world.length }
  *     //                     ^- the type defines static that we map the second element
  *   result === ('hello, 5)
  * }}}
  */
trait MultiFunctorStrategy {

  /**
    * Allows to map on the first element of Tuple2
    */
  implicit def tuple1_2[ISI, I, O, Second](implicit inner: SelectiveMapStrategy[ISI, I, O]) =
    new SelectiveMapStrategy[(ISI, Second), I, O] {
      override type ReturnType = (inner.ReturnType, Second)

      override def map(data: (ISI, Second))(f: (I) => O): ReturnType =
        (inner.map(data._1)(f), data._2)
    }

  /**
    * Allows to map on the second element of Tuple2
    */
  implicit def tuple2_2[ISI, I, O, First](implicit inner: SelectiveMapStrategy[ISI, I, O]) =
    new SelectiveMapStrategy[(First, ISI), I, O] {
      override type ReturnType = (First, inner.ReturnType)

      override def map(data: (First, ISI))(f: (I) => O): (First, inner.ReturnType) =
        (data._1, inner.map(data._2)(f))
    }

  /**
    * Allows to map on left projection of an either
    */
  implicit def either_left[ISI, I, O, Right](implicit inner: SelectiveMapStrategy[ISI, I, O]) =
    new SelectiveMapStrategy[Either[ISI, Right], I, O] {
      override type ReturnType = Either[inner.ReturnType, Right]

      override def map(data: Either[ISI, Right])(f: (I) => O): Either[inner.ReturnType, Right] =
        data.left.map(inner.map(_)(f))
    }

  /**
    * Allows to map on right projection of an either
    */
  implicit def either_right[ISI, I, O, Left](implicit inner: SelectiveMapStrategy[ISI, I, O]) =
    new SelectiveMapStrategy[Either[Left, ISI], I, O] {
      override type ReturnType = Either[Left, inner.ReturnType]

      override def map(data: Either[Left, ISI])(f: (I) => O): Either[Left, inner.ReturnType] =
        data.right.map(inner.map(_)(f))
    }
}
