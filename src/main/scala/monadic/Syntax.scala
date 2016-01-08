package monadic

import scala.language.implicitConversions

/**
  * Import this object to add the selective methods.
  */
object Syntax {

  implicit def selectiveMethods[Data](data: Data): SelectiveMethods[Data] =
    new SelectiveMethods(data)
}
