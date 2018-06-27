package linalg.vector

import linalg._

/**
  *
  */

trait SetVecLike[V, F] extends VectorSpace[V, F]{

     //def identity(size: Int): V
     def rowReducedEchelon(m: V): V
     def rowEchelon(m: V): V

     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def size(v: V): (Int, Int)
}

object SetVecLike {
     @inline final def apply[V, F](implicit ev: SetVecLike[V, F]): SetVecLike[V, F] = ev
}