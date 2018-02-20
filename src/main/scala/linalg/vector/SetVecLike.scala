package linalg.vector

import linalg.theory.space.VectorSpace

/**
  *
  */

trait SetVecLike[V, F] extends VectorSpace[V, F]{

     def identity(size: Int): V
     def rowReducedEchelon(m: V): V
     def rowEchelon(m: V): V

     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
}

object SetVecLike {
     @inline final def apply[V, F](implicit ev: SetVecLike[V, F]): SetVecLike[V, F] = ev
}