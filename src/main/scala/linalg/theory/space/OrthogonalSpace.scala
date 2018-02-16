package linalg.theory.space

/**
  *
  */

trait OrthogonalSpace[O, F] extends linalg.VectorSpace[O, F] {

     def isOrthogonal(v: O): Boolean
     def areOrthogonal(v1: O, v2: O): Boolean
     def orthogonalize(v: O): O
}



object OrthogonalSpace {
     @inline final def apply[O, F](implicit ev: OrthogonalSpace[O, F]): OrthogonalSpace[O, F] = ev
}