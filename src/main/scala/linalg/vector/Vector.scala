package linalg.vector

//TODO tomorrow - no self-types, just do general approach like in Number class.

import cats.Eq
import linalg.theory.space._
import linalg.theory._
import linalg.numeric._



//todo: then make trait MatrixLike[M] and implement adjoint, ... all matrix-type stuff.
//make square matrix and the rest do the same. or inherit from matrix???
//todo then Independence[L] and Basis[B] traits to have functions basis() ...- the typeclass way.

/**
  * Features:
  *
  * //note:
  * - if there is a has-a relation, we use typeclass implementation.
  * If there is is-a relation use trait extension. Example: Vector class HAS-A Basis so declare the
  * Basis[Vector[N] ] implementation bu Vectr IS-A Hilbert Space so extend the Hilbert Space in VectorLike trait.
  *
  * -
  */
//todo use Cats Eq typeclass instead!


trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with BanachSpace[V, F] {

     //note: inherited
     def plus(v: V, w: V): V
     def negate(v: V): V
     def minus(v: V, w: V): V = plus(v, negate(w))
     def scale(v: V, factor: F): V
     // no divide, no inverse!

     def isZero(v: V): Boolean
     def innerProduct(v: V, w: V): F
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V

     def angle(v: V, w: V): V
}


class Vector[F: Field](elems: F*)

object Vector {

}