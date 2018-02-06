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
     //def plus(v: V, w: V): V
     //def negate(v: V): V
     def minus(v: V, w: V): V = plus(v, negate(w))
     //def scale(v: V, factor: F): V
     // no divide, no inverse!

     def isZero(v: V): Boolean
     //def innerProduct(v: V, w: V): F
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
}


class Vector[N: Number](val elems: N*)

object Vector {

     def apply[N: Number](elems: N*): Vector[N] = new Vector(elems:_*)


     implicit def VectorIsVectorLike[N: Number: Trig] = new VectorLike[Vector[N], N] {

          import Number._ //for implicit numberops syntax


          def plus(v: Vector[N], w: Vector[N]): Vector[N] =
               Vector(v.elems.zip(w.elems).map(pair => pair._1 + pair._2):_*)

          def negate(v: Vector[N]): Vector[N] = Vector(v.elems.map(e => e.negate()):_*)

          def scale(v: Vector[N], factor: N): Vector[N] = Vector(v.elems.map(e => e * factor):_*)

          def isZero(v: Vector[N]): Boolean = v.elems.forall(e => e == Number.ZERO[N])

          def innerProduct(v: Vector[N], w: Vector[N]): N = v.elems.zip(w.elems).map(pair => pair._1 * pair._2).sum

          def outerProduct(v: Vector[N], w: Vector[N]): Vector[N] = ???

          def crossProduct(v: Vector[N], w: Vector[N]): Vector[N] = ???

          def angle(v: Vector[N], w: Vector[N]): N = innerProduct(v, w) / (norm(v) * norm(w)).arccos()

          def norm(v: Vector[N])(implicit root: Root[N,N]): N = v.elems.map(e => root.power(e, Number.TWO[N])) //todo
          // sum finish
     }
}
