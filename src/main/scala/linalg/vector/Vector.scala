package linalg.vector

//TODO tomorrow - no self-types, just do general approach like in Number class.

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
/*trait Eq[E] {
     def equals(x: E, y: E): Boolean
}*/


trait VectorLike[V, F] extends InnerProductSpace[V, F] with HilbertSpace[V, F] with BanachSpace[V, F] {

     //this: Number[N] =>
     //implicit def scalar: Field[F]

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

//todo - decide overall if should use extension to specify type of the 'smaller' parameters.

trait MatrixLike[M, F] extends /*Number[N] with*/ VectorLike[M, F] /*with Field[M]*/ {

     //this: Number[N] =>

     val identity: M

     def inverse(m: M): M
     def transpose(m: M): M
     def conjugateTranspose(m: M): M
     def adjoint(m: M): M
     def cofactor(m: M): M
     def minor(m: M): M
     def minor(m: M, rowIndex: Int, colIndex: Int): F
     def determinant(m: M): M
     def trace(m: M): F
     def rowReducedEchelonForm(m: M): M
}
//note: matrix types:
//Square, Hessenberg, LowerTri, UpperTri, Unitary,
//Orthogonal, Hermitian, Symmetric, Diagonal, Hilbert
// Similar, UpperHessenberg, Jacobian.


//todo ok to pass no param? instead of passing non-used N?

trait LinearSystem[S, N] extends MatrixLike[S, N] {

     //this: Number[N] =>

     def isInconsistent(s: S): Boolean
     def isConsistent(s: S): Boolean  = ! isInconsistent(s)

     def hasNoSolution(s: S): Boolean = isInconsistent(s)
     def hasUniqueSolution(s: S): Boolean = equals(rowReducedEchelonForm(s), identity)

     def infiniteSolutionSolver(s: S): S
     def solve(s: S): Option[S]
}