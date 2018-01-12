package linalg.vector



import linalg.theory._



//todo: then make trait MatrixLike[M] and implement adjoint, ... all matrix-type stuff.
//make square matrix and the rest do the same. or inherit from matrix???
//todo then Independence[L] and Basis[B] traits to have functions basis() ...- the typeclass way.


trait Eq[E] {
     def equals(x: E, y: E): Boolean
}


trait VectorLike[R, V] extends Ring[V] with Eq[V] /*with HilbertSpace[ --- and banachspace*/{

     val zero: V
     val one: V

     def plus(v: V, w: V): V
     def minus(v: V, w: V): V
     def times(v: V, w: V): V  //dot product
     def crossProduct(v: V, w: V): V  //maybe won't work
     def outerProduct(v: V, w: V): V
     def scale(v: V, factor: R): V
     //no inverse, no divide!
     def negate(v: V): V

     def isZero(v: V): Boolean

     def angle(v: V, w: V): V
}


trait MatrixLike[R, M] extends VectorLike[R, M]{
     val IDENTITY: M

     def divide(m: M, p: M): M = times(m, inverse(p))

     def inverse(m: M): M
     def transpose(m: M): M
     def conjugateTranspose(m: M): M
     def adjoint(m: M): M
     def cofactor(m: M): M
     def minor(m: M): M
     def minor(m: M, rowIndex: Int, colIndex: Int): R
     def determinant(m: M): M
     def trace(m: M): R
     def rowReducedEchelonForm(m: M): M
}
//note: matrix types:
//Square, Hessenberg, LowerTri, UpperTri, Unitary,
//Orthogonal, Hermitian, Symmetric, Diagonal, Hilbert
// Similar, UpperHessenberg, Jacobian.

trait System[S] extends MatrixLike[_, S] //todo ok to pass no param?
{
     def isConsistent(s: S): Boolean
     def isInconsistent(s: S): Boolean
     def hasNoSolution(s: S): Boolean = isInconsistent(s)
     def hasUniqueSolution(s: S): Boolean = equals(rowReducedEchelonForm(s), IDENTITY)

     def infiniteSolutionSolver(s: S): S
     def solve(s: S): Option[S]


     def equals(s1: S, s2: S): Boolean
}