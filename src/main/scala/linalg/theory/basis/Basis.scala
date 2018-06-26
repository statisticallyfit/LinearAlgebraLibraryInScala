package linalg.theory.basis

/**
  *
  */
trait Basis[V, B, F] /*extends VectorSpace[B, F] with Span[B, F]*/ {

     //note ifvecset cols are linearly independent, then the vecset is a basis for vecpsace V^n,
     // if not return None. which means this vecset is not a basis for the V^n vecspace.
     // prereq is isBasisOfSpaceWith function
     def basis(vspace: B): B

     // is vset a basis for vecspace ?
     def isBasisOfSpace(vset: B): Boolean // like isSpanned(vset)

     // is basis: B a basis for the vset?
     def isBasisOfVector(vset: B, v: V): Boolean // like isSpanned(vset, v)

     //is v in basis of vset?
     def isInBasis(v: V, vset: B): Boolean = isBasisOfVector(vset, v)

}


object Basis {
     final def apply[V, B, F](implicit ev: Basis[V, B, F]): Basis[V, B, F] = ev
}