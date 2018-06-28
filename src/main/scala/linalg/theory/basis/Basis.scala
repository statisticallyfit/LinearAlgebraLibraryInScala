package linalg.theory.basis

import linalg._

/**
  *
  */
trait Basis[V, W, F] /*extends VectorSpace[S, F] with VectorSpace[V, F]*/ {

     //note ifvecset cols are linearly independent, then the vecset is a basis for vecpsace V^n,
     // if not return None. which means this vecset is not a basis for the V^n vecspace.
     // prereq is isBasisOfSpaceWith function
     //TODO example 3.29 in singh uses the rows instead. versus page 246 howard
     def basis(vset: W): W

     // is vset a basis for vecspace which they occupy? only if rank(vset) == numrows(vset)
     def isBasisOfSpace(vset: W): Boolean // like isSpanned(vset)

     // is basis: B a basis for the vset?
     def isBasisOfVector(vset: W, v: V): Boolean // like isSpanned(vset, v)

     //is v in basis of vset?
     def isInBasis(v: V, vset: W): Boolean = isBasisOfVector(vset, v)

}


object Basis {
     final def apply[V, B, F](implicit ev: Basis[V, B, F]): Basis[V, B, F] = ev
}