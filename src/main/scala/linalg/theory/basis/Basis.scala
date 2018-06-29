package linalg.theory.basis

import linalg._

/**
  *
  */
trait Basis[V, W, F] /*extends VectorSpace[S, F] with VectorSpace[V, F]*/ {

     // Gets the basis for the vector space spanned by given set of vectors
     def basisOfSpaceSpannedBySet(vset: W): W //uses page 246 howard methhod
     def alternateBasisOfSpaceSpannedBySet(vset: W): W //uses row method example 3.46 in david poole, like 3.29 singh
     def isBasisOfSet(vset: W, maybeBasis: W): Boolean //checks if one of the above bases or more

     //def basisOfSpace(vset: W): W //basis of spcae like R^n
     def isBasisOfSpace(vset: W): Boolean // is vset a basis for vecspace which they occupy? only if rank(vset) == numrows(vset)

     // is basis: B a basis for the vset?
     def isBasisOfVector(vset: W, v: V): Boolean // like isSpanned(vset, v)

     //is v in basis of vset?
     def isInBasis(v: V, vset: W): Boolean = isBasisOfVector(vset, v)

}


object Basis {
     final def apply[V, B, F](implicit ev: Basis[V, B, F]): Basis[V, B, F] = ev
}