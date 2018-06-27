package linalg.theory.space

/**
  *
  */
trait RowSpace[R, F] extends VectorSpace[R, F] {

     def isInRowSpace(vset: R, v: R): Boolean // true if and only if A.transpose * x = v.transpose
     // is a consistent system
     def haveEqualRowSpaces(vset1: R, vset2: R): Boolean // true if rref(vset1) == rref(vset2)
     // means they are the same matrices, same row ops to get to rref form.

     def rowSpace(vset: R): R // gets the basis of the rowspace of the vector set / matrix
     //nonzero rows of rref(vset) form a basis for rowspace of vset.

     //do rows span the space R^n? only if rref(vset) = IDENTITY (nxn)
     def doRowsSpanSpace(vset: R): Boolean // true if and only if rref(vset) == identity(vset) with nxn
     def doRowsFormBasisOfSpace(vset: R): Boolean = doRowsSpanSpace(vset)
}


object RowSpace {
     @inline final def apply[R, F](implicit ev: RowSpace[R, F]): RowSpace[R, F] = ev
}
