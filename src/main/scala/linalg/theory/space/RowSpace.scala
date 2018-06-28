package linalg.theory.space

/**
  *
  */
trait RowSpace[V, W, F] /*extends VectorSpace[W[F], F] with VectorSpace[V[F], F]*/ {

     def isInRowSpace(vset: W, v: V): Boolean // true if and only if A.transpose * x = v.transpose
     // is a consistent system

     def equalRowSpaces(vset1: W, vset2: W): Boolean // true if rref(vset1) == rref(vset2)
     // means they are the same matrices, same row ops to get to rref form.

     //TODO does this mean that vset2 is in rowspace of vset1?
     def isSetInRowSpace(vset1: W, vset2: W): Boolean = equalRowSpaces(vset1, vset2)

     def rowSpace(vset: W): W // gets the basis of the rowspace of the vector set / matrix
     //nonzero rows of rref(vset) form a basis for rowspace of vset.

     //do rows span the space R^n? only if rref(vset) = IDENTITY (nxn)
     def areRowsSpanningSpace(vset: W): Boolean // true if and only if rref(vset) == identity(vset) with nxn
     def areRowsBasisOfSpace(vset: W): Boolean = areRowsSpanningSpace(vset)
}


object RowSpace {
     @inline final def apply[V, W, F](implicit ev: RowSpace[V, W, F]): RowSpace[V, W, F] = ev
}
