package linalg.theory.space

/**
  *
  */
//TODO how to express tha tboth V, W are vecspaces? cannot inherit twice ...

trait RowSpace[V, R, F] /*extends VectorSpace[W[F], F] with VectorSpace[V[F], F]*/ {

     def isInRowSpace(vset: R, v: V): Boolean // true if and only if A.transpose * x = v.transpose
     // is a consistent system

     def equalRowSpaces(vset1: R, vset2: R): Boolean // true if rref(vset1) == rref(vset2)
     // means they are the same matrices, same row ops to get to rref form.

     //TODO does this mean that vset2 is in rowspace of vset1?
     def isSetInRowSpace(vset1: R, vset2: R): Boolean = equalRowSpaces(vset1, vset2)

     def rowSpace(vset: R): R // gets the basis of the rowspace of the vector set / matrix
     //nonzero rows of rref(vset) form a basis for rowspace of vset.

     //do rows span the space R^n? only if rref(vset) = IDENTITY (nxn)
     def areRowsSpanningSpace(vset: R): Boolean // true if and only if rref(vset) == identity(vset) with nxn
     def areRowsBasisOfSpace(vset: R): Boolean = areRowsSpanningSpace(vset)


     def rank(vset: R): Int
     def isFullRank(vset: R): Boolean
}


object RowSpace {
     @inline final def apply[V, R, F](implicit ev: RowSpace[V, R, F]): RowSpace[V, R, F] = ev
}
