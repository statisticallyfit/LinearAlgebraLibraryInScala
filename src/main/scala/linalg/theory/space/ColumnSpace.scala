package linalg.theory.space

/**
  *
  */
trait ColumnSpace[V, C, F] /*extends VectorSpace[C, F]*/ {

     def isInColumnSpace(vset: C, v: V): Boolean // true if and only if A * x = v is consistent.

     def isSetInColumnSpace(vset1: C, vset2: C): Boolean = equalColSpaces(vset1, vset2)

     def equalColSpaces(vset1: C, vset2: C): Boolean //true if rref(vset1) == rref(vset2) ?? //TODO

     /// get the basis of the column space of the vector set = the pivot cols of rref matched
     // to cols of vset
     def columnSpace(vset: C): C

     // TODO
     def areColsSpanningSpace(vset: C): Boolean
     def areColsBasisOfSpace(vset: C): Boolean = areColsSpanningSpace(vset)

     //----
     def columnRank(vset: C): Int
}

//TODO: start page 237 howard and refactor matrix definitions to use m or n depending on if
// rowspace or colspace calculation.

object ColumnSpace {
     @inline final def apply[V, C, F](implicit ev: ColumnSpace[V, C, F]): ColumnSpace[V, C, F] = ev
}