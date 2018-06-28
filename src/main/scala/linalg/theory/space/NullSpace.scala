package linalg.theory.space

/**
  *
  */
trait NullSpace[V, N, F] /*extends VectorSpace[N, F]*/ {

     def isInNullSpace(vset: N, v: V): Boolean // true if A * v = 0 vector

     def isSetInNullSpace(vset1: N, vset2: N): Boolean = equalNullSpaces(vset1, vset2)

     //TODO //true if rref(vset1) == rref(vset2) ??
     def equalNullSpaces(vset1: N, vset2: N): Boolean


     def nullSpace(vset: N): N // the basis is the solution to Ax = 0
     // so write Augmented(vset, zerovec(numcols of vset)).solve()

     // TODO --- spansNullspace() ? versus spansSpace()?? --- ask of rowspace and colspaces too
     ///def areColsSpanningSpace(vset: N): Boolean
     //def areColsBasisOfSpace(vset: N): Boolean = areColsSpanningSpace(vset)

     // ---------
     def nullity(vset: N): Int
}


object NullSpace {
     @inline final def apply[V, N, F](implicit ev: NullSpace[V, N, F]): NullSpace[V, N, F] = ev
}
