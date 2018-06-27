package linalg.theory.space

/**
  *
  */
trait ColumnSpace[C, F] extends VectorSpace[C, F] {

     def isInColumnSpace(vset: C, v: C): Boolean // true if and only if A * x = v is consistent.
     def columnSpace(vset: C): C
}

//TODO: start page 237 howard and refactor matrix definitions to use m or n depending on if
// rowspace or colspace calculation.

object ColumnSpace {
     @inline final def apply[C, F](implicit ev: ColumnSpace[C, F]): ColumnSpace[C, F] = ev
}