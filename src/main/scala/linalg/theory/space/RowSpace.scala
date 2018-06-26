package linalg.theory.space

/**
  *
  */
trait RowSpace[R, F] extends VectorSpace[R, F] {

     def isInRowSpace(vset: R, v: R): Boolean
     def rowSpace(vset: R): R // gets the basis of the rowspace of the vector set / matrix
}


object RowSpace {
     @inline final def apply[R, F](implicit ev: RowSpace[R, F]): RowSpace[R, F] = ev
}
