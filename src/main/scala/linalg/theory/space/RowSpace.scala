package linalg.theory.space

/**
  *
  */
trait RowSpace[R, F] extends VectorSpace[R, F] {

     def isInRowSpace(vset: R, v: R): Boolean
     def rowSpaceBasis(vset: R): R
}


object RowSpace {
     @inline final def apply[R, F](implicit ev: RowSpace[R, F]): RowSpace[R, F] = ev
}
