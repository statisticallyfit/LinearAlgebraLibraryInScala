package linalg.theory.space

/**
  *
  */
trait ColumnSpace[C, F] extends VectorSpace[C, F] {

}

object ColumnSpace {
     @inline final def apply[C, F](implicit ev: ColumnSpace[C, F]): ColumnSpace[C, F] = ev
}