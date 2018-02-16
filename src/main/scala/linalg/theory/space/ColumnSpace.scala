package linalg.theory.space

/**
  *
  */
trait ColumnSpace[C, F] extends linalg.VectorSpace[C, F] {

}

object ColumnSpace {
     @inline final def apply[C, F](implicit ev: ColumnSpace[C, F]): ColumnSpace[C, F] = ev
}