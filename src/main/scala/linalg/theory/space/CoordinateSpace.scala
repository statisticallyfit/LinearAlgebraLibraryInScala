package linalg.theory.space

/** todo
  *
  */
//todo Number should extend this
trait CoordinateSpace[C, F] extends linalg.VectorSpace[C, F] {

}

object CoordinateSpace {
     @inline final def apply[C, F](implicit ev: ColumnSpace[C, F]): ColumnSpace[C, F] = ev
}