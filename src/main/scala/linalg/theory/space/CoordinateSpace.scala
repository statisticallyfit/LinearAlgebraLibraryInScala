package linalg.theory.space

/** todo
  *
  */
//todo Number should extend this
//todo see spire if extends innerprod space and then make number extend this!
trait CoordinateSpace[C, F] extends VectorSpace[C, F] {

}

object CoordinateSpace {
     @inline final def apply[C, F](implicit ev: ColumnSpace[C, F]): ColumnSpace[C, F] = ev
}