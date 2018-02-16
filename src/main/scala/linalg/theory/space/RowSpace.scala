package linalg.theory.space

/**
  *
  */
trait RowSpace[R, F] extends linalg.VectorSpace[R, F] {
//todo see class to sketch out methods
}


object RowSpace {
     @inline final def apply[R, F](implicit ev: RowSpace[R, F]): RowSpace[R, F] = ev
}
