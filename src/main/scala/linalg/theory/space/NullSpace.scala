package linalg.theory.space

/**
  *
  */
trait NullSpace[N, F] extends linalg.VectorSpace[N, F] {
     //todo see class to sketch out methods
}


object NullSpace {
     @inline final def apply[N, F](implicit ev: NullSpace[N, F]): NullSpace[N, F] = ev
}
