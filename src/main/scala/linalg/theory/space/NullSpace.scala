package linalg.theory.space

/**
  *
  */
trait NullSpace[U, F] extends VectorSpace[U, F] {
     def isInNullSpace(vset: U): Boolean
     def nullity(vset: U): Int
}


object NullSpace {
     @inline final def apply[U, F](implicit ev: NullSpace[U, F]): NullSpace[U, F] = ev
}
