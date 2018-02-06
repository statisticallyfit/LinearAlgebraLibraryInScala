package linalg.theory.basis


import linalg.theory.space._

/**
  *
  */

trait Dimension[V]{

     def dimension(v: V): Int
}

object Dimension {
     final def apply[V](implicit ev: Dimension[V]): Dimension[V] = ev
}
