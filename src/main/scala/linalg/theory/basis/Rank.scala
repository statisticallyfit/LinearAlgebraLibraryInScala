package linalg.theory.basis


import linalg._

/**
  *
  *
  */
trait Rank[V, F] extends VectorSpace[V, F] {

     def rank(vectorSpace: V): Int
}

object Rank {
     @inline final def apply[V,F](implicit ev: Rank[V,F]): Rank[V,F] = ev
}