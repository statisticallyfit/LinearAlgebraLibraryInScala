package linalg.theory.basis



/**
  *
  *
  */
trait Rank[V]  {

     def rank(vectorSpace: V): Int
}

object Rank {
     @inline final def apply[V](implicit ev: Rank[V]): Rank[V] = ev
}