package linalg.theory


import linalg.numeric._



trait Rank[V, F] extends VectorSpace[V, F] {

     this: Field[F] => //todo - ok?

     def rank(vecSpace: V): Int
}