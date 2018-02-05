package linalg.theory.space




//todo need to provide V, F or just V?
trait Dimension[V]{

     //this: VectorSpace[V, _] =>

     def dimension(v: V): Int
}