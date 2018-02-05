package linalg.theory.basis

import linalg.theory.space.VectorSpace

trait Orthogonal[V, F] extends VectorSpace[V, F] /*with Field[F] */{
     //this: Field[F] =>

     def isOrthogonal(v: V): Boolean
     def areOrthogonal(v1: V, v2: V): Boolean
     def orthogonalize(v: V): V
}

