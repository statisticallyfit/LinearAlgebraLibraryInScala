package linalg.theory.basis

import linalg.theory.space.NormedVectorSpace

trait Orthonormal[V, F] extends Orthogonal[V, F] with NormedVectorSpace[V, F]  {

     //this: Field[F] =>
     def orthonormalize(v: V): V = normalize( orthogonalize(v) )
}