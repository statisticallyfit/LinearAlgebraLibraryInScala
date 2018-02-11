package linalg.theory.space

import linalg.theory._
/**
  *
  */

trait OrthonormalSpace[V, F] extends OrthogonalSpace[V, F] with NormedVectorSpace[V, F]  {

     def orthonormalize(v: V)(implicit f: Field[F]): V = normalize( orthogonalize(v) )
}

object OrthonormalSpace {
     final def apply[V, R](implicit ev: OrthonormalSpace[V, R]): OrthonormalSpace[V, R] = ev
}