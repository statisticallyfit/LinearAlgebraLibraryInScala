package linalg.theory.space

import linalg.numeric.{RealNumber, RootLike}
import linalg.theory._
/**
  *
  */

trait OrthonormalSpace[V, F] extends OrthogonalSpace[V, F] with NormedVectorSpace[V, F]  {

     def orthonormalize[R:RealNumber](v: V)(implicit f: Field[F], r: RootLike[F,R]): V =
          normalize[R]( orthogonalize(v) )
}

object OrthonormalSpace {
     final def apply[V, R](implicit ev: OrthonormalSpace[V, R]): OrthonormalSpace[V, R] = ev
}