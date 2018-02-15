package linalg.theory.space

import linalg.kernel.{RealNumber, NRoot}
import linalg.theory._
/**
  *
  */

trait OrthonormalSpace[O, F] extends OrthogonalSpace[O, F] with NormedVectorSpace[O, F]  {

     def orthonormalize[R:RealNumber](v: O)(implicit f: Field[F], r: NRoot[F,R]): O =
          normalize[R]( orthogonalize(v) )
}

object OrthonormalSpace {
     @inline final def apply[O, F](implicit ev: OrthonormalSpace[O, F]): OrthonormalSpace[O, F] = ev
}