package linalg.vector

import linalg.kernel.{RealNumber, Root}
import linalg.theory.Field
import linalg.theory.basis.Dimension
import linalg.theory.space.{HilbertSpace, NormedVectorSpace}

/**
  *
  */

trait VectorLike[V, F] extends HilbertSpace[V, F] with NormedVectorSpace[V, F] {


     // inherited - plus, negate, scale, innerProduct, norm, angle
     def minus(v: V, w: V): V = plus(v, negate(w))
     def isZero(v: V): Boolean
     def crossProduct(v: V, w: V)(implicit d: Dimension[V]): Option[V]  //maybe won't work
     //def outerProduct(v: V, w: V): SetOfVectors[F]

     def projection[R:RealNumber](v: V, onto: V)(implicit f: Field[F], r: Root[F,R]): V
     /*def get(v: V, i: Int): F
     def set(v: V, i: Int, value: F): Unit
     def toList(v: V): List[F]
     def toBuff(v: V): ListBuffer[F]*/
}
object VectorLike {
     @inline final def apply[V, F](implicit ev: VectorLike[V, F]): VectorLike[V, F] = ev
}