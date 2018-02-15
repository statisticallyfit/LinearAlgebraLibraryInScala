package linalg.syntax

import linalg.numeric._
import linalg.theory._
import linalg.vector._
import linalg.vector.VectorLike._

import cats.Eq

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import scala.language.implicitConversions



/**
  *
  */
//note: finall vec implicits worked in file Vector because I imported linalg.numeric.Number._ in file Vector! (I
// note think that is because Vector file uses Number and so does the implicit class veclikeops)

object VectorLikeSyntax {

     implicit class VecLikeOps[V[_], N: Number](current: V[N])(implicit vecLike: VectorLike[V[N], N]){

          /** Vector like */
          def -(other: V[N]): V[N] = vecLike.minus(current, other)

          def angle[R:RealNumber](other: V[N])
                                 (implicit t: Trigonometric[N],
                                  r: RootLike[N,R]): N = vecLike.angle[R](current, other)

          def crossProduct(other: V[N]): Option[V[N]] = vecLike.crossProduct(current, other)
          def outerProduct(other: V[N]): SetOfVectors[N]= vecLike.outerProduct(current, other)
          def isZero: Boolean = vecLike.isZero(current)
          def projection[R:RealNumber](onto: V[N])(implicit f: Field[N], r: RootLike[N, R]): V[N] = vecLike
               .projection[R](current, onto)

          /** Inner product space */
          def innerProduct(other: V[N]): N = vecLike.innerProduct(current, other)
          def dotProduct(other: V[N]): N = vecLike.dotProduct(current, other)

          /** Normed vector space */
          def norm[R:RealNumber]()(implicit r: RootLike[N,R]): N = vecLike.norm[R](current)
          def normalize[R:RealNumber]()(implicit r: RootLike[N,R]): V[N] = vecLike.normalize[R](current)
          def isNormalized[R:RealNumber]()(implicit eqVec: Eq[V[N]],  r: RootLike[N,R]): Boolean =
               vecLike.isNormalized[R](current)
          def distance[R:RealNumber](other: V[N])(implicit r: RootLike[N,R]): N = vecLike.distance[R](current, other)
     }
     import Number._
     import linalg.syntax.NumberSyntax._
     Vector(1,2,3).isZero
}
