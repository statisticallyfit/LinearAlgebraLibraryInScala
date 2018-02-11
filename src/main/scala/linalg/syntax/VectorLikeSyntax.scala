package linalg.syntax

import linalg.numeric._
import linalg.vector._
import linalg.util.Util.SizeChecker

import cats.Eq

import scala.language.higherKinds
import scala.language.implicitConversions



/**
  *
  */
//note: finall vec implicits worked in file Vector because I imported linalg.numeric.Number._ in file Vector! (I
// note think that is because Vector file uses Number and so does the implicit class veclikeops)

object VectorLikeSyntax {

     implicit class VecLikeOps[V[_], N: Number: Trig: Root: Absolute: Compare](current: V[N])
                                                              (implicit vecLike: VectorLike[V[N], N],
                                                               sz: SizeChecker[V[N]]){

          /** Vector like */
          def -(other: V[N]): V[N] = vecLike.minus(current, other)
          def angle(other: V[N]): N = vecLike.angle(current, other)
          def crossProduct(other: V[N]): Option[V[N]] = vecLike.crossProduct(current, other)
          def outerProduct(other: V[N]): SetOfVectors[N]= vecLike.outerProduct(current, other)
          def isZero(): Boolean = vecLike.isZero(current)
          def get(i: Int): N = vecLike.get(current, i)

          /** Inner product space */
          def innerProduct(other: V[N]): N = vecLike.innerProduct(current, other)
          def dotProduct(other: V[N]): N = vecLike.dotProduct(current, other)

          /** Normed vector space */
          def norm(): N = vecLike.norm(current)
          def normalize(): V[N] = vecLike.normalize(current)
          def isNormalized()(implicit eqVec: Eq[V[N]]): Boolean = vecLike.isNormalized(current)
          def distance(other: V[N]): N = vecLike.distance(current, other)
     }
}
