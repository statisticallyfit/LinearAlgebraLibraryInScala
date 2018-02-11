package linalg.syntax

import linalg.numeric._
import linalg.vector._

import scala.language.higherKinds
import scala.language.implicitConversions



/**
  *
  */
//note: finall vec implicits worked in file Vector because I imported linalg.numeric.Number._ in file Vector! (I
// note think that is because Vector file uses Number and so does the implicit class veclikeops)

object VectorLikeSyntax {

     implicit class VecLikeOps[V[_], N: Number: Trig: Compare](current: V[N])
                                                              (implicit vecLike: VectorLike[V[N], N]){


          def +(other: V[N]): V[N] = vecLike.plus(current, other)
          def -(other: V[N]): V[N] = vecLike.minus(current, other)
          def negate(): V[N] = vecLike.negate(current)
          def scale(factor: N): V[N] = vecLike.scale(current, factor)

          //todo test vector complex norm (is the implicit Root0[N,N] correct?) + check in typeclass def.
          def norm(): N = vecLike.norm(current)
          def angle(other: V[N]): N = vecLike.angle(current, other)

          def innerProduct(other: V[N]): N = vecLike.innerProduct(current, other)
          def dotProduct(other: V[N]): N = vecLike.dotProduct(current, other)
          def crossProduct(other: V[N]): Option[SetOfVectors[N]] = vecLike.crossProduct(current, other)
          def outerProduct(other: V[N]): SetOfVectors[N]= vecLike.outerProduct(current, other)

          def isZero(): Boolean = vecLike.isZero(current)
     }
}
