package linalg.syntax

import linalg.numeric._
import linalg.vector._

/**
  *
  */
object VectorLikeSyntax {


     implicit class VectorLikeOps[N: Number: Trig, V](current: V)(implicit root: Root[N,N],
                                                                  vectorLike: VectorLike[V, N]){

          def +(other: V): V = vectorLike.plus(current, other)
          def -(other: V): V = vectorLike.minus(current, other)
          def negate(): V = vectorLike.negate(current)
          def scale(factor: N): V = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other: V): N = vectorLike.angle(current, other)
          def innerProduct(other: V): N = vectorLike.innerProduct(current, other)
          def dotProduct(other: V): N = vectorLike.dotProduct(current, other)
          def crossProduct(other: V): V = vectorLike.crossProduct(current, other)

          def isZero(): Boolean = vectorLike.isZero(current)
     }
}
