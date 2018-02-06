package linalg.syntax

import linalg.numeric.{Number, Root, Trig}
import linalg.vector.VectorLike

/**
  *
  */
object VectorLikeSyntax {
     implicit class VectorLikeOps[V, N: Number: Trig](current: V)(implicit root: Root[N,N],
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
          /*def +(other: V[N]): V[N] = vectorLike.plus(current, other)
          def -(other: V[N]): V[N] = vectorLike.minus(current, other)
          def negate(): V[N] = vectorLike.negate(current)
          def scale(factor: N): V[N] = vectorLike.scale(current, factor)

          def norm(): N = vectorLike.norm(current)
          def angle(other: V[N]): N = vectorLike.angle(current, other)
          def innerProduct(other: V[N]): N = vectorLike.innerProduct(current, other)
          def dotProduct(other: V[N]): N = vectorLike.dotProduct(current, other)
          def crossProduct(other: V[N]): V[N] = vectorLike.crossProduct(current, other)

          def isZero(): Boolean = vectorLike.isZero(current)*/
          //---
          /* def +(other: Vector[N]): Vector[N] = vectorLike.plus(current, other)
           def -(other: Vector[N]): Vector[N] = vectorLike.minus(current, other)
           def negate(): Vector[N] = vectorLike.negate(current)
           def scale(factor: N): Vector[N] = vectorLike.scale(current, factor)

           def norm(): N = vectorLike.norm(current)
           def angle(other: Vector[N]): N = vectorLike.angle(current, other)
           def innerProduct(other: Vector[N]): N = vectorLike.innerProduct(current, other)
           //def dotProduct(other: Vector[N]): N = vectorLike.dotProduct(current, other)
           def crossProduct(other: Vector[N]): Vector[N] = vectorLike.crossProduct(current, other)

           def isZero(): Boolean = vectorLike.isZero(current)*/
     }
}
