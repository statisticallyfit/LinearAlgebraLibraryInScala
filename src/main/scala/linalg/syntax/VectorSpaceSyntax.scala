package linalg.syntax

import linalg.theory.space._
/**
  *
  */
object VectorSpaceSyntax {

     implicit class VectorSpaceOps[V, F](current: V)(implicit vecSpace: VectorSpace[V, F]){
          def +(other: V): V = vecSpace.plus(current, other)
          def negate(): V = vecSpace.negate(current)
          def scale(factor: F): V = vecSpace.scale(current, factor)
     }
}
