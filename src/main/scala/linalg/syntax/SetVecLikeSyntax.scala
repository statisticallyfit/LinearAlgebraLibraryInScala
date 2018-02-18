package linalg.syntax

import linalg.kernel._
import linalg.vector._
import linalg.theory.basis._
/**
  *
  */
trait SetVecLikeSyntax extends VectorSpaceSyntax {

     implicit class SetVecLikeOps[S[_], N: Number](current: S[N])
                                                  (implicit ev: SetVecLike[S[N], N]){

          def rowEchelon(): S[N] = ev.rowEchelon(current)
          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)

          def minus(other: S[N]): S[N] = ev.minus(current, other)
          def isZero: Boolean = ev.isZero(current)
     }
}