package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait SetVecLikeSyntax extends VectorSpaceSyntax {

     implicit class SetVecLikeOps[S[_], N: Number](current: S[N])(implicit ev: SetVecLike[S[N], N]){

          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)
          def minus(other: S[N]): S[N] = ev.minus(current, other)
          def isZero: Boolean = ev.isZero(current)
     }
}