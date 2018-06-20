package linalg.syntax

import linalg.implicits._
import linalg._
import linalg.vector.{Vector, SetOfVectors}

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait SetVecLikeSyntax extends VectorSpaceSyntax {

     implicit class SetVecLikeOps[S[_], N: Number](current: S[N])
                                                  (implicit ev: SetVecLike[S[N], N]/*,
                                                   d: Dimension[S[N]]*/){

          def rowEchelon(): S[N] = ev.rowEchelon(current)
          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)

          def minus(other: S[N]): S[N] = ev.minus(current, other)
          def isZero: Boolean = ev.isZero(current)
     }
}