package linalg.syntax

import cats.Eq

import linalg.numeric._
import linalg.numeric.Number._
import linalg.vector._
import linalg.vector.VectorLike._
import linalg.theory.basis._

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */


object SetVecLikeSyntax {

     implicit class SetVecLikeOps[S[_],
          N: Number: Trigonometric: Root: Absolute: Equality](current: S[N])(implicit ev: SetVecLike[S[N], N], dim:
     Dimension[S[N]]){

          def rowEchelon(): S[N] = ev.rowEchelon(current)
          def rowReducedEchelon(): S[N] = ev.rowReducedEchelon(current)
          def dimension(): Int = dim.dimension(current)

          def minus(other: S[N]): S[N] = ev.minus(current, other)
          def isZero: Boolean = ev.isZero(current)
     }

}
