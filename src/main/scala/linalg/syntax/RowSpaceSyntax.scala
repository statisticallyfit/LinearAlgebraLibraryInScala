package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait RowSpaceSyntax {

     implicit class RowSpaceOps[V[_], R[_], N: Number](current: R[N])(implicit ev: RowSpace[V[N], R[N], N]){

          def isInRowSpace(v: V[N]): Boolean = ev.isInRowSpace(current, v)
          def isRowSpaceEqualTo(other: R[N]): Boolean = ev.equalRowSpaces(current, other)
          def isInRowSpace(other: R[N]): Boolean = ev.isSetInRowSpace(current, other)
          def rowSpace(): R[N] = ev.rowSpace(current)
          def areRowsSpanningSpace(): Boolean = ev.areRowsSpanningSpace(current)
          def areRowsBasisOfSpace(): Boolean = ev.areRowsBasisOfSpace(current)
          def rank(): Int = ev.rank(current)
          def isFullRank(): Boolean = ev.isFullRank(current)
     }
}

