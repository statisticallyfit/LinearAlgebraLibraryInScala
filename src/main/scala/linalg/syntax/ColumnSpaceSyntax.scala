package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait ColumnSpaceSyntax {

     implicit class ColSpaceOps[V[_], C[_], N: Number](current: C[N])(implicit ev: ColumnSpace[V[N], C[N], N]){

          def isInColumnSpace(v: V[N]): Boolean = ev.isInColumnSpace(current, v)
          def isColumnSpaceEqualTo(other: C[N]): Boolean = ev.equalColSpaces(current, other)
          def isSetInColumnSpace(other: C[N]): Boolean = ev.isSetInColumnSpace(current, other)
          def columnSpace(): C[N] = ev.columnSpace(current)
          def areColsSpanningSpace(): Boolean = ev.areColsSpanningSpace(current)
          def areColsBasisOfSpace(): Boolean = ev.areColsBasisOfSpace(current)
          def columnRank(): Int = ev.columnRank(current)
     }
}

