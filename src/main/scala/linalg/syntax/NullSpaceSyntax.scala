package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait NullSpaceSyntax {

     implicit class NullSpaceOps[V[_], U[_], N: Number](current: U[N])(implicit ev: NullSpace[V[N], U[N], N]){

          def isInNullSpace(v: V[N]): Boolean = ev.isInNullSpace(current, v)
          def isSetInNullSpace(other: U[N]): Boolean = ev.isSetInNullSpace(current, other)
          def isNullSpaceEqualTo(other: U[N]): Boolean = ev.equalNullSpaces(current, other)
          def nullSpace(): U[N] = ev.nullSpace(current)
          //TODO def areColsSpanningSpace(): Boolean = ev.areColsSpanningSpace(current)
          //TODO def areColsBasisOfSpace(): Boolean = ev.areColsBasisOfSpace(current)
          def nullity(): Int = ev.nullity(current)
     }
}
