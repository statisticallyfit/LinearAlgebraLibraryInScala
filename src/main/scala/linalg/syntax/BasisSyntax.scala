package linalg.syntax

import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */
trait BasisSyntax {

     implicit class BasisOps[V[_], B[_], N: Number](current: B[N])(implicit ev: Basis[V[N], B[N], N]){

          def basis(): B[N] = ev.basisOfSpaceSpannedBySet(current)
          def alternateBasis(): B[N] = ev.alternateBasisOfSpaceSpannedBySet(current)
          def isBasisOfSet(maybeBasis: B[N]): Boolean = ev.isBasisOfSet(current, maybeBasis)

          //def basis(): B[N] = ev.basisOfSpace(current)
          def isBasisOfSpace(): Boolean = ev.isBasisOfSpace(current)

          def isBasisOfVector(vector: V[N]): Boolean = ev.isBasisOfVector(current, vector)
          def isInBasis(vector: V[N]): Boolean = ev.isInBasis(vector, current)
     }
}
