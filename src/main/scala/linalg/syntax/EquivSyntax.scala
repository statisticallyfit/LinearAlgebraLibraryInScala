package linalg.syntax

import linalg.numeric.Equiv

/**
  *
  */
object EquivSyntax {
     implicit class EqualityOps[E: Equiv](current: E){
          private val eq = implicitly[Equiv[E]]

          def :==:(other: E): Boolean = eq.equal(current, other)
          def !==(other: E): Boolean = ! eq.equal(current, other)
          def <(other: E): Boolean = eq.lessThan(current, other)
          def >(other: E): Boolean = eq.greaterThan(current, other)
          def <=(other: E): Boolean = eq.lessThanOrEqual(current, other)
          def >=(other: E): Boolean = eq.greaterThanOrEqual(current, other)
     }
}
