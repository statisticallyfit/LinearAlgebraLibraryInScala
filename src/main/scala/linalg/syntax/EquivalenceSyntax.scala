package linalg.syntax

import linalg.numeric.Equivalence

/**
  *
  */
object EquivalenceSyntax {
     implicit class EqualityOps[E: Equivalence](current: E){
          private val eq = implicitly[Equivalence[E]]

          def :==:(other: E): Boolean = eq.equal(current, other)
          def !==(other: E): Boolean = ! eq.equal(current, other)
          def <(other: E): Boolean = eq.lessThan(current, other)
          def >(other: E): Boolean = eq.greaterThan(current, other)
          def <=(other: E): Boolean = eq.lessThanOrEqual(current, other)
          def >=(other: E): Boolean = eq.greaterThanOrEqual(current, other)
     }
}
