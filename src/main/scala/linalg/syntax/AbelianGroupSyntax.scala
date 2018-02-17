package linalg.syntax

import linalg.theory.AbelianGroup

/**
  *
  */

trait AbelianGroupSyntax extends MonoidSyntax {
     implicit class AbelianGroupOps[A:AbelianGroup](current: A){
          private val abelian = implicitly[AbelianGroup[A]]

          def negate(): A = abelian.negate(current)
     }
}