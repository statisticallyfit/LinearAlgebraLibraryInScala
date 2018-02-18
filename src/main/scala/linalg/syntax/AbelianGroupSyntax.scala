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

     //note: for layer types like vector
     implicit class AbelianGroupLayerOps[A[_], N](current: A[N])(implicit abelian: AbelianGroup[A[N]]){

          def negate(): A[N] = abelian.negate(current)
     }
}