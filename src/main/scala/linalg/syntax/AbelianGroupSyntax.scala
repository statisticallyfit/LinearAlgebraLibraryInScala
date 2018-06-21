package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions
/**
  *
  */

trait AbelianGroupSyntax extends MonoidSyntax {
     implicit class AbelianGroupOps[A:AbelianGroup](current: A){
          private val abelian = implicitly[AbelianGroup[A]]

          def negate(): A = abelian.negate(current)
     }

     //note: for layer types like vector[N] and complex[R]
     implicit class AbelianGroupLayerOps[A[_], N](current: A[N])(implicit abelian: AbelianGroup[A[N]]){

          def negate(): A[N] = abelian.negate(current)
     }
}