package linalg.syntax

import linalg.theory.{AbelianGroup, Field, Monoid, Ring}

/**
  *
  */
trait CategorySyntax {

     implicit class MonoidOps[M: Monoid](current: M){
          private val monoid = implicitly[Monoid[M]]

          def +(other: M): M = monoid.plus(current, other)
     }

     implicit class AbelianGroupOps[A:AbelianGroup](current: A){
          private val abelian = implicitly[AbelianGroup[A]]

          def negate(): A = abelian.negate(current)
     }

     implicit class RingOps[R: Ring](current: R){
          private val ring = implicitly[Ring[R]]

          def *(other: R): R = ring.times(current, other)
     }

     implicit class FieldOps[F: Field](current: F){
          private val field = implicitly[Field[F]]

          def /(other: F): F = field.divide(current, other)
          def inverse(): F = field.inverse(current)
     }
}
