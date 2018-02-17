package linalg.syntax

import linalg.theory.Field

/**
  *
  */

trait FieldSyntax extends RingSyntax {

     implicit class FieldOps[F: Field](current: F){
          private val field = implicitly[Field[F]]

          def /(other: F): F = field.divide(current, other)
          def inverse(): F = field.inverse(current)
     }
}
