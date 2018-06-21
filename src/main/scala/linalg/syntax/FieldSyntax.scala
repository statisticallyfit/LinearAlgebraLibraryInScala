package linalg.syntax

import linalg.implicits._
import linalg._

import scala.language.higherKinds
import scala.language.implicitConversions

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
