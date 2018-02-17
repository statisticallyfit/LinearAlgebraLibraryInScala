package linalg.syntax



import linalg.kernel.Number

import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */
trait NumberSyntax extends FieldSyntax with TrigSyntax with EqualitySyntax
     with RootSyntax with AbsoluteSyntax {


     implicit class NumberOps[N: Number](current: N) {

          private val number = implicitly[Number[N]]

          def -(other: N): N = number.minus(current, other)
          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)
          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!
     }
}

//TODO need to do realnumber syntax???

//trait RealNumberSyntax extends RootSyntax with AbsoluteSyntax