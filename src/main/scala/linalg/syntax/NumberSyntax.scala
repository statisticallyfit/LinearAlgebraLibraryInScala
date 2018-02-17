package linalg.syntax



import linalg.kernel.Number

import scala.language.implicitConversions
import scala.language.higherKinds

/**
  *
  */
//note have extending rootsyntax + absolutesyntax here even though not so in the trait Number
// note so that it works for realnumber syntax. Will it work for complex root?
//note, answer YES will  work as long as you put ALL SYNTAXES extending the AllSyntax trait!!
//(giddy)

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

//trait RealNumberSyntax
//TODO need to do realnumber syntax???

//trait RealNumberSyntax extends RootSyntax with AbsoluteSyntax