package linalg.syntax

import linalg.kernel._
import linalg.theory._

import scala.language.implicitConversions
import scala.language.higherKinds
/**
  *
  */
trait NumberSyntax {
     implicit class NumberOps[N: Number](current: N) {

          private val number = implicitly[Number[N]]

          def -(other: N): N = number.minus(current, other)
          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)

          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!
     }


     implicit class RealNumberOps[R: RealNumber](current: R){
          private val root: Root[R] = implicitly[Root[R]]
          private val ab: Abs[R] = implicitly[Abs[R]]

          // Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }

     import Number._ //note if we want miniature implicits tests tow ork
     import CategorySyntax._


     import RootLikeSyntax._
     import AbsoluteLikeSyntax._

     Complex(1,2).nRoot(2)
     Rational(2) + Rational(2)
     Rational(2) ^ Rational(2)
     Rational(2).nRoot(Rational(2))
}

