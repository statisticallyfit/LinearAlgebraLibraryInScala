package linalg.syntax

//import linalg.kernel._
import linalg._
import linalg.kernel.{Complex, Real, Rational}
import linalg.theory._

import linalg.implicits._
import linalg.instances._

import scala.language.implicitConversions
import scala.language.higherKinds
/**
  *
  */
trait NumberSyntax {

     implicit class NumberOps[N: Number](current: N) {

          private val number = implicitly[Number[N]]

          def +(other: N): N = number.plus(current, other)
          def -(other: N): N = number.minus(current, other)
          def *(other: N): N = number.times(current, other)
          def /(other: N): N = number.divide(current, other)
          def negate(): N = number.negate(current)

          def inverse(): N = number.inverse(current)
          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)

          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!
     }


     //TODO all red
     implicit class RealNumberOps[R: RealNumber](current: R){
          private val root: Root[R] = implicitly[Root[R]]
          private val ab: Absolute[R] = implicitly[Absolute[R]]

          // Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }


     Complex(1,2).nRoot(2)
     Rational(2) + Rational(2)
     Rational(2) ^ Rational(2)
     Rational(2).nRoot(Rational(2))
}

