package linalg.syntax

import linalg.numeric._
import linalg.theory._

import scala.language.implicitConversions
import scala.language.higherKinds
/**
  *
  */
object NumberSyntax {

     import Number._ //note if we want miniature implicits tests tow ork
     implicit class NumberOps[N](current: N)(implicit number: Number[N]){

          //val n = implicitly[Number[N]]

          // Number like stuff
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

          // ---------------------------------------------------------------------------
          private val trig: Trigonometric[N] = number.trig
          private val comp: Equality[N] = number.eq

          // Trig stuff
          def sin(): N = trig.sin(current)
          def cos(): N = trig.cos(current)
          def tan(): N = trig.tan(current)
          def csc(): N = trig.csc(current)
          def sec(): N = trig.sec(current)
          def cot(): N = trig.cot(current)
          def arcsin(): N = trig.arcsin(current)
          def arccos(): N = trig.arccos(current)
          def arctan(): N = trig.arctan(current)
          def arccsc(): N = trig.arccsc(current)
          def arcsec(): N = trig.arcsec(current)
          def arccot(): N = trig.arccot(current)
          def theta(x: N): N = trig.theta(current, x)

          //Compare stuff
          def :==:(other: N): Boolean = comp.equal(current, other)
          def !==(other: N): Boolean = ! comp.equal(current, other)
          def <(other: N): Boolean = comp.lessThan(current, other)
          def >(other: N): Boolean = comp.greaterThan(current, other)
          def <=(other: N): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: N): Boolean = comp.greaterThanOrEqual(current, other)

     }


     implicit class ComplexNumberOps[R: RealNumber](current: Complex[R])(implicit c: ComplexNumber[R]){
          private val _root = c.complexRoot
          private val _abs = c.complexAbs

          // Root stuff
          def ^(exp: R): Complex[R] = _root.power(current, exp)
          def sqrt(): Complex[R] = _root.squareRoot(current)
          def nRoot(n: R): Complex[R] = _root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = _abs.absoluteValue(current)
     }


     implicit class RealNumberOps[R](current: R)(implicit realNum: RealNumber[R]) {
          private val root: Root[R] = realNum.root
          private val ab: Absolute[R] = realNum.abs

          //Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }
}
