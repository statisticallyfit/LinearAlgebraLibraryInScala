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
     implicit class NumberOps[N[_], R: RealNumber](current: N[R])(implicit number: Number[N[R], R]){

          //val n = implicitly[Number[N]]

          // Number like stuff
          def +(other: N[R]): N[R] = number.plus(current, other)
          def -(other: N[R]): N[R] = number.minus(current, other)
          def *(other: N[R]): N[R] = number.times(current, other)
          def /(other: N[R]): N[R] = number.divide(current, other)
          def negate(): N[R] = number.negate(current)
          def inverse(): N[R] = number.inverse(current)
          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)
          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!

          // ---------------------------------------------------------------------------
          private val trig: Trigonometric[N[R]] = number.trig
          private val comp: Equality[N[R]] = number.eq
          private val root: RootLike[N[R], R] = number.complexRoot
          private val ab: AbsoluteLike[N[R], R] = number.complexAbs

          // Trig stuff
          def sin(): N[R] = trig.sin(current)
          def cos(): N[R] = trig.cos(current)
          def tan(): N[R] = trig.tan(current)
          def csc(): N[R] = trig.csc(current)
          def sec(): N[R] = trig.sec(current)
          def cot(): N[R] = trig.cot(current)
          def arcsin(): N[R] = trig.arcsin(current)
          def arccos(): N[R] = trig.arccos(current)
          def arctan(): N[R] = trig.arctan(current)
          def arccsc(): N[R] = trig.arccsc(current)
          def arcsec(): N[R] = trig.arcsec(current)
          def arccot(): N[R] = trig.arccot(current)
          def theta(x: N[R]): N[R] = trig.theta(current, x)

          //Compare stuff
          def :==:(other: N[R]): Boolean = comp.equal(current, other)
          def !==(other: N[R]): Boolean = ! comp.equal(current, other)
          def <(other: N[R]): Boolean = comp.lessThan(current, other)
          def >(other: N[R]): Boolean = comp.greaterThan(current, other)
          def <=(other: N[R]): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: N[R]): Boolean = comp.greaterThanOrEqual(current, other)

          // Root stuff
          def ^(exp: R): N[R] = root.power(current, exp)
          def sqrt(): N[R] = root.squareRoot(current)
          def nRoot(n: R): N[R] = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }



     /*implicit class ComplexNumberOps[R: RealNumber](current: Complex[R])(implicit c: ComplexNumber[R]){
          private val _root = c.complexRoot
          private val _abs = c.complexAbs

          // Root stuff
          def ^(exp: R): Complex[R] = _root.power(current, exp)
          def sqrt(): Complex[R] = _root.squareRoot(current)
          def nRoot(n: R): Complex[R] = _root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = _abs.absoluteValue(current)
     }*/


     implicit class RealNumberOps[R](current: R)(implicit real: RealNumber[R]) {

          // Number like stuff
          def +(other: R): R = real.plus(current, other)
          def -(other: R): R = real.minus(current, other)
          def *(other: R): R = real.times(current, other)
          def /(other: R): R = real.divide(current, other)
          def negate(): R = real.negate(current)
          def inverse(): R = real.inverse(current)
          def isZero: Boolean = real.isZero(current)
          def isNegative: Boolean = real.isNegative(current)
          def toDouble: Double = real.doubleValue(current)
          def toInt: Int = real.doubleValue(current).toInt // todo check this can be chopped off!

          // ---------------------------------------------------------------------------
          private val trig: Trigonometric[R] = real.trig
          private val comp: Equality[R] = real.eq
          private val root: Root[R] = real.root
          private val ab: Absolute[R] = real.abs

          // Trig stuff
          def sin(): R = trig.sin(current)
          def cos(): R = trig.cos(current)
          def tan(): R = trig.tan(current)
          def csc(): R = trig.csc(current)
          def sec(): R = trig.sec(current)
          def cot(): R = trig.cot(current)
          def arcsin(): R = trig.arcsin(current)
          def arccos(): R = trig.arccos(current)
          def arctan(): R = trig.arctan(current)
          def arccsc(): R = trig.arccsc(current)
          def arcsec(): R = trig.arcsec(current)
          def arccot(): R = trig.arccot(current)
          def theta(x: R): R = trig.theta(current, x)

          //Compare stuff
          def :==:(other: R): Boolean = comp.equal(current, other)
          def !==(other: R): Boolean = ! comp.equal(current, other)
          def <(other: R): Boolean = comp.lessThan(current, other)
          def >(other: R): Boolean = comp.greaterThan(current, other)
          def <=(other: R): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: R): Boolean = comp.greaterThanOrEqual(current, other)


          //Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }
     Complex(1,2).nRoot(2)
     Rational(2) + Rational(2)
     Rational(2) ^ Rational(2)
}
