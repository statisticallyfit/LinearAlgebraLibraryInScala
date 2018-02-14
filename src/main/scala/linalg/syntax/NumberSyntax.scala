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
     implicit class NumberOps[N](current: N)(implicit numLike: Number[N]){

          //val n = implicitly[Number[N]]

          // Number like stuff
          def +(other: N): N = numLike.plus(current, other)
          def -(other: N): N = numLike.minus(current, other)
          def *(other: N): N = numLike.times(current, other)
          def /(other: N): N = numLike.divide(current, other)
          def negate(): N = numLike.negate(current)
          def inverse(): N = numLike.inverse(current)
          def isZero: Boolean = numLike.isZero(current)
          def isNegative: Boolean = numLike.isNegative(current)
          def toDouble: Double = numLike.doubleValue(current)
          def toInt: Int = numLike.doubleValue(current).toInt // todo check this can be chopped off!

          // ---------------------------------------------------------------------------
          private val trig: Trigonometric[N] = numLike.trig
          private val comp: Equality[N] = numLike.eq

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


     implicit class ComplexNumberOps[C[_], R: RealNumber](current: C[R])(implicit c: ComplexNumber[C[R], R]){
          private val _root = c.complexRoot
          private val _abs = c.complexAbs

          // Root stuff
          def ^(exp: R): C[R] = _root.power(current, exp)
          def sqrt(): C[R] = _root.squareRoot(current)
          def nRoot(n: R): C[R] = _root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = _abs.absoluteValue(current)
     }



     implicit class RealNumberOps[R](current: R)(implicit realNum: RealNumber[R]) {
          private val root: Root[R] = realNum.root
          private val ab: AbsoluteValue[R] = realNum.abs

          //Root stuff
          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }
}
