package linalg.syntax

import linalg.numeric._
import linalg.theory._

import scala.language.implicitConversions
import scala.language.higherKinds
/**
  *
  */
object NumberSyntax {

     implicit class NumberOps[N](current: N)(implicit number: Number[N])  {

          //private val number = implicitly[Number[N]]

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


          private val comp: Compare[N] = number.numberIsComparable

          def :==:(other: N): Boolean = comp.equal(current, other)
          def !==(other: N): Boolean = ! comp.equal(current, other)
          def <(other: N): Boolean = comp.lessThan(current, other)
          def >(other: N): Boolean = comp.greaterThan(current, other)
          def <=(other: N): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: N): Boolean = comp.greaterThanOrEqual(current, other)

     }

     implicit class NumberComplexOps[N](current: Number[N])(implicit number: Number[N]){

          private  val root: Root0[Number[N], N] = number.numberHasRoot

          def ^(exp: N): Number[N] = root.power(current, exp)
          def sqrt(): Number[N] = root.squareRoot(current)
          def nRoot(n: N): Number[N] = root.nRoot(current, n)


          private val ab: Absolute0[Number[N], N] = number.numberHasAbsoluteValue

          def abs(): N = ab.absoluteValue(current)
     }


     implicit class RealLikeOps[R](current: R)(implicit realLike: RealLike[R]){

          private val root: Root[R] = realLike.realNumberHasRoot

          def ^(exp: R): R = root.power(current, exp)
          def sqrt(): R = root.squareRoot(current)
          def nRoot(n: R): R = root.nRoot(current, n)


          private val ab: Absolute[R] = realLike.realNumberHasAbsoluteValue
          def abs(): R = ab.absoluteValue(current)


          private val trig: Trig[R] = realLike.realNumberIsTrigonometric

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
     }

     /*implicit class NumberComplexOps[H[_], L: Number](current: Number[L])(implicit numH: Number[H[L]], numL: Number[L]){

          private  val root: Root0[Number[L], L] = numL.numberHasRoot

          def ^(exp: L): Number[L] = root.power(current, exp)
          def sqrt(): Number[L] = root.squareRoot(current)
          def nRoot(n: L): Number[L] = root.nRoot(current, n)
     }*/

     /*implicit class RootOps[R: Root: Field](base: R)(implicit root: Root[R]){

          def ^(exp: R): R = root.power(base, exp)
          def sqrt(): R = root.squareRoot(base)
          def nRoot(n: R): R = root.nRoot(base, n)
     }*/

     /*implicit class RootBaseOps[N: Number](base: Number[N])(implicit root: Root0[Number[N], N]){

          def ^(exp: N): Number[N] = root.power(base, exp)
          def sqrt(): Number[N] = root.squareRoot(base)
          def nRoot(n: N): Number[N] = root.nRoot(base, n)
     }*/
     /*implicit class RootBaseOps[H[_], L: Field](base: H[L])(implicit root: Root0[H[L], L],
                                                            numH: Number[H[L]],
                                                            numL: Number[L]){

          def ^(exp: L): H[L] = root.power(base, exp)
          def sqrt(): H[L] = root.squareRoot(base)
          def nRoot(n: L): H[L] = root.nRoot(base, n)
     }*/
     Complex(1,2).nRoot(2)

}
