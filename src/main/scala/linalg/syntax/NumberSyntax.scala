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


     implicit class TrigOps[T: Trigonometric](current: T){
          // ---------------------------------------------------------------------------
          private val trig: Trigonometric[T] = implicitly[Trigonometric[T]]

          // Trig stuff
          def sin(): T = trig.sin(current)
          def cos(): T = trig.cos(current)
          def tan(): T = trig.tan(current)
          def csc(): T = trig.csc(current)
          def sec(): T = trig.sec(current)
          def cot(): T = trig.cot(current)
          def arcsin(): T = trig.arcsin(current)
          def arccos(): T = trig.arccos(current)
          def arctan(): T = trig.arctan(current)
          def arccsc(): T = trig.arccsc(current)
          def arcsec(): T = trig.arcsec(current)
          def arccot(): T = trig.arccot(current)
          def theta(x: T): T = trig.theta(current, x)
     }

     implicit class EqualityOps[E: Equality](current: E){

          private val comp: Equality[E] = implicitly[Equality[E]]

          //Compare stuff
          def :==:(other: E): Boolean = comp.equal(current, other)
          def !==(other: E): Boolean = ! comp.equal(current, other)
          def <(other: E): Boolean = comp.lessThan(current, other)
          def >(other: E): Boolean = comp.greaterThan(current, other)
          def <=(other: E): Boolean = comp.lessThanOrEqual(current, other)
          def >=(other: E): Boolean = comp.greaterThanOrEqual(current, other)

     }

     implicit class RootLikeLAYEROps[N[_], R: RealNumber](current: N[R])(implicit root: RootLike[N[R], R]){

          // Root stuff
          def ^(exp: R): N[R] = root.power(current, exp)
          def sqrt(): N[R] = root.squareRoot(current)
          def nRoot(n: R): N[R] = root.nRoot(current, n)
     }

     implicit class AbsoluteLikeLAYEROps[N[_], R: RealNumber](current: N[R])
                                                             (implicit ab: AbsoluteLike[N[R], R]){

          // Absolute stuff
          def abs(): R = ab.absoluteValue(current)
     }

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
