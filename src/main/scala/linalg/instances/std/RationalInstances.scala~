package linalg.instances.std

import cats.Eq
import linalg._
import linalg.kernel.Rational

/**
  *
  */

private[instances] object RationalUtil {
     def makeDouble(x: Rational): Double = x.num * 1.0 / x.den
}



trait RationalIsAbsolute extends Absolute[Rational, Rational]{
     def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))
}

trait RationalIsRoot extends Root[Rational, Rational] {
     def power(base: Rational, exp: Rational): Rational =
          Rational(math.pow(RationalUtil.makeDouble(base), RationalUtil.makeDouble(exp)))
}
trait RationalHasEq extends Eq[Rational] {
     def eqv(x: Rational, y: Rational): Boolean = x.num * y.den == y.num * x.den
}

trait RationalHasEquality extends RationalHasEq with Equality[Rational]{
     def lessThan(x: Rational, y: Rational): Boolean = x.num * y.den < y.num * x.den
}

//TODO ok to extend Number ? or cyclic reference?

trait RationalIsTrig extends Trig[Rational] /*with Number[Rational]*/ {
     val E: Rational = Rational(scala.math.E)
     val PI: Rational = Rational(scala.math.Pi) // this is what spire does too, because these are finit     e.

     def sin(x: Rational): Rational = Rational(math.sin(RationalUtil.makeDouble(x)))
     def cos(x: Rational): Rational = Rational(math.cos(RationalUtil.makeDouble(x)))
     def tan(x: Rational): Rational = Rational(math.tan(RationalUtil.makeDouble(x)))

     def arcsin(x: Rational): Rational = Rational(math.asin(RationalUtil.makeDouble(x)))
     def arccos(x: Rational): Rational = Rational(math.acos(RationalUtil.makeDouble(x)))
     def arctan(x: Rational): Rational = Rational(math.atan(RationalUtil.makeDouble(x)))

}

trait RationalIsMonoid extends Monoid[Rational]{
     val zero: Rational = Rational.ZERO
     def plus(x: Rational, y: Rational): Rational = Rational(x.num*y.den + y.num*x.den, x.den*y.den)
}

trait RationalIsAbelianGroup extends AbelianGroup[Rational] with RationalIsMonoid {
     def negate(x: Rational): Rational = Rational(-x.num, x.den)
}

trait RationalIsRing extends Ring[Rational] with RationalIsAbelianGroup {
     def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
}

trait RationalIsField extends Field[Rational] with RationalIsRing {
     val one: Rational = Rational.ONE
     def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
}


//note only putting the necessary inheritances here e.g. field,trig -
// note use traits to extend additional ones like abeliangroup, no need to repeat them here.

class RationalIsRealNumber extends RationalIsField
     with RationalIsTrig
     with RationalIsAbsolute
     with RationalIsRoot
     with RationalHasEq
     with RationalHasEquality
     with RealNumber[Rational] {

     def isZero(x: Rational): Boolean = x.num == 0
     def conjugate(x: Rational): Rational = x
     def isNegative(x: Rational): Boolean = x.num < 0
     def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
     def denominator(x: Rational): Int = x.den
     def from(x: Int): Rational = Rational(x)
}



trait RationalInstances {

     //TODO must we do the below thing for all the instance traits above??

     implicit final val rationalIsRealNumber = new RationalIsRealNumber
}
