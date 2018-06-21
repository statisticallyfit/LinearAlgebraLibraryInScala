package linalg.instances.std

import linalg._

/**
  *
  */
trait DoubleIsAbsolute extends Absolute[Double,Double]{

     def absoluteValue(x: Double): Double = math.abs(x)
}

trait DoubleIsRoot extends Root[Double, Double] {
     def power(base: Double, exp: Double): Double = math.pow(base, exp)
}

trait DoubleHasEquality extends Equality[Double]{
     def eqv(x: Double, y: Double): Boolean = x == y
     def lessThan(x: Double, y: Double): Boolean = x < y
}

trait DoubleIsTrigonometric extends Trig[Double] {
     val E: Double = scala.math.E
     val PI: Double = scala.math.Pi

     def sin(x: Double): Double = math.sin(x)
     def cos(x: Double): Double = math.cos(x)
     def tan(x: Double): Double = math.tan(x)
     /*def csc(x: Double): Double = 1.0 / sin(x)
     def sec(x: Double): Double = 1.0 / cos(x)
     def cot(x: Double): Double = 1.0 / tan(x)*/

     def arcsin(x: Double): Double = math.asin(x)
     def arccos(x: Double): Double = math.acos(x)
     def arctan(x: Double): Double = math.atan(x)
     /*def arccsc(x: Double): Double = 1.0 / arcsin(x)
     def arcsec(x: Double): Double = 1.0 / arccos(x)
     def arccot(x: Double): Double = 1.0 / arctan(x)

     def theta(y: Double, x: Double): Double = math.tan(y / x)*/
}

trait DoubleIsMonoid extends Monoid[Double]{
     val zero: Double = 0.0
     def plus(x: Double, y: Double): Double = x + y
}

trait DoubleIsAbelianGroup extends AbelianGroup[Double] with DoubleIsMonoid {
     def negate(x: Double): Double = -x
}

trait DoubleIsRing extends Ring[Double] with DoubleIsAbelianGroup {
     def times(x: Double, y: Double): Double = x * y
}

trait DoubleIsField extends Field[Double] with DoubleIsRing {
     val one: Double = 1.0
     def divide(x: Double, y: Double): Double = x / y
}


//note only putting the necessary inheritances here e.g. field,trig -
// note use traits to extend additional ones like abeliangroup, no need to repeat them here.

class DoubleIsRealNumber extends DoubleIsField
     with DoubleIsTrigonometric
     with DoubleIsAbsolute
     with DoubleIsRoot
     with DoubleHasEquality
     with RealNumber[Double] {

     def isNegative(x: Double): Boolean = x < 0
     def doubleValue(x: Double): Double = x * 1.0
     def from(x: Int): Double = x
}



trait DoubleInstances {

     implicit final val doubleIsRealNumber = new DoubleIsRealNumber
}
