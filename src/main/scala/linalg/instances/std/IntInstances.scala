package linalg.instances.std

import linalg._

/**
  *
  */
//inspiration for structure:
//https://github.com/non/spire/blob/8eb8ab1ac5c27e660fcf7abd24b44b3cd3bac960/core/shared/src/main/scala/spire/std/long.scala


trait IntIsAbsolute extends Absolute[Int,Int]{

     def absoluteValue(x: Int): Int = math.abs(x)
}

trait IntIsRoot extends Root[Int, Int] {
     def power(base: Int, exp: Int): Int = math.pow(base, exp).toInt
}

trait IntHasEquality extends Equality[Int]{
     def eqv(x: Int, y: Int): Boolean = x == y
     def lessThan(x: Int, y: Int): Boolean = x < y
}

trait IntIsTrigonometric extends Trig[Int] {
     val PI: Int = 3 //just approximations! - note: int is not good for calculations

     def sin(x: Int): Int = math.sin(x).toInt
     def cos(x: Int): Int = math.cos(x).toInt
     def tan(x: Int): Int = math.tan(x).toInt
     /*def csc(x: Int): Int = (1.0 / sin(x)).toInt
     def sec(x: Int): Int = (1.0 / cos(x)).toInt
     def cot(x: Int): Int = (1.0 / tan(x)).toInt*/

     def arcsin(x: Int): Int = math.asin(x).toInt
     def arccos(x: Int): Int = math.acos(x).toInt
     def arctan(x: Int): Int = math.atan(x).toInt
     /*def arccsc(x: Int): Int = (1.0 / arcsin(x)).toInt
     def arcsec(x: Int): Int = (1.0 / arccos(x)).toInt
     def arccot(x: Int): Int = (1.0 / arctan(x)).toInt

     def theta(y: Int, x: Int): Int = math.tan(y / x).toInt*/
}

trait IntIsMonoid extends Monoid[Int]{
     val zero: Int = 0
     def plus(x: Int, y: Int): Int = x + y
}

trait IntIsAbelianGroup extends AbelianGroup[Int] with IntIsMonoid {
     def negate(x: Int): Int = -x
}

trait IntIsRing extends Ring[Int] with IntIsAbelianGroup {
     def times(x: Int, y: Int): Int = x * y
}

trait IntIsField extends Field[Int] with IntIsRing {
     val one: Int = 1
     def divide(x: Int, y: Int): Int = ((x * 1.0) / y).toInt
}


//note only putting the necessary inheritances here e.g. field,trig -
// note use traits to extend additional ones like abeliangroup, no need to repeat them here.

class IntIsRealNumber extends IntIsField
     with IntIsTrigonometric
     with IntIsAbsolute
     with IntIsRoot
     with IntHasEquality
     with RealNumber[Int] {

     def isNegative(x: Int): Boolean = x < 0
     def doubleValue(x: Int): Double = x * 1.0
     def from(x: Int): Int = x
}



trait IntInstances {

     implicit final val intIsRealNumber = new IntIsRealNumber
}
