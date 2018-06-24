package linalg.instances.std

import linalg._
import linalg.kernel.Real


/**
  *
  */


trait RealIsAbsolute extends Absolute[Real,Real]{

     def absoluteValue(x: Real): Real = Real(math.abs(x.double))
}

trait RealIsRoot extends Root[Real, Real] {
     def power(base: Real, exp: Real): Real = Real(math.pow(base.double, exp.double))
}

trait RealHasEquality extends Equality[Real]{
     def eqv(x: Real, y: Real): Boolean = x.double == y.double
     def lessThan(x: Real, y: Real): Boolean = x.double < y.double
}

//TODO don't know if this is allowed, cyclic reference
//TODO if not, put trig in isrealnumber instance trait  OR have implicits per method like ROot currently
//TODO if it DOES, then make Root extend field the same way to get one and divide

trait RealIsTrig extends Trig[Real] /*with Field[Real]*/ {
     val E: Real = Real(scala.math.E)
     val PI: Real = Real(scala.math.Pi)

     def sin(x: Real): Real = Real(math.sin(x.double))
     def cos(x: Real): Real = Real(math.cos(x.double))
     def tan(x: Real): Real = Real(math.tan(x.double))

     def arcsin(x: Real): Real = Real(math.asin(x.double))
     def arccos(x: Real): Real = Real(math.acos(x.double))
     def arctan(x: Real): Real = Real(math.atan(x.double))

     //def theta(y: Real, x: Real): Real = Real(math.tan(y.double / x.double))
}

trait RealIsMonoid extends Monoid[Real]{
     val zero: Real = Real.ZERO
     def plus(x: Real, y: Real): Real = Real(x.double + y.double)
}

trait RealIsAbelianGroup extends AbelianGroup[Real] with RealIsMonoid {
     def negate(x: Real): Real = Real(-x.double)
}

trait RealIsRing extends Ring[Real] with RealIsAbelianGroup {
     def times(x: Real, y: Real): Real = Real(x.double * y.double)
}

trait RealIsField extends Field[Real] with RealIsRing {
     val one: Real = Real.ONE
     def divide(x: Real, y: Real): Real = Real(x.double / y.double)
}


//note only putting the necessary inheritances here e.g. field,trig -
// note use traits to extend additional ones like abeliangroup, no need to repeat them here.

class RealIsRealNumber extends RealIsField
     with RealIsTrig
     with RealIsAbsolute
     with RealIsRoot
     with RealHasEquality
     with RealNumber[Real] {

     def isZero(x: Real): Boolean = x.double == 0
     def conjugate(x: Real): Real = x
     def isNegative(x: Real): Boolean = x.double < 0
     def doubleValue(x: Real): Double = x.double
     def from(x: Int): Real = Real(x)
}



trait RealInstances {

     implicit final val realIsRealNumber = new RealIsRealNumber
}
