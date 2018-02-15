package linalg.instances

import linalg.kernel.{Absolute, Complex, NRoot, Number, Rational, Real, RealNumber}
import linalg.implicits._
import linalg.theory.{AbelianGroup, Field, Monoid}

/**
  *
  */

//note: cannot write e.g. "NRoot instances" trait because NRoot must go here since otherwise
// it will complain it doesn't have RealNumber for param R
//todo remove extending e.g. Number extends Equality to see if we still need Number:Trig:Eq
// in the type parameters of functions like ensureSize -- can we have separate? so that
// we can do NRootInstances and so forth? Want to put all NRoot instances in one place
//like for non-number types (Vector)


trait NumberInstances {

     implicit def ComplexIsNumber[R: RealNumber] = new Number[Complex[R]] with NRoot[Complex[R], R]
          with Absolute[Complex[R], R] {

          /** Number part */
          val zero: Complex[R] = Complex.ZERO[R]
          val one: Complex[R] = Complex.ONE[R]

          def plus(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)
          def negate(x: Complex[R]): Complex[R] = Complex(x.re.negate(), x.im.negate())
          def times(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: Complex[R], y: Complex[R]): Complex[R] = {
               val prod: Complex[R] = times(x, y)
               val absDenom: R = Complex.magnitude(y)
               Complex(prod.re / absDenom, prod.im / absDenom)
          }
          def isNegative(x: Complex[R]): Boolean = x.re.isNegative && x.im.isNegative
          def doubleValue(x: Complex[R]): Double = Complex.magnitude(x).toDouble
          def from(x: Int): Complex[R] = Complex(RealNumber[R].from(x))


          /** Root part */
          def power(base: Complex[R], exp: R): Complex[R] =
               Complex(Complex.magnitude(base) ^ exp, Complex.angle(base) * exp)

          /** Absolute part */
          def absoluteValue(z: Complex[R]): R = Complex.magnitude(z)

          /** Equality part */
          //def equal(x: Complex[R], y: Complex[R]): Boolean = x.re :==: y.re && x.im :==: y.im
          def eqv(x: Complex[R], y: Complex[R]): Boolean = x.re :==: y.re && x.im :==: y.im
          def lessThan(x: Complex[R], y: Complex[R]): Boolean = x.re < y.re || (x.re :==: y.re && x.im < y.im)


          /** Trig part */
          val E: Complex[R] = ??? //Complex(scala.math.E).asInstanceOf[Complex[R]] //todo more graceful way?
          val PI: Complex[R] = ??? //Complex(scala.math.Pi).asInstanceOf[Complex[R]]

          //todo major todo
          def sin(x: Complex[R]): Complex[R] = ???
          def cos(x: Complex[R]): Complex[R] = ???
          def tan(x: Complex[R]): Complex[R] = ???
          def csc(x: Complex[R]): Complex[R] = ???
          def sec(x: Complex[R]): Complex[R] = ???
          def cot(x: Complex[R]): Complex[R] = ???

          def arcsin(x: Complex[R]): Complex[R] = ???
          def arccos(x: Complex[R]): Complex[R] = ???
          def arctan(x: Complex[R]): Complex[R] = ???
          def arccsc(x: Complex[R]): Complex[R] = ???
          def arcsec(x: Complex[R]): Complex[R] = ???
          def arccot(x: Complex[R]): Complex[R] = ???

          def theta(y: Complex[R], x: Complex[R]): Complex[R] = tan(divide(y, x))
     }



     implicit object RealIsNumber extends RealNumber[Real] /*with Trigonometric[Real] with Absolute[Real]
     with Root[Real] with Equality[Real]*/ {

          val zero: Real = Real.ZERO
          val one: Real = Real.ONE


          def plus(x: Real, y: Real): Real = Real(x.double + y.double)
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)
          def negate(x: Real): Real = Real(-x.double)
          def isNegative(x: Real): Boolean = x.double < 0
          def doubleValue(x: Real): Double = x.double
          def from(x: Int): Real = Real(x)

          /** Root part */
          val rOne: Real = one
          val rTwo: Real = two
          def power(base: Real, exp: Real): Real = Real(math.pow(base.double, exp.double))

          /** Absolute value part */
          def absoluteValue(x: Real): Real = Real(math.abs(x.double))

          /** Equality part */
          def eqv(x: Real, y: Real): Boolean = x.double == y.double
          def lessThan(x: Real, y: Real): Boolean = x.double < y.double

          /** Trig part **/
          val E: Real = Real(scala.math.E)
          val PI: Real = Real(scala.math.Pi)

          def sin(x: Real): Real = Real(math.sin(x.double))
          def cos(x: Real): Real = Real(math.cos(x.double))
          def tan(x: Real): Real = Real(math.tan(x.double))
          def csc(x: Real): Real = divide(Real.ONE, sin(x))
          def sec(x: Real): Real = divide(Real.ONE, cos(x))
          def cot(x: Real): Real = divide(Real.ONE, tan(x))

          def arcsin(x: Real): Real = Real(math.asin(x.double))
          def arccos(x: Real): Real = Real(math.acos(x.double))
          def arctan(x: Real): Real = Real(math.atan(x.double))
          def arccsc(x: Real): Real = divide(Real.ONE, arcsin(x))
          def arcsec(x: Real): Real = divide(Real.ONE, arccos(x))
          def arccot(x: Real): Real = divide(Real.ONE, arctan(x))

          def theta(y: Real, x: Real): Real = Real(math.tan(y.double / x.double))
     }


     implicit object RationalIsRealNumber extends RealNumber[Rational] /*with Trigonometric[Rational]
     with Absolute[Rational] with Root[Rational] with Equality[Rational]*/ {

          /** Real part */
          val zero: Rational = Rational.ONE
          val one: Rational = Rational.ONE

          def plus(x: Rational, y: Rational): Rational = Rational(x.num*y.den + y.num*x.den, x.den*y.den)
          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
          def negate(x: Rational): Rational = Rational(-x.num, -x.den)
          def isNegative(x: Rational): Boolean = x.num < 0
          def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
          def from(x: Int): Rational = Rational(x)


          /** Root part */
          val rOne: Rational = one
          val rTwo: Rational = two
          def power(base: Rational, exp: Rational): Rational = Rational(math.pow(doubleValue(base), doubleValue(exp)))

          /** Absolute value part */
          def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))

          /** Equality part */
          def eqv(x: Rational, y: Rational): Boolean = x.num * y.den == y.num * x.den
          def lessThan(x: Rational, y: Rational): Boolean = x.num * y.den < y.num * x.den

          /** Trig part **/
          val E: Rational = Rational(scala.math.E)
          val PI: Rational = Rational(scala.math.Pi) // this is what spire does too, because these are finit     e.

          def sin(x: Rational): Rational = Rational(math.sin(doubleValue(x)))
          def cos(x: Rational): Rational = Rational(math.cos(doubleValue(x)))
          def tan(x: Rational): Rational = Rational(math.tan(doubleValue(x)))
          def csc(x: Rational): Rational = divide(Rational.ONE, sin(x))
          def sec(x: Rational): Rational = divide(Rational.ONE, cos(x))
          def cot(x: Rational): Rational = divide(Rational.ONE, tan(x))

          def arcsin(x: Rational): Rational = Rational(math.asin(doubleValue(x)))
          def arccos(x: Rational): Rational = Rational(math.acos(doubleValue(x)))
          def arctan(x: Rational): Rational = Rational(math.atan(doubleValue(x)))
          def arccsc(x: Rational): Rational = divide(Rational.ONE, arcsin(x))
          def arcsec(x: Rational): Rational = divide(Rational.ONE, arccos(x))
          def arccot(x: Rational): Rational = divide(Rational.ONE, arctan(x))

          //todo isn't it supposed to be arctan?
          def theta(y: Rational, x: Rational): Rational = Rational(math.tan(doubleValue(x) / doubleValue(x)))
     }


     implicit object IntIsRealNumber extends RealNumber[Int] /*with Trigonometric[Int] with Absolute[Int]
     with Root[Int] with Equality[Int]*/{

          val zero: Int = 0
          val one: Int = 1

          def plus(x: Int, y: Int): Int = x + y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y
          def negate(x: Int): Int = -x
          def isNegative(x: Int): Boolean = x < 0
          def doubleValue(x: Int): Double = x * 1.0
          def from(x: Int): Int = x

          /** Root part */
          val rOne: Int = 1
          val rTwo: Int = 2
          def power(base: Int, exp: Int): Int = math.pow(base, exp).toInt //not chopped off

          /** Absolute value part */
          def absoluteValue(x: Int): Int = math.abs(x)

          /** Equality part */
          def eqv(x: Int, y: Int): Boolean = x == y
          def lessThan(x: Int, y: Int): Boolean = x < y

          /** Trig part **/
          val E: Int = 2
          val PI: Int = 3 //just approximations! - note: int is not good for calculations

          def sin(x: Int): Int = math.sin(x).toInt
          def cos(x: Int): Int = math.cos(x).toInt
          def tan(x: Int): Int = math.tan(x).toInt
          def csc(x: Int): Int = (1.0 / sin(x)).toInt
          def sec(x: Int): Int = (1.0 / cos(x)).toInt
          def cot(x: Int): Int = (1.0 / tan(x)).toInt

          def arcsin(x: Int): Int = math.asin(x).toInt
          def arccos(x: Int): Int = math.acos(x).toInt
          def arctan(x: Int): Int = math.atan(x).toInt
          def arccsc(x: Int): Int = (1.0 / arcsin(x)).toInt
          def arcsec(x: Int): Int = (1.0 / arccos(x)).toInt
          def arccot(x: Int): Int = (1.0 / arctan(x)).toInt

          def theta(y: Int, x: Int): Int = math.tan(y / x).toInt
     }


     implicit object DoubleIsRealNumber extends RealNumber[Double] /*with Trigonometric[Double] with Absolute[Double]
     with Root[Double] with Equality[Double]*/{

          val zero: Double = 0.0
          val one: Double = 1.0

          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y
          def negate(x: Double): Double = -x
          def isNegative(x: Double): Boolean = x < 0
          def doubleValue(x: Double): Double = x
          def from(x: Int): Double = x * 1.0

          /** Root part */
          val rOne: Double = 1.0
          val rTwo: Double = 2.0
          def power(base: Double, exp: Double): Double = math.pow(base, exp)

          /** Absolute value part */
          def absoluteValue(x: Double): Double = math.abs(x)

          /** Equality part */
          def eqv(x: Double, y: Double): Boolean = x == y
          def lessThan(x: Double, y: Double): Boolean = x < y


          /** Trig part **/
          val E: Double = scala.math.E
          val PI: Double = scala.math.Pi

          def sin(x: Double): Double = math.sin(x)
          def cos(x: Double): Double = math.cos(x)
          def tan(x: Double): Double = math.tan(x)
          def csc(x: Double): Double = 1.0 / sin(x)
          def sec(x: Double): Double = 1.0 / cos(x)
          def cot(x: Double): Double = 1.0 / tan(x)

          def arcsin(x: Double): Double = math.asin(x)
          def arccos(x: Double): Double = math.acos(x)
          def arctan(x: Double): Double = math.atan(x)
          def arccsc(x: Double): Double = 1.0 / arcsin(x)
          def arcsec(x: Double): Double = 1.0 / arccos(x)
          def arccot(x: Double): Double = 1.0 / arctan(x)

          def theta(y: Double, x: Double): Double = math.tan(y / x)
     }

}
