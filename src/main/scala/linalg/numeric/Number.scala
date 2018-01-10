package linalg.numeric


import linalg.theory._
import linalg.util.ImplicitConversions._
import org.apache.commons.lang3.math.Fraction

import scala.language.implicitConversions


/**
  * todo: make trait Trig, and mix in appropriately (decide if need it - complex: if must stay angle as N or as Double)
  * todo make class Polar to interact with Complex.
  *
  * todo: move Complex theta, polar ... functionality in same place with the main +, -, / code ...
  *
  * todo: interoperability: Complex + Real + Rational + Int + Double --- towrk
  * todo: interoperability: 4 + 3*i notation should work.
  *
  *
  *
  *
  * note: Source for complex .i accessor:
  * https://stackoverflow.com/questions/17381896/scala-simple-notation-of-imaginary-number
  */


trait Number[N] extends Field[N] {

     //inherited: add, multiply, divide, one, zero, negate, inverse
     def one: N
     def zero: N

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N = plus(x, negate(y))
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
     def power(x: N, y: N): N
     def squareRoot(x: N): N
     def absoluteValue(x: N): N

     def negate(x: N): N
     def inverse(x: N): N

     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean
     def areEqual(x: N, y: N): Boolean

     def doubleValue(x: N): Double
}

trait RealNumber[R] extends Number[R]

trait Show[S] {

     def show(x: S): String
}
object Show {
     implicit class ShowOps[S: Show](current: S) {
          val ev = implicitly[Show[S]]

          def show: String = ev.show(current)
     }
}
import Show._


object Number {

     implicit class NumberOps[N : Number](current: N) extends Ordered[N]  {
          val ev = implicitly[Number[N]]

          def +(other: N): N = ev.plus(current, other)
          def -(other: N): N = ev.minus(current, other)
          def *(other: N): N = ev.times(current, other)
          def /(other: N): N = ev.divide(current, other)
          def ^(expo: N): N = ev.power(current, expo)

          def sqrt(): N = ev.squareRoot(current)
          def abs(): N = ev.absoluteValue(current)

          def negate(): N = ev.negate(current)
          def inverse(): N = ev.inverse(current)

          def isZero: Boolean = ev.isZero(current)
          def isNegative: Boolean = ev.isNegative(current)
          def isEqualTo(other: N): Boolean = ev.areEqual(current, other)
          /*override*/ def compare(other: N): Int = ev.minus(current, other).toDouble.toInt

          def toDouble: Double = ev.doubleValue(current)
     }



     implicit object IntIsRealNumber extends RealNumber[Int] with Show[Int] {
          val one: Int = 1
          val zero: Int = 0

          def plus(x: Int, y: Int): Int = x + y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y

          def power(x: Int, y: Int): Int = math.pow(x, y).toInt
          def squareRoot(x: Int): Int = math.sqrt(x).toInt  //this chops off
          def absoluteValue(x: Int): Int = math.abs(x)

          def negate(x: Int): Int = -x
          //def inverse(x: Int): Int = 1 / x //note won't get a reciprocal ...

          def isZero(x: Int): Boolean = x == 0
          def isNegative(x: Int): Boolean = x < 0
          def areEqual(x: Int, y: Int): Boolean = x == y

          //def compare(x: Int) = minus(implicitly[Int], x)
          //def compare(x: Int, y: Int) = x.compare(y)

          def doubleValue(x: Int): Double = x * 1.0
          def show(x: Int): String = x.toString
     }



     implicit object DoubleIsRealNumber extends RealNumber[Double] with Show[Double] {
          val one: Double = 1.0
          val zero: Double = 0.0

          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y

          def power(x: Double, y: Double): Double = math.pow(x, y)
          def squareRoot(x: Double): Double = math.sqrt(x)  //this chops off
          def absoluteValue(x: Double): Double = math.abs(x)

          def negate(x: Double): Double = -x
          //def inverse(x: Double): Double = 1 / x //note won't get a reciprocal ...

          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def areEqual(x: Double, y: Double): Boolean = x == y

          //def compare(x: Double) = implicitly[Number[Double]].compare(x)
          //def compare(x: Double, y: Double): Int = x.compare(y)

          def doubleValue(x: Double): Double = x
          def show(x: Double): String = x.toString
     }

     //implicit class Complex[N : Number](re: N, im: N)

     implicit def ComplexIsNumber[R : RealNumber] = new Number[Complex[R]] with Show[Complex[R]] {


          type C = Complex[R]
          val gen = implicitly[RealNumber[R]]

          val zero: C = Complex(gen.zero, gen.zero)
          val one: C = Complex(gen.one, gen.zero)
          private val genTwo: R = gen.one + gen.one


          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)
          def times(x: C, y: C): C = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: C, y: C): C = {
               val newNumerator: C = times(x, y)
               val newDenominator: R = absoluteValue(y).re
               //todo assert(y.abs().im.isZero)
               Complex(x.re / newDenominator, x.im / newDenominator)
          }

          //todo the inner class can return abs(): N not abs(): Complex[N] ????????????? or make them return DOUBLE.
          def power(x: C, y: C): C = ???
          def squareRoot(x: C): C = ???
          //todo can the inner class return abs(): N ?
          def absoluteValue(x: C): C = Complex((x.re ^ genTwo + x.im ^ genTwo).sqrt(), gen.zero)

          def negate(x: C): C = Complex(x.re.negate(), x.im.negate())

          //todo how to mix in Ordered/Ordering? none fits the bill.
          /*def compare(x: C): Int = (toDouble(implicitly[C]) - toDouble(x)).toInt
          def compare(x: C, y: C): Int = x.compare(y)*/

          def areEqual(x: C, y: C): Boolean = gen.areEqual(x.re, y.re) && gen.areEqual(x.im, y.im)
          def isZero(x: C): Boolean = areEqual(x, zero)
          def isNegative(x: C): Boolean = x.re.isNegative && x.im.isNegative
          def isReal(x: C): Boolean = x.im.isZero
          def isImaginary(x: C): Boolean = !isReal(x)

          def doubleValue(x: C): Double = absoluteValue(x).re.toDouble
          def show(x: C): String = x.re.toString + " + " + x.im.toString + "i" //todo fix later
     }


     //todo: weird "can't find type $anon" error when this is implicit val - change to object and it works ?

     implicit object RealIsNumber extends RealNumber[Real] with Show[Real] {
          val zero: Real = Real(0)
          val one: Real = Real(1)

          def plus(x: Real, y: Real): Real = Real(x.double + y.double)
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)

          //the inner class can return abs(): N not abs(): Complex[N]
          def power(x: Real, y: Real): Real = Real(math.pow(x.double, y.double))
          def squareRoot(x: Real): Real = Real(math.sqrt(x.double))
          def absoluteValue(x: Real): Real = Real(math.abs(x.double))

          def negate(x: Real): Real = Real(-x.double)

          ///todo   def compare(x: Real): Int = (implicitly[Real].value - x.value).toInt
          def areEqual(x: Real, y: Real): Boolean = x.double == y.double
          def isZero(x: Real): Boolean = areEqual(x, zero)
          def isNegative(x: Real): Boolean = x.double < 0

          def doubleValue(x: Real): Double = x.double
          def show(x: Real): String = x.double.toString
     }



     implicit object RationalIsRealNumber extends RealNumber[Rational] with Show[Rational] {

          val zero: Rational = Rational(0, 1)
          val one: Rational = Rational(1, 1)


          def plus(x: Rational, y: Rational): Rational =
               Rational(x.num*y.den + y.num*x.den, x.den * y.den)

          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)

          //the inner class can return abs(): N not abs(): Complex[N]
          def power(x: Rational, y: Rational): Rational = {
               val doublePow: Double = math.pow(doubleValue(x), doubleValue(y))
               val frac: Fraction = Fraction.getFraction(doublePow)
               Rational(frac.getNumerator, frac.getDenominator)
          }

          def squareRoot(x: Rational): Rational = {
               val theSquaredRoot: Double = math.sqrt(doubleValue(x))
               val frac: Fraction = Fraction.getFraction(theSquaredRoot)
               Rational(frac.getNumerator, frac.getDenominator)
          }

          def absoluteValue(x: Rational): Rational = ??? //todo why error? Rational(x.num.abs(), x.den.abs())

          def negate(x: Rational): Rational = Rational(x.num.negate(), x.num.negate())

          //todo def compare(x: Rational): Int = (toDouble(implicitly[Rational]) - toDouble(x)).toInt

          def areEqual(x: Rational, y: Rational): Boolean = x.num == y.num && x.num == y.num
          def isZero(x: Rational): Boolean = areEqual(x, zero)
          def isNegative(x: Rational): Boolean = x.num == 0

          def doubleValue(x: Rational): Double = x.num * 1.0 / x.num
          def show(x: Rational): String = x.num.toString + "/" + x.den.toString //todo fix later.
     }


     ///---------------
     implicit class ToImaginary[R : RealNumber](private val imaginaryPart: R) /*extends AnyVal*/ {
          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }
     implicit class ToComplexMixed[R: RealNumber](private val realPart: R) /*extends  AnyVal*/ {
          def +(that: Imaginary[R]) = Complex(realPart, that.im)
     }
}
import Number._





private[numeric] sealed trait ComplexNumberCreator[T]{
     val re: T
     val im: T
}

case class Complex[R:RealNumber](re:R, im:R) extends ComplexNumberCreator[R] {
     //private val ev: Number[Complex[R]] = implicitly[Number[Complex[R]]]

     override def toString: String = Complex(re, im).show
     //re.toString + " + " + im.toString + "i" // todo fix later
}

case class Real(double: Double) extends ComplexNumberCreator[Real] {
     val re: Real = Real(double)
     val im: Real = Real.ZERO

     //private val s: Show[Real] = implicitly[Show[Real]]

     override def toString = Real(double).show //double.toString //

     //todo - this asinstnace of cast seems patchy and weird ...?? -- use either 'this' or 'double' - ok?
     //implicit def +[R: RealNumber](imaginary: Imaginary[R]): Complex[R] = Complex(this.asInstanceOf[R], imaginary.im)
}

case class Imaginary[R: RealNumber](private val theImag: R) extends ComplexNumberCreator[R] {
     val gen = implicitly[RealNumber[R]]

     val re: R = gen.zero
     val im: R = theImag

     implicit def i: Imaginary[R] = this
}

//todo - get ACTUAL interoperabilit ybetween the types, not this measely attempt that just fuses rationals and reals.

case class Rational(private val n: Int, private val d: Int) /*extends Real(n * 1.0 / d)*/ {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     //private val s: Show[Rational] = implicitly[Show[Rational]]

     override def toString: String = Rational(num, den).show // num.toString + "/" + den.toString //.show(this)
}




object Complex {
     def ZERO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.one, gen.zero)
     //def i[N](implicit gen: RealNumber[N]): Complex[N] = Complex(gen.zero, gen.one)

     //additional apply method - cannot override the automatically created one. Complex(_,_)
     def apply[R](realPart: R)(implicit gen: RealNumber[R]) = new Complex(realPart, gen.zero)

     /** --- Implicits --- */
     /*implicit def fromDouble(double: Double): Complex[Double] = new Complex(double, 0)
     implicit def fromInt(int: Int): Complex[Int] = new Complex(int, 0)
     implicit def fromReal(real: Real): Complex[Real] = new Complex(real, Real.ZERO)
     implicit def fromRational(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)*/
}

object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)

     /** --- Implicits --- */
     /*implicit def fromDouble(double: Double): Real = new Real(double)
     implicit def fromInt(int: Int): Real = new Real(int)
     implicit def fromComplex[N : Number](complex: Complex[N]): Real = new Real(complex.toDouble)
     implicit def fromRational(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)*/
}

object Rational {
     val ZERO: Rational = new Rational(0, 1)
     val ONE: Rational = new Rational(1, 1)

     /** --- Applies --- */
     def apply(numerator: Int): Rational = new Rational(numerator, 1)

     def apply(fracAsDouble: Double): Rational = {
          val f = Fraction.getFraction(fracAsDouble).reduce()
          new Rational(f.getNumerator, f.getDenominator)
     }

     /** --- Implicits --- */
     /*implicit def fromDouble(double: Double): Rational = Rational(double)
     implicit def fromInt(int: Int): Real = new Real(int)
     implicit def fromComplex[N : Number](complex: Complex[N]): Real = new Real(complex.toDouble)
     implicit def fromReal(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)*/
}

/*import Complex._
import Real._
import Rational._*/





object NumberTester extends App {


//     println(Complex(Rational(1,2), new Real(3)))
//     println(Complex(1, Rational(1,4)))

     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i
     val b: Complex[Int] = 3 + 5.i

     println(a)
     println(b)
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}