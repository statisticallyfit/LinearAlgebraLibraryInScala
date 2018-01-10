package linalg.numeric


import linalg.show.Show._
import linalg.theory._
import linalg.util.ImplicitConversions._
import org.apache.commons.lang3.math.Fraction

import scala.language.implicitConversions


/**
  *
  * note: if things get too hard, just make trait Numerical and make spire's Complex, Rational, Real etc implement
  * note: that Numerical in typeclass form then make Vector[N : Numerical]
  *
  *
  * todo: make trait Trig, and mix in appropriately (decide if need it - complex: if must stay angle as N or as Double)
  * todo make class Polar to interact with Complex.
  *
  * todo: move Complex theta, polar ... functionality in same place with the main +, -, / code ...
  *
  * todo: interoperability: Complex + Real + Rational + Int + Double --- towrk
  *
  * todo: make trait Equality that inherits from Ordered[N] and make equality typeclass instance for
  *
  * Features:
  * - rational number reducability upon creation
  * - complex number .i creation
  * - complex numbers can have rational arguments too, anything that implements the RealNumber typeclass.
  * - printing occurs via neat Show trait, just like in Haskell.
  *
  *
  * note: Source for complex .i accessor:
  * https://stackoverflow.com/questions/17381896/scala-simple-notation-of-imaginary-number
  */

//note or can just have Vec[F : Field]
/*object FieldImplicits {
     implicit class FieldOps[F : spire.algebra.Field](current: F) {
          val ev = implicitly[Field[F]]

          def +(other: F): F = ev.plus(current, other)
     }
}
import FieldImplicits._

case class Vec[N : spire.algebra.Field](elems: N*){
     def +(other: Vec[N]): Vec[N] = new Vec(elems.zip(other.elems).map(p => p._1 + p._2):_*)

}*/


trait Number[R <: N, N] extends Field[N] /*with Field[R]*/ {

     //inherited: add, multiply, divide, one, zero, negate, inverse
     def one: N
     def zero: N

     def plus(x: N, y: R): N
     def minus(x: N, y: R): N = plus(x, negate(y))
     def times(x: N, y: R): N
     def divide(x: N, y: R): N
     def power(x: N, y: R): N
     def squareRoot(x: N): N
     def absoluteValue(x: N): N

     def negate(x: N): N
     def inverse(x: N): N

     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean
     def areEqual(x: N, y: N): Boolean

     def doubleValue(x: N): Double
     /*def plus(x: N, y: N): N
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

     def doubleValue(x: N): Double*/
}

trait RealNumber[R <: N, N] extends Number[R, N]



object Number {

     implicit class NumberOps[R <: N, N](current: N)(implicit n: Number[R, N]) /*extends Ordered[N]*/ {
          //private val n = implicitly[Number[R, N]]
          //private val r = implicitly[RealNumber[R]]
          //private val c = implicitly[Number[Complex[R]]]

          def +(other: R): N = n.plus(current, other)
          def -(other: R): N = n.minus(current, other)
          def *(other: R): N = n.times(current, other)
          def /(other: R): N = n.divide(current, other)
          def ^(expo: R): N = n.power(current, expo)

          def sqrt(): N = n.squareRoot(current)
          def abs(): N = n.absoluteValue(current)

          def negate(): N = n.negate(current)
          def inverse(): N = n.inverse(current)

          def isZero: Boolean = n.isZero(current)
          def isNegative: Boolean = n.isNegative(current)
          def isEqualTo(other: N): Boolean = n.areEqual(current, other)
          def compare(other: R): Int = n.minus(current, other).toDouble.toInt //todo

          def toDouble: Double = n.doubleValue(current)
     }




     implicit def ComplexIsNumber[R <: N, N](implicit gen: RealNumber[R, N]) = new Number[R, Complex[R]]  {

          type C = Complex[R]
          ///val gen = implicitly[RealNumber[R, Complex[R]]]

          val zero: C = Complex(gen.zero, gen.zero)
          val one: C = Complex(gen.one, gen.zero)
          private val genTwo: R = gen.one + gen.one

          //def plusO(x: C, y: R): C = Complex(x.re + y, x.im)
          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)

          //def minusReal(x: C, y: R): C = Complex(x.re - y, x.im)

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

          def areEqual(x: C, y: C): Boolean = gen.areEqual(x.re, y.re) && gen.areEqual(x.im, y.im)
          def isZero(x: C): Boolean = areEqual(x, zero)
          def isNegative(x: C): Boolean = x.re.isNegative && x.im.isNegative
          def isReal(x: C): Boolean = x.im.isZero
          def isImaginary(x: C): Boolean = !isReal(x)

          def doubleValue(x: C): Double = absoluteValue(x).re.toDouble
     }


     //todo: weird "can't find type $anon" error when this is implicit val - change to object and it works ?

     implicit object RealIsNumber extends RealNumber[Real]  {
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

          def areEqual(x: Real, y: Real): Boolean = x.double == y.double
          def isZero(x: Real): Boolean = areEqual(x, zero)
          def isNegative(x: Real): Boolean = x.double < 0

          def doubleValue(x: Real): Double = x.double
     }



     implicit object RationalIsRealNumber extends RealNumber[Rational]  {

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

          def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))

          def negate(x: Rational): Rational = Rational(x.num.negate(), x.num.negate())

          def areEqual(x: Rational, y: Rational): Boolean = x.num == y.num && x.num == y.num
          def isZero(x: Rational): Boolean = areEqual(x, zero)
          def isNegative(x: Rational): Boolean = x.num == 0

          def doubleValue(x: Rational): Double = x.num * 1.0 / x.num
     }


     implicit object IntIsRealNumber extends RealNumber[Int]  {
          val one: Int = 1
          val zero: Int = 0

          def plus(x: Int, y: Int): Int = x + y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y

          def power(x: Int, y: Int): Int = math.pow(x, y).toInt
          def squareRoot(x: Int): Int = math.sqrt(x).toInt  //this chops off
          def absoluteValue(x: Int): Int = math.abs(x)

          def negate(x: Int): Int = -x

          def isZero(x: Int): Boolean = x == 0
          def isNegative(x: Int): Boolean = x < 0
          def areEqual(x: Int, y: Int): Boolean = x == y

          def doubleValue(x: Int): Double = x * 1.0
     }



     implicit object DoubleIsRealNumber extends RealNumber[Double]  {
          val one: Double = 1.0
          val zero: Double = 0.0

          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y

          def power(x: Double, y: Double): Double = math.pow(x, y)
          def squareRoot(x: Double): Double = math.sqrt(x)  //this chops off
          def absoluteValue(x: Double): Double = math.abs(x)

          def negate(x: Double): Double = -x

          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def areEqual(x: Double, y: Double): Boolean = x == y

          def doubleValue(x: Double): Double = x
     }


     ///---------------------------------------------------------------------------------------------
     //note: removing the 'extends AnyVal' finally fixed my final problems!!!

     implicit class ToImaginary[R : RealNumber](private val imaginaryPart: R) /*extends AnyVal*/ {
          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }
     implicit class ToComplexMixed[R: RealNumber](private val realPart: R) /*extends  AnyVal*/ {
          def +(that: Imaginary[R]) = Complex(realPart, that.im)
     }
}
import Number._





private[numeric] sealed trait ComplexMaker[T]{
     val re: T
     val im: T
}

case class Complex[R:RealNumber](re:R, im:R) extends ComplexMaker[R] {
     def +(other: R): Complex[R] = Complex(re + other, im)
     def -(other: R): Complex[R] = Complex(re - other, im)
     def *(other: R): Complex[R] = Complex(re * other, im * other)
     def /(other: R): Complex[R] = Complex(re / other, im / other)

     override def toString: String = Complex(re, im).show
}

case class Real(double: Double) extends ComplexMaker[Real] {
     val re: Real = this
     val im: Real = Real.ZERO

     override def toString = Real(double).show
}

case class Imaginary[R: RealNumber](private val theImag: R) extends ComplexMaker[R] {
     val gen = implicitly[RealNumber[R]]

     val re: R = gen.zero
     val im: R = theImag

     implicit def i: Imaginary[R] = this

     override def toString: String = im match {
          case _: Rational => im.isNegative match {
               case true => " - (" + im.negate().toString + ")" + "i"
               case false => " + (" + im.toString + ")" + "i"
          }
          case _ => im.isNegative match {
               case true => " - " + im.negate().toString + "i"
               case false => " + " + im.toString + "i"
          }
     }
}

//todo - get ACTUAL interoperability between the types, not this measly attempt that just fuses rationals and reals.

case class Rational(private val n: Int, private val d: Int)  {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     override def toString: String = Rational(num, den).show
}




object Complex {
     def ZERO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.one, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealNumber[R]) = new Complex(realPart, gen.zero)

     /** --- Implicits --- */
     implicit def fromDouble(double: Double): Complex[Double] = new Complex(double, 0)
     implicit def fromInt(int: Int): Complex[Int] = new Complex(int, 0)
     implicit def fromReal(real: Real): Complex[Real] = new Complex(real, Real.ZERO)
     implicit def fromRational(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)
}

object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)

     /** --- Implicits --- */
     implicit def fromDouble(double: Double): Real = new Real(double)
     implicit def fromInt(int: Int): Real = new Real(int)
     implicit def fromComplex[R : RealNumber](complex: Complex[R]): Real = new Real(complex.toDouble)
     implicit def fromRational(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)
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
     implicit def fromDouble(double: Double): Rational = Rational(double)
     implicit def fromInt(int: Int): Real = new Real(int)
     implicit def fromComplex[R : RealNumber](complex: Complex[R]): Real = new Real(complex.toDouble)
     implicit def fromReal(rational: Rational): Complex[Rational] = new Complex(rational, Rational.ZERO)
}

//import Complex._
//import Real._
//import Rational._





object NumberTester extends App {


//     println(Complex(Rational(1,2), new Real(3)))
//     println(Complex(1, Rational(1,4))) //todo interoperability aspect.

     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i
     val b: Complex[Int] = 3 + 5.i

     implicit def intToComplexRat(int: Int): Complex[Rational] = Complex(Rational(int), Rational.ZERO)

     println(a + Rational(1))
     println(b)
     println(Complex(1,2) + 4)
     println(7 + 8.i + 3)
     //println((7 + Rational(3,4).i) + 2)
     // todo doesn't work - need to make overall trait NumberLower[L, H] { def +(other: L): H ...}
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(new Rational(4, 8))
     println(Real(2) + Real(3) + 1)
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))


     //println(spire.math.Complex(1, 2) + 4)
}