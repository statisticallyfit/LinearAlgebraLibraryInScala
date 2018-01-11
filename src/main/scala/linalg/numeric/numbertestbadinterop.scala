/*
package linalg.numeric


import linalg.show.Show._
import linalg.theory._
import linalg.util.ImplicitConversions._
import org.apache.commons.lang3.math.Fraction

import scala.language.implicitConversions


trait GeneralNumber[F, T] {
     def one: T
     def zero: T

     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     //tododef power(from: T, to: F): T
     def areEqual(from: F, to: T): Boolean
}

trait Number[N] extends GeneralNumber[N, N] {
     def power(x: N, y: N): N
     def squareRoot(x: N): N
     def absoluteValue(x: N): N
     def negate(x: N): N
     def inverse(x: N): N
     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean
     def doubleValue(x: N): Double

     //inherited: add, multiply, divide, one, zero, negate, inverse
     /*def one: N
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

     def doubleValue(x: N): Double*/
}

trait RealNumber[R] extends Number[R]





object Number {



     // ------------------------------------------------------------------------------------------------
     implicit class GeneralNumberOps[F, T](from: F)(implicit gen: GeneralNumber[F, T]){
          def +(to: T): T = gen.plus(from, to)
          def -(to: T): T = gen.minus(from, to)
          def *(to: T): T = gen.times(from, to)
          def /(to: T): T = gen.divide(from, to)
          //def ^(to: T): T = gen.power(from, to)
          def isEqual(to: T): Boolean = gen.areEqual(from, to)
     }

     implicit def ConversionComplex[R : RealNumber] = new GeneralNumber[R, Complex[R]] {

          type C = Complex[R]
          val gen = implicitly[RealNumber[R]]

          val zero: C = Complex(gen.zero, gen.zero)
          val one: C = Complex(gen.one, gen.zero)

          def plus(x: R, y: C): C = Complex(x + y.re, y.im)
          def minus(x: R, y: C): C = Complex(x - y.re, y.im)
          def times(x: R, y: C): C = Complex(x * y.re, x * y.im)
          def divide(x: R, y: C): C = Complex(y.re / x, y.im / x)
          def power(x: C, y: R): C = ??? //todo implement
          def areEqual(x: R, y: C): Boolean = false
     }

     implicit def ConversionReal[R: RealNumber] = new GeneralNumber[R, Real] {

          val zero: Real = Real.ZERO
          val one: Real = Real.ONE

          def plus(x: R, y: Real): Real = Real(x.toDouble + y.double)
          def minus(x: R, y: Real): Real = Real(x.toDouble - y.double)
          def times(x: R, y: Real): Real = Real(x.toDouble * y.double)
          def divide(x: R, y: Real): Real = Real(x.toDouble / y.double)
          def power(x: Real, y: R): Real = Real(math.pow(x.double, y.toDouble))
          def areEqual(x: R, y: Real): Boolean = x.toDouble == y.double
     }

     implicit def ConversionRational[R: RealNumber] = new GeneralNumber[R, Rational] {

          val zero: Rational = Rational.ZERO
          val one: Rational = Rational.ONE

          def plus(x: R, y: Rational): Rational = Rational(x.toDouble + y.toDouble)
          def minus(x: R, y: Rational): Rational = Rational(x.toDouble - y.toDouble)
          def times(x: R, y: Rational): Rational = Rational(x.toDouble * y.toDouble)
          def divide(x: R, y: Rational): Rational = Rational(x.toDouble / y.toDouble)
          def power(x: Rational, y: R): Rational = Rational(math.pow(x.toDouble, y.toDouble))
          def areEqual(x: R, y: Rational): Boolean = x.toDouble == y.toDouble
     }






     implicit class NumberOps[N: Number](current: N) /*extends Ordered[N]*/ {

          private val number = implicitly[Number[N]]

          def +(other: N): N = number.plus(current, other)
          def -(other: N): N = number.minus(current, other)
          def *(other: N): N = number.times(current, other)
          def /(other: N): N = number.divide(current, other)
          def ^(expo: N): N = number.power(current, expo)

          def sqrt(): N = number.squareRoot(current)
          def abs(): N = number.absoluteValue(current)

          def negate(): N = number.negate(current)
          def inverse(): N = number.inverse(current)

          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)
          def isEqualTo(other: N): Boolean = number.areEqual(current, other)

          //todo def compare(other: N): Int = number.minus(current, other).toDouble.toInt //todo

          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!
     }



     implicit def ComplexIsNumber[R : RealNumber]: Number[Complex[R]] = new Number[Complex[R]]  {

          type C = Complex[R]
          val gen = implicitly[RealNumber[R]]

          val zero: C = Complex(gen.zero, gen.zero)
          val one: C = Complex(gen.one, gen.zero)
          private val genTwo: R = gen.one + gen.one


          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)
          def minus(x: C, y: C): C = plus(x, negate(y))
          def times(x: C, y: C): C = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: C, y: C): C = {
               val newNumerator: C = times(x, y)
               val newDenominator: R = absoluteValue(y).re
               //todo assert(y.abs().im.isZero)
               Complex(x.re / newDenominator, x.im / newDenominator)
          }
          def inverse(x :C): C = divide(one, x)

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
          def minus(x: Real, y: Real): Real = plus(x, negate(y)) //todo why cannot use dot operator?
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)
          def inverse(x: Real): Real = divide(one, x)

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
          def minus(x: Rational, y: Rational): Rational = plus(x, negate(y))
          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
          def inverse(x: Rational): Rational = divide(one, x)

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
          def negate(x: Rational): Rational = Rational(-x.num, -x.den)
          def areEqual(x: Rational, y: Rational): Boolean = x.num == y.num && x.den == y.den
          def isZero(x: Rational): Boolean = areEqual(x, zero)
          def isNegative(x: Rational): Boolean = x.num < 0

          def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
     }


     implicit object IntIsRealNumber extends RealNumber[Int]  {
          val one: Int = 1
          val zero: Int = 0

          def plus(x: Int, y: Int): Int = x + y
          def minus(x: Int, y: Int): Int = x - y
          def times(x: Int, y: Int): Int = x * y
          def divide(x: Int, y: Int): Int = x / y
          def inverse(x: Int): Int = divide(one, x) //todo gets chopped off
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
          def minus(x: Double, y: Double): Double = x - y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y
          def inverse(x: Double): Double = divide(one, x)
          def power(x: Double, y: Double): Double = math.pow(x, y)
          def squareRoot(x: Double): Double = math.sqrt(x)
          def absoluteValue(x: Double): Double = math.abs(x)

          def negate(x: Double): Double = -x

          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def areEqual(x: Double, y: Double): Boolean = x == y

          def doubleValue(x: Double): Double = x
     }


}
import Number._



//represents a conversion between numbers
//todo is it weird that it has similar methods as Number? Repetitive? Maybe have general number type with
// SimpleNumber[R] ext Number[R, R]
/*trait Conversion[F, T] {
     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     def power(from: F, to: T): T
     def areEqual(from: F, to: T): Boolean
}*/
/*object From {
     implicit def GeneralRealToComplex[R: RealNumber]: From[R, Complex[R]] = new From[R, Complex[R]]{
          def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
          def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
          def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
          def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
          def power(from: R, to: Complex[R]): Complex[R] = ??? //todo
          def areEqual(from: R, to: Complex[R]): Boolean = false
     }
     implicit class ConvertFrom[F, T](val from: F)(implicit conv: From[F, T]){
          def +(to: T): T = conv.plus(from, to)
          def -(to: T): T = conv.minus(from, to)
          def *(to: T): T = conv.times(from, to)
          def /(to: T): T = conv.divide(from, to)
          def ^(to: T): T = conv.power(from, to)
          def isEqual(to: T): Boolean = conv.areEqual(from, to)
     }
     implicit class ConvertTo[F, T](val to: T)(implicit conv: From[F, T]){
          def +(from: F): T = conv.plus(from, to)
          def -(from: F): T = conv.minus(from, to)
          def *(from: F): T = conv.times(from, to)
          def /(from: F): T = conv.divide(from, to)
          def ^(from: F): T = conv.power(from, to)
          def isEqual(from: F): Boolean = conv.areEqual(from, to)
     }
}*/
//import NumberGeneral._



private[numeric] trait ComplexLike[T]{
     val re: T
     val im: T
}
object ComplexLike {
     //mechanism: takes something that implements RealNumber and gives it .i accessor, returning Imaginary.
     implicit class ToImaginary[R : RealNumber](private val imaginaryPart: R){
          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }
     //mechanism: takes something that implements RealNumber and makes it addable with Imaginary (which BTW cannot
     // implement Number because i*i = -1, not imaginary)
     implicit class ToComplex[R: RealNumber](private val realPart: R) {
          def +(that: Imaginary[R]) = Complex(realPart, that.im)
          def -(that: Imaginary[R]) = Complex(realPart, that.im)
     }
}
import ComplexLike._






case class Real(double: Double) extends ComplexLike[Real] {
     val re: Real = this
     val im: Real = Real.ZERO

     override def toString = Real(double).show
}


case class Rational(private val n: Int, private val d: Int) extends ComplexLike[Rational] {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     val re: Rational = this
     val im: Rational = Rational.ZERO

     override def toString: String = Rational(num, den).show
}


case class Complex[R:RealNumber](re:R, im:R) extends ComplexLike[R] {
     override def toString: String = Complex(re, im).show
}


case class Imaginary[R: RealNumber](private val theImag: R) extends ComplexLike[R] {
     private val gen = implicitly[RealNumber[R]]

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





object Complex {
     def ZERO[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealNumber[R]): Complex[R] = new Complex(gen.one, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealNumber[R]): Complex[R] = new Complex(realPart, gen.zero)
}

object Real {
     val ZERO: Real = new Real(0)
     val ONE: Real = new Real(1)
}

object Rational {
     val ZERO: Rational = new Rational(0, 1)
     val ONE: Rational = new Rational(1, 1)

     def apply(numerator: Int): Rational = new Rational(numerator, 1)

     def apply(fracAsDouble: Double): Rational = {
          val f = Fraction.getFraction(fracAsDouble).reduce()
          new Rational(f.getNumerator, f.getDenominator)
     }
}





object NumberTester extends App {


     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1)
     val b: Complex[Int] = 3 + 5.i + 3

     println(a)
     println(b)
     println(a + Rational(1))
     println(Rational(33) + a)
     println(23.0 + (1.0 + 3.0.i))
     println((1.5 + 3.2.i) + 23.2)
     println((1 + 3.i) + 1)
     println(1 + (1 + 3.i))
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     //println((8 + 2.i) < (9 + 2.i)) //todo separate out ordering

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}*/
