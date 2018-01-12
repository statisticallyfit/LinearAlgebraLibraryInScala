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
  *
  * todo: do Complex theta, polar ... functionality in the Complex object.
  *
  * todo: make trait Equality that inherits from Ordered[N] and make equality typeclass instance for
  * todo: typeclassify the ComplexLike pattern instead of using inheritance.
  *
  *
  * Features:
  * - rational number reducability upon creation
  * - complex number .i creation
  * - complex numbers can have rational arguments too, anything that implements the RealNumber typeclass.
  * - printing occurs via neat Show trait, just like in Haskell.
  * - interoperability between Complex[R] and R types.
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
//todo how to typeclassify this?
/*trait Equality[E] extends Ordered[E] {
     def ==(e: E): Boolean
}*/

trait Number[N] extends Field[N] /*with ComplexMaker[N] -- takes entire type -wrtong - not inner type!*/ {

     //inherited: add, multiply, divide, one, zero, negate, inverse
     val one: N
     val zero: N

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N = plus(x, negate(y))
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
     def negate(x: N): N
     def inverse(x: N): N
     def isZero(x: N): Boolean
     def isNegative(x: N): Boolean
     def areEqual(x: N, y: N): Boolean

     def doubleValue(x: N): Double
}

trait Absolute[H, L]{
     def absoluteValue(x: H): L
}

trait Root[H, L]{
     def power(base: H, exp: L): H
     def nRoot(base: H, n: L): H // = power(base, one / n)
     def squareRoot(base: H): H // = nRoot(base, two)
}

trait RealLike[R] extends Number[R] with Absolute[R, R] with Root[R, R]


object Number {


     //TODO IMPORTANT - here tomorrow
     //todo try this: have Number[F, T] and inside use both plus(F, T): T and plus(T, T): T
     //todo then the implicit numberops takes the from: F and implements and then another one takes to: T and
     // implements.
     implicit class NumberOps[N: Number](current: N) /*extends Equality[N]*/ {

          private val number = implicitly[Number[N]]

          def +(other: N): N = number.plus(current, other)
          def -(other: N): N = number.minus(current, other)
          def *(other: N): N = number.times(current, other)
          def /(other: N): N = number.divide(current, other)
          def negate(): N = number.negate(current)
          def inverse(): N = number.inverse(current)
          def isZero: Boolean = number.isZero(current)
          def isNegative: Boolean = number.isNegative(current)
          def isEqual(other: N): Boolean = number.areEqual(current, other)
          def toDouble: Double = number.doubleValue(current)
          def toInt: Int = number.doubleValue(current).toInt // todo check this can be chopped off!
     }


     implicit def ComplexIsNumber[R : RealLike]: Number[Complex[R]] = new Number[Complex[R]]  {

          type C = Complex[R]
          val gen = implicitly[RealLike[R]]

          val zero: C = Complex.ZERO[R]
          val one: C = Complex.ONE[R]

          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)
          def times(x: C, y: C): C = Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
          def divide(x: C, y: C): C = {
               val prod: C = times(x, y)
               val absDenom: R = Complex.magnitude(y)
               Complex(prod.re / absDenom, prod.im / absDenom)
          }
          def negate(x: C): C = Complex(x.re.negate(), x.im.negate())
          def areEqual(x: C, y: C): Boolean = gen.areEqual(x.re, y.re) && gen.areEqual(x.im, y.im)
          def isZero(x: C): Boolean = areEqual(x, zero)
          def isNegative(x: C): Boolean = x.re.isNegative && x.im.isNegative
          def isReal(x: C): Boolean = x.im.isZero
          def isImaginary(x: C): Boolean = !isReal(x)
          def doubleValue(x: C): Double = Complex.magnitude(x).toDouble
     }


     //todo: weird "can't find type $anon" error when this is implicit val - change to object and it works ?
     implicit object RealIsNumber extends RealLike[Real]  {
          val zero: Real = Real.ZERO
          val one: Real = Real.ONE
          private val two: Real = plus(one, one)

          def plus(x: Real, y: Real): Real = Real(x.double + y.double)
          def times(x: Real, y: Real): Real = Real(x.double * y.double)
          def divide(x: Real, y: Real): Real = Real(x.double / y.double)

          def power(base: Real, exp: Real): Real = Real(math.pow(base.double, exp.double))
          def nRoot(base: Real, n: Real): Real = power(base, divide(one, n))
          def squareRoot(base: Real): Real = nRoot(base, two)

          def absoluteValue(x: Real): Real = Real(math.abs(x.double))

          def negate(x: Real): Real = Real(-x.double)
          def areEqual(x: Real, y: Real): Boolean = x.double == y.double
          def isZero(x: Real): Boolean = areEqual(x, zero)
          def isNegative(x: Real): Boolean = x.double < 0
          def doubleValue(x: Real): Double = x.double
     }


     implicit object RationalIsRealNumber extends RealLike[Rational]  {

          val zero: Rational = Rational(0)
          val one: Rational = Rational(1)
          private val two: Rational = plus(one, one)

          def plus(x: Rational, y: Rational): Rational = Rational(x.num*y.den + y.num*x.den, x.den*y.den)
          def times(x: Rational, y: Rational): Rational = Rational(x.num * y.num, x.den * y.den)
          def divide(x: Rational, y: Rational): Rational = Rational(x.num * y.den, x.den * y.num)
          def power(x: Rational, y: Rational): Rational = Rational(math.pow(doubleValue(x), doubleValue(y)))
          def nRoot()
          def squareRoot(base: Rational): Rational = nRoot(base, divide(one, ))
          def absoluteValue(x: Rational): Rational = Rational(math.abs(x.num), math.abs(x.den))
          def negate(x: Rational): Rational = Rational(-x.num, -x.den)
          def areEqual(x: Rational, y: Rational): Boolean = x.num == y.num && x.den == y.den
          def isZero(x: Rational): Boolean = areEqual(x, zero)
          def isNegative(x: Rational): Boolean = x.num < 0
          def doubleValue(x: Rational): Double = x.num * 1.0 / x.den
     }


     implicit object IntIsRealNumber extends RealLike[Int]  {
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



     implicit object DoubleIsRealNumber extends RealLike[Double]  {
          val one: Double = 1.0
          val zero: Double = 0.0


          def plus(x: Double, y: Double): Double = x + y
          def times(x: Double, y: Double): Double = x * y
          def divide(x: Double, y: Double): Double = x / y

          def power(x: Double, y: Double): Double = math.pow(x, y)
          def squareRoot(x: Double): Double = math.sqrt(x)
          def absoluteValue(x: Double): Double = math.abs(x)

          def negate(x: Double): Double = -x

          def isZero(x: Double): Boolean = x == 0
          def isNegative(x: Double): Boolean = x < 0
          def areEqual(x: Double, y: Double): Boolean = x == y

          def doubleValue(x: Double): Double = x
     }


     // ---------------------------------------------------------------------------

     implicit class RootOps[H, L](base: H)(implicit root: Root[H, L]){

          def ^(exp: L): H = root.power(base, exp)
          def sqrt(): H = root.squareRoot(base)
          def nRoot(n: L): H = root.nRoot(base, n)
     }
     implicit class AbsoluteOps[H, L](current: H)(implicit pos: Absolute[H, L]){
          def abs(): L = pos.absoluteValue(current)
     }

     implicit def ComplexHasRoot[R: RealLike] = new Root[Complex[R], R]{
          private val one = implicitly[RealLike[R]].one
          private val two = one + one

          def power(base: Complex[R], exp: R): Complex[R] =
               Complex(Complex.magnitude(base) ^ exp, Complex.angle(base) * exp)

          def nRoot(base: Complex[R], n: R): Complex[R] = power(base, one / n)

          def squareRoot(base: Complex[R]): Complex[R] = nRoot(base, one / two)
     }

     implicit def ComplexHasAbsoluteValue[R: RealLike] = new Absolute[Complex[R], R]{
          def absoluteValue(z: Complex[R]): R = Complex.magnitude(z)
     }

     /*implicit class OrderedOps[O: Ordering](current: O){

          private val ord = implicitly[Ordering[O]]

          def compare(that: O): Int = ord.compare(current, that)
     }
     implicit def ComplexIsOrdered[R: RealNumber] = new Ordering[Complex[R]] {
          def compare(x: Complex[R], y: Complex[R]): Int = x.toDouble.compare(y.toDouble)
     }
     implicit object RealIsOrdered extends Ordering[Real]{
          def compare(x: Real, y: Real): Int = x.double.compare(y.double)
     }
     implicit object RationalIsOrdered extends Ordering[Rational]{
          def compare(x: Rational, y: Rational): Int = x.num * y.den - x.den * y.num
     }*/
}
import Number._



// ---------------------------------------------------------------------------------------------------------


trait Trig[T] {

     /*val e: T
     val pi: T*/ //todo should I support?

     def sin(x: T): T
     def cos(x: T): T
     def tan(x: T): T
     def csc(x: T): T
     def sec(x: T): T
     def cot(x: T): T

     def arcsin(x: T): T
     def arccos(x: T): T
     def arctan(x: T): T
     def arccsc(x: T): T
     def arcsec(x: T): T
     def arccot(x: T): T

     //returns the theta component of polar (r, theta) of the x-y coordinate (x: T, y: T)
     def theta(y: T, x: T): T
}
object Trig {

     implicit object RationalHasTrig extends Trig[Rational] {
          def sin(x: Rational): Rational = Rational(math.sin(x.toDouble))
          def cos(x: Rational): Rational = Rational(math.cos(x.toDouble))
          def tan(x: Rational): Rational = Rational(math.tan(x.toDouble))
          def csc(x: Rational): Rational = Rational.ONE / sin(x)
          def sec(x: Rational): Rational = Rational.ONE / cos(x)
          def cot(x: Rational): Rational = Rational.ONE / tan(x)

          def arcsin(x: Rational): Rational = Rational(math.asin(x.toDouble))
          def arccos(x: Rational): Rational = Rational(math.acos(x.toDouble))
          def arctan(x: Rational): Rational = Rational(math.atan(x.toDouble))
          def arccsc(x: Rational): Rational = Rational.ONE / arcsin(x)
          def arcsec(x: Rational): Rational = Rational.ONE / arccos(x)
          def arccot(x: Rational): Rational = Rational.ONE / arctan(x)

          def theta(y: Rational, x: Rational): Rational = Rational(math.tan(y.toDouble / x.toDouble))
     }

     implicit object RealHasTrig extends Trig[Real] {
          def sin(x: Real): Real = Real(math.sin(x.double))
          def cos(x: Real): Real = Real(math.cos(x.double))
          def tan(x: Real): Real = Real(math.tan(x.double))
          def csc(x: Real): Real = Real.ONE / sin(x)
          def sec(x: Real): Real = Real.ONE / cos(x)
          def cot(x: Real): Real = Real.ONE / tan(x)

          def arcsin(x: Real): Real = Real(math.asin(x.double))
          def arccos(x: Real): Real = Real(math.acos(x.double))
          def arctan(x: Real): Real = Real(math.atan(x.double))
          def arccsc(x: Real): Real = Real.ONE / arcsin(x)
          def arcsec(x: Real): Real = Real.ONE / arccos(x)
          def arccot(x: Real): Real = Real.ONE / arctan(x)

          def theta(y: Real, x: Real): Real = Real(math.tan(y.double / x.double))
     }
     //todo - need int has trig? cannot... if not have it then will implicit Trig of int not work when calculating
     // theta?
     implicit object DoubleHasTrig extends Trig[Double] {
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


// ---------------------------------------------------------------------------------------------------------



//represents a conversion between numbers
//todo is it weird that it has similar methods as Number? Repetitive? Maybe have general number type with
// SimpleNumber[R] ext Number[R, R]
trait Conversion[F, T] {
     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     def power(from: F, to: T): T
     def areEqual(from: F, to: T): Boolean
}
object Conversion {
     implicit def GeneralRealToComplex[R: RealLike]: Conversion[R, Complex[R]] = new Conversion[R, Complex[R]]{
          def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
          def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
          def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
          def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
          def power(from: R, to: Complex[R]): Complex[R] = ??? //todo
          def areEqual(from: R, to: Complex[R]): Boolean = false
     }
     implicit class ConvertFrom[F, T](val from: F)(implicit conv: Conversion[F, T]){
          def +(to: T): T = conv.plus(from, to)
          def -(to: T): T = conv.minus(from, to)
          def *(to: T): T = conv.times(from, to)
          def /(to: T): T = conv.divide(from, to)
          def ^(to: T): T = conv.power(from, to)
          def isEqual(to: T): Boolean = conv.areEqual(from, to)
     }
     implicit class ConvertTo[F, T](val to: T)(implicit conv: Conversion[F, T]){
          def +(from: F): T = conv.plus(from, to)
          def -(from: F): T = conv.minus(from, to)
          def *(from: F): T = conv.times(from, to)
          def /(from: F): T = conv.divide(from, to)
          def ^(from: F): T = conv.power(from, to)
          def isEqual(from: F): Boolean = conv.areEqual(from, to)
     }
}
import Conversion._


// ---------------------------------------------------------------------------------------------------------

private[numeric] trait ComplexLike[T]{
     val re: T
     val im: T
}
object ComplexLike {
     //mechanism: takes something that implements RealNumber and gives it .i accessor, returning Imaginary.
     implicit class ToImaginary[R : RealLike](private val imaginaryPart: R){
          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }
     //mechanism: takes something that implements RealNumber and makes it addable with Imaginary (which BTW cannot
     // implement Number because i*i = -1, not imaginary)
     implicit class ToComplex[R: RealLike](private val realPart: R) {
          def +(that: Imaginary[R]) = Complex(realPart, that.im)
          def -(that: Imaginary[R]) = Complex(realPart, that.im)
     }
}
import ComplexLike._




// ---------------------------------------------------------------------------------------------------------


case class Real(double: Double) extends Ordered[Real] with ComplexLike[Real] {
     val re: Real = this
     val im: Real = Real.ZERO

     def ==(other: Real): Boolean = this.compare(other) == 0
     override def compare(other: Real): Int = double.compare(other.double)
     override def toString = Real(double).show
}


case class Rational(private val n: Int, private val d: Int) extends Ordered[Rational] with ComplexLike[Rational] {
     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
     val num: Int = reduced.getNumerator
     val den: Int = reduced.getDenominator

     val re: Rational = this
     val im: Rational = Rational.ZERO

     def ==(other: Rational): Boolean = this.compare(other) == 0
     override def compare(other: Rational): Int = num * other.den - den * other.num
     override def toString: String = Rational(num, den).show
}


case class Complex[R:RealLike](re:R, im:R) extends Ordered[Complex[R]] with ComplexLike[R] {
     def ==(other: Complex[R]): Boolean = re == other.re && im == other.im
     override def compare(other: Complex[R]): Int = Complex(re, im).toDouble.compare(other.toDouble)
     override def toString: String = Complex(re, im).show
     //todo check if compare modulus comparison approach makes sense - if it does actually compare a < b then c < d etc
     //todo maybe not - make your own trait with <, <=, ==, etc.
}


case class Imaginary[R: RealLike](private val theImag: R) extends Ordered[Imaginary[R]] with ComplexLike[R] {
     private val gen = implicitly[RealLike[R]]

     val re: R = gen.zero
     val im: R = theImag

     implicit def i: Imaginary[R] = this

     def ==(other: Imaginary[R]): Boolean = this.compare(other) == 0
     override def compare(other: Imaginary[R]): Int = im.toDouble.compare(other.im.toDouble)
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
     def ZERO[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.zero, gen.zero)
     def ONE[R](implicit gen: RealLike[R]): Complex[R] = new Complex(gen.one, gen.zero)

     def apply[R](realPart: R)(implicit gen: RealLike[R]): Complex[R] = new Complex(realPart, gen.zero)


     // --- Operations ---
     def polar[R: RealLike](z: Complex[R])(implicit t: Trig[R]): Complex[R] = Complex(magnitude(z), angle(z))

     def magnitude[R: RealLike](z: Complex[R]): R = (z.re * z.re + z.im * z.im).sqrt()

     //just returns the value of theta for the complex number: theta = arctan(b / a), where c = a + bi
     def angle[R: RealLike](z: Complex[R])(implicit trig: Trig[R]): R = {
          //println("Spire's theta: " + new spire.math.Complex[R](z.re, z.im).arg)

          trig.theta(z.re, z.im)
     }

     def rootsOfUnity[R: RealLike](z: Complex[R]): Complex[R] = ??? //todo

     def conjugate[R: RealLike](z: Complex[R]): Complex[R] = Complex(z.re, z.im.negate())
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


// ---------------------------------------------------------------------------------------------------------



object NumberTester extends App {


     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1)
     val b: Complex[Int] = 3 + 5.i + 3
     val c: Complex[Int] = 1 + 2.i

     println(b < c)


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
     println((8 + 2.i) < (9 + 2.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}