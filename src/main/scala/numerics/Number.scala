package numerics


import theory._

import scala.language.implicitConversions


trait Number

private[numerics] trait Numerical[N] extends Field[N] with Ring[N] with AbelianGroup[N] { //intermediary type!

     def plus(x: N, y: N): N
     def minus(x: N, y: N): N
     def times(x: N, y: N): N
     def divide(x: N, y: N): N
}

object Numerical {
     implicit object RealIsNumerical extends Numerical[Real] {
          def plus(x: Real, y: Real): Real = Real(x.value + y.value)
          def minus(x: Real, y: Real): Real = Real(x.value - y.value)
          def times(x: Real, y: Real): Real = Real(x.value * y.value)
          def divide(x: Real, y: Real): Real = Real(x.value / y.value)
     }
}

class Real(val value: Double)(implicit n: Numerical[Real]) extends Number {

     def +(other: Real): Real = n.plus(this, other)
     def -(other: Real): Real = n.minus(this, other)
     def *(other: Real): Real = n.times(this, other)
     def /(other: Real): Real = n.divide(this, other)

     def inverse(): Real = Real(1 / this.value)

     override def toString: String = value.toString
}

object Real {
     def apply(doubleValue: Double) = new Real(doubleValue)
}




object Tester extends App {
     println(Real(24) + Real(31))
     /*import Numerical._

     def addTwoNumbers[A](first: A, second: A)(implicit n: Numerical[A]): A = {
          n.plus(first, second)
     }



     Console.println(addTwoNumbers(Real(24), Real(31)))*/
}
/*sealed trait Number[T <: Numeric[T]] extends Ring[T] with Field[T] with Ordered[T]{
     //inherited: ONE, ZERO, +, -, *, /, inverse, compare, equals
     def negate(): T
     def abs(): Real
     def ^(exp: T): T
     def sqrt(): Real

     def isZero: Boolean
     def isNegative: Boolean

     def plus(a: T, b: T): T
}

/*object Number {
     def plus[A](x: A, y: A)(implicit numeric: Numeric[A]): A = numeric.plus(x, y)

}*/
trait FakeNum[T] {
     def plus(a: T, b: T): T
}

object FakeNum {
     implicit object RealIsNumeric extends FakeNum[Real]{
          def plus(x: Real, y: Real): Real = Real(x.dec + y.dec)
     }
}

object Temp {
     def makeRealsAdd[N](a: N, b: N)(implicit n: FakeNum[N]) = n.plus(a, b)
}

class Real(val dec: Double)(implicit n: FakeNum[Real]) /*{
     def +(other: Real) = n.plus(this, other)
}*/
object Real {
     implicit def apply(real: Double)(implicit n : Numeric[Real]): Real = new Real(real)(n)
}


object NumberTester extends App {

     val r1 = new Real(3)
     val r2 = new Real(5)
     println(r1 + r2 + r1)
     /*val c1: Complex[Int] = Complex(3, 2)
     val c2: Complex[Int] = Complex(5, 1)
     //println(c1 plus c2)
     println(c1.plus(c1, c2))*/
}*/

/*class Complex[N <: Numeric[N]](val re: N, val im: N) extends NumFake[N]{
     def +(other: Complex[N]): Complex[N] = new Complex(re + other.re, im + other.im)
}*/

/*object Number {

     //implicit def intToComplex(int: Int): Complex[Int] = new Complex(int, 0)
     //implicit def doubleToComplex(double: Double): Complex[Double] = new Complex(double, 0)

     implicit class IntOps(int: Int) extends Number[Int] {
          override val ONE: Int = 1
          override val ZERO: Int = 0

          def plus(a: Int, b: Int) = a + b

          def +(other: Int): Int = int + other
          def *(other: Int): Int = int * other
          def ^(exp: Int): Int = int ^ exp
          def /(other: Int): Int = int / other
          def sqrt(): Double = scala.math.sqrt(int)

          def abs(): Double = if(int < 0) int.negate()*1.0 else int*1.0

          def inverse(): Int = int.negate()

          def negate(): Int = int * -1 //Option[Int] = None

          def compare(other: Int): Int = other + int.inverse()

          def isNegative: Boolean = int < 0
          def isZero: Boolean = int == 0
     }

     implicit class DoubleOps(double: Double) extends Number[Double] {
          override val ONE: Double = 1
          override val ZERO: Double = 0

          def plus(a: Double, b: Double) = a + b

          def +(other: Double): Double = double + other
          def *(other: Double): Double = double * other
          def ^(exp: Double): Double = double ^ exp
          def /(other: Double): Double = double / other
          def sqrt(): Double = scala.math.sqrt(double)

          def abs(): Double = if(double < 0) double.negate() else double

          def inverse(): Double = double.negate()

          def negate(): Double = double * -1 //Option[Int] = None

          def compare(other: Double): Int = (double - other).toInt

          def isNegative: Boolean = double < 0
          def isZero: Boolean = double == 0
     }
}*/

/*class Complex[N](val re: N, val im: N)(implicit n: Number[N]) /*extends Number[Complex[N]]*/ {

     val ZERO: Complex[N] = Complex(n.ZERO, n.ZERO)
     val ONE: Complex[N] = Complex(n.ONE, n.ZERO)

     //def +(other: Complex[N]): Complex[N] = new Complex[N](n + other.re, other.im)
     // .im)
     //def -(other: Complex[N]): Complex[N] = Complex(n  )
     def plus(a: N, b: N): N = n.plus(a, b)
}

object Complex {

     implicit def apply[N](re: N, im: N)(implicit n: Number[N]) = new Complex[N](re, im)(n)

     implicit def apply(int: Int)(implicit n: Number[Int]): Complex[Int] =  new Complex[Int](int, 0)(n)

     implicit def apply(double: Double)(implicit n: Number[Double]): Complex[Double] =  new Complex[Double](double, 0)(n)
}*/









//------------------------------------------------------------------------------------------------------

/*case class Complex(re: Int, im: Int) extends Number[Complex] { //(implicit n: Number[Complex])//
     val ZERO: Complex = Complex(0, 0)
     val ONE: Complex = Complex(1, 0)

     def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
     def -(that: Complex): Complex = Complex(re - that.re, im - that.im)
     def *(that: Complex): Complex = Complex(re*that.im - that.re*im, re*that.re + that.im*im)
     def ^(power: )
     def /(that: Complex): Complex = {
          //if (b.isZero) throw new ArithmeticException
          val quot: Complex = this * that.conjugate()
          val recMod: Double = that.re ^ 2 + that.im ^ 2
          new Complex(quot.re/recMod, quot.im/recMod)
     }
     def conjugate(): Complex = Complex(re, -im)

}*/


/*class Real(override val re: Double) extends Complex[Double](re, 0)

class Rational(val num: Int, val denom: Int) extends Real(num * 1.0 / denom)

class Natural(nat: Int) extends Rational(nat, 1) //{ require}*/


/*object Complex {
     implicit def apply(n: Int): Complex[Int] = new Complex(n)
}*/

/*
object Number {

     implicit val complexNumber = new Number[Complex[Int]] {

          /*def /(a: Complex[Int], b: Complex[Int]): Complex[Int] = {
               //if (b.isZero) throw new ArithmeticException
               val quot: Complex[Int] = a * conjugate(b)
               val recMod: Real = (that.real ^ 2) + (that.imaginary ^ 2)
               new Complex(quot.real/recMod, quot.imaginary/recMod)
          }*/
          //todo def ^(a: Complex[Int], b: Complex[Int]): Complex[Int]
          //def sqrt(c: Complex[Int]): Real

          def inverse(c: Complex[Int]): Complex[Int] = conjugate(c)
          def abs(c: Complex[Int]): Real = Real(0) //todo
          //todo def abs(c: Complex[Int]): Real = Real((c.re^2 + c.im^2).sqrt())

          def conjugate(c: Complex[Int]): Complex[Int] = Complex(c.re, -c.im)

          /*todo def compareHelper(a: Complex[Int], b: Complex[Int]): Int = abs(a) - abs(b)
          def compare(other: Complex[Int]): Int = compareHelper(this, other)*/

          def areEqual(a: Complex[Int], b: Complex[Int]): Boolean = a.re == b.re && a.im == b.im

     }
}*/
