package numerics


import theory._



sealed trait Number[T] extends Ring[T] with Field[T] with Ordered[T]{
     //inherited: ONE, ZERO, +, -, *, /, inverse, compare
     def negate(): T
     def abs()
     def ^(exp: T): T
     def sqrt()

     def equals(that: T): Boolean
     def isZero: Boolean
     def isNegative: Boolean
}
object Number {

     implicit class IntOps(int: Int) extends Number[Int] {
          override val ONE: Int = 1
          override val ZERO: Int = 0

          def +(other: Int): Int = int + other
          def *(other: Int): Int = int * other
          def ^(exp: Int): Int = int ^ exp
          def /(other: Int): Int = int / other
          def sqrt(): Double = scala.math.sqrt(int)

          def abs(): Int = if(int < 0) int.negate() else int

          def inverse(): Int = int.negate()

          def negate(): Int = int * -1 //Option[Int] = None

          def compare(other: Int): Int = other + int.inverse()

          def equals(other: Int): Boolean = int == other
          def isNegative: Boolean = int < 0
          def isZero: Boolean = int == 0
     }

     implicit class DoubleOps(double: Double) extends Number[Double] {
          override val ONE: Double = 1
          override val ZERO: Double = 0

          def +(other: Double): Double = double + other
          def *(other: Double): Double = double * other
          def ^(exp: Double): Double = double ^ exp
          def /(other: Double): Double = double / other
          def sqrt(): Double = scala.math.sqrt(double)

          def abs(): Double = if(double < 0) double.negate() else double

          def inverse(): Double = double.negate()

          def negate(): Double = double * -1 //Option[Int] = None

          def compare(other: Double): Int = (double - other).toInt

          def equals(other: Double): Boolean = double == other
          def isNegative: Boolean = double < 0
          def isZero: Boolean = double == 0
     }
}

case class Complex[N](re: N, im: N)(implicit n: Number[N]) {
     val ZERO: Complex[N] = Complex(n.ZERO, n.ZERO)
     val ONE: Complex[N] = Complex(n.ONE, n.ZERO)

     def +(other: Complex[N]): Complex[N] = Complex(n + other.re, other.im)
     //def -(other: Complex[N]): Complex[N] = Complex(n  )
}

object Tester extends App {
     val c1 = Complex(3, 2)
     val c2 = Complex(5, 1)
     println(c1 + c2)
}
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
object Complex {

}

class Real(override val re: Double) extends Complex[Double](re, 0)
class Rational(num: Int, denom: Int) extends Real(num * 1.0 / denom)
//integers

//case class Whole(whole: Int) extends Integer(whole)
class Natural(nat: Int) extends Rational(nat, 1) //{ require}


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
