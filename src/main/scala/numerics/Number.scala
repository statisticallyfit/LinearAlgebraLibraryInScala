package numerics


import theory._


/*val ZERO: T
     val ONE: T

     def +(that: T)
     def -(that: T)
     def *(that: T)
     def /(that: T)
     def ^(): T
     def sqrt(): T

     def inverse(): T
     def opposite(): T

     def abs(): T

     def compare(that: T): Int
     def equals(that: T): Boolean*/


sealed trait Number[T] extends Ring[T] with Field[T] with Ordered[T]

case class Complex[T](re: T, im: T) extends Number[Complex[T]]
case class Real(override val re: Double) extends Complex[Real](Real(re), Real(0))
case class Rational(num: Int, denom: Int) extends Real(num * 1.0 / denom)
case class Natural(nat: Int) extends Rational(nat, 1) //{ require}

/*object Complex {
     implicit def apply(n: Int): Complex[Int] = new Complex(n)
}*/

object Number {

     implicit val complexNumber = new Number[Complex[Int]] {

          override val ZERO: Complex[Int] = Complex(0,0)
          override val ONE: Complex[Int] = Complex(1, 0)

          def +(a: Complex[Int], b: Complex[Int]): Complex[Int] = Complex(a.re + b.re, a.im + b.im)

          def -(a: Complex[Int], b: Complex[Int]): Complex[Int] = Complex(a.re - b.re, a.im - b.im)

          def *(a: Complex[Int], b: Complex[Int]): Complex[Int] ={
               val (x, y) = (a.re, a.im)
               val (z, w) = (b.re, b.im)
               Complex(x*w - z*y, x*z + w*y)
          }
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
}