package linalg.kernel


import linalg.theory.Field
import linalg.implicits._

import cats.Eq

import scala.language.implicitConversions
import scala.language.higherKinds



//note cannot have extending AbsoluteLike because of same old problem, so just implement them separately in the
//implicit typeclass declaration

trait Number[N] extends Field[N] with Trig[N] with Equality[N] with Eq[N]
     /*with Root[N] with Absolute[N]*/ {

     val two: N = plus(one, one)

     def minus(x: N, y: N): N = plus(x, negate(y))

     def isZero(x: N): Boolean = eqv(zero, x)
     def isNegative(x: N): Boolean

     def doubleValue(x: N): Double
     def from(x: Int): N
     //def to(x: Number[N]): N
}

object Number {

     def ZERO[N](implicit gen: Number[N]): N = gen.zero
     def ONE[N](implicit gen: Number[N]): N = gen.one
     def TWO[N](implicit gen: Number[N]): N = gen.two

     @inline final def apply[N](implicit ev: Number[N]): Number[N] = ev
}



trait RealNumber[R] extends Number[R] with Root[R] with Absolute[R]


object RealNumber {
     def ZERO[R](implicit gen: RealNumber[R]): R = gen.zero
     def ONE[R](implicit gen: RealNumber[R]): R = gen.one
     def TWO[R](implicit gen: RealNumber[R]): R = gen.two

     @inline final def apply[R](implicit ev: RealNumber[R]): RealNumber[R] = ev
}




object NumberTester extends App {


     import Number._

     val a: Complex[Rational] = Rational(3,5) + Rational(2, 4).i + Rational(1)
     val b: Complex[Int] = 3 + 5.i + 3
     val c: Complex[Int] = 1 - 2.i

     val r1: Rational = Rational(2)
     val r2: Rational = Rational(4,5)


     println("NROOT TEST: " + Complex(1.0, 2.0).nRoot(2))
     println("^ test: " + (Complex(1,2) ^ 2))
     println("NROOT TEST: " + (Rational(2) ^ Rational(2)))
     println("ABS TEST: " + Real(-2).abs())

     //import linalg.syntax.AbsoluteSyntax._
     //import scala.runtime.{RichInt => _, _}
     //import scala.runtime.{ScalaNumberProxy => _, _}
     //println("ABS TEST: " + (-24).abs()) //todo this uses RichInt's abs method how to stop this?
     println("ABS TEST: " + Complex[Double](-1, 2).abs())


     println(r1 + r2)
     println(c)

     println(b < c)
     println(b :==: c)
     println((4 + 3.i) :==: (4 + 3.i))
     println((2 + 5.i) < (2 + 7.i))
     println((2 + 5.i) < (2 - 5.i))
     println((8 + 2.i) + (9 + 2.i))
     println((8 + 2.i) - (9 + 2.i))
     println((8 + 2.i) < (9 + 2.i))

     println(a)
     println(b)
     println(a + Rational(1))
     println(Rational(33) + a)
     println(23.0 + (1.0 + 3.0.i))
     println((1.5 + 3.2.i) + 23.2)
     println((1 + 3.i) + 1)
     println(1 + (1 + 3.i))

     println(new Rational(4, 8))
     println(Rational(4, 8) + Rational(5, 15))
     println(Complex(1,2))
     println(Complex(1,2) + Complex(3,4))
}