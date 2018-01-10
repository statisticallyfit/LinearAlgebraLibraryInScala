//package linalg.numeric
//
//
//import linalg.theory._
//import linalg.util.ImplicitConversions._
//import org.apache.commons.lang3.math.Fraction
//
//import scala.language.implicitConversions
//
//
//
//trait Number[N] /*extends Field[N]*/ {
//
//     def plus(x: N, y: N): N
//}
//
//trait RealNumber[R] extends Number[R]
//
//
//object Number {
//
//     implicit class NumberOps[N : Number](current: N) {
//          val ev = implicitly[Number[N]]
//
//          def +(other: N): N = ev.plus(current, other)
//
//     }
//
//     implicit object IntIsRealNumber extends RealNumber[Int] {
//          def plus(x: Int, y: Int): Int = x + y
//     }
//
//     implicit def ComplexIsRealNumber[R: RealNumber] = new RealNumber[Complex[R]] {
//
//          type C = Complex[R]
//          val gen = implicitly[RealNumber[R]]
//
//          def plus(x: C, y: C): C = Complex(x.re + y.re, x.im + y.im)
//     }
//
//     implicit object RealIsRealNumber extends RealNumber[Real] {
//          def plus(x: Real, y: Real): Real = Real(x.value + y.value)
//     }
//
//     implicit object RationalIsNumber extends RealNumber[Rational] {
//          def plus(x: Rational, y: Rational): Rational =
//               Rational(x.num*y.den + y.num*x.den, x.den * y.den)
//     }
//}
//import Number._
//
//
//case class Complex[R: RealNumber](re: R, im:R){
//     override def toString = re.toString + " + " + im.toString + "i"
//}
//
//case class Real(value: Double) {
//     override def toString = value.toString
//}
//
//case class Rational(private val n: Int, private val d: Int){
//     val reduced: Fraction = Fraction.getFraction(n, d).reduce()
//     val num: Int = reduced.getNumerator
//     val den: Int = reduced.getDenominator
//
//     override def toString = num.toString + "/" + den.toString
//}
//
//
//
//object NumberTester extends App {
//
//     println(Complex(Real(2), Real(3)))
//
//     println(Complex(3,5) + Complex(5, 6))
//}