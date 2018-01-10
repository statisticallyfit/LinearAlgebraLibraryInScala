//package linalg.numeric
//
//
//import spire.math.{Complex, Rational}
//import scala.language.implicitConversions
//
//
//
//
//object complextest extends App {
//
//     val j: Complex[Int] = Complex.i[Int]
//
//     val a: Complex[Int] = Complex(2,3) + 5
//     val b: Complex[Rational] = Complex(Rational(2,3), Rational(4,5))
//
//     println(a)
//     println(b)
//     println(Complex(2,3).eq)
//
//     // can't do this with spire unfortunately, so must build my own if wanting to be fancy.
//     //val c: Complex[Int] = 3 + 3*j
//}