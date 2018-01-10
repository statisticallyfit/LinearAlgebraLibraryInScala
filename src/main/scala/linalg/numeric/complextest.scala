package linalg.numeric


import linalg.theory._
import linalg.util.ImplicitConversions._
import org.apache.commons.lang3.math.Fraction
import spire.math.{Complex, Rational}
import spire.math.Complex._

import scala.language.implicitConversions




object complextest extends App {

     val j: Complex[Int] = Complex.i[Int]

     val a: Complex[Int] = Complex(2,3) + 5
     val b: Complex[Rational] = Complex(Rational(2,3), Rational(4,5))

     // can't do this with spire unfortunately, so must build my own if wanting to be fancy.
     //val c: Complex[Int] = 3 + 3*j
}