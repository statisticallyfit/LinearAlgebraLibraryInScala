package linalg.instances.std

import cats.Eq
import linalg.implicits._
import linalg._
import linalg.kernel.{Complex, Rational, Real}
//import linalg.theory.{AbelianGroup, Field, Monoid, Ring}

/**
  *
  */

//note
//note benefits of using class ComplesIsSomething versus trait
// note -- 1: can extend multiple classes, not just one (but only if we extend each previous class
// like in vectorinstances, here we cannot do that!)
// note -- 2: can say new so we can do many instances in the instances trait at the end

class ComplexThings[R:RealNumber] {

     trait ComplexIsAbsolute extends Absolute[Complex[R], R] {
          def absoluteValue(z: Complex[R]): R = Complex.magnitude(z)
     }

     trait ComplexIsRoot extends Root[Complex[R], R] {

          def power(base: Complex[R], exp: R): Complex[R] ={
               //Complex(Complex.magnitude(base) ^ exp, Complex.angle(base) * exp)

               def rationalPowerToComplex(rat: Rational): Complex[R] = {
                    val complexPowerNumerator: Complex[R] = Complex.powerDeMoivre(base, rat.num)

                    val (modulusRoot, nRootsList): (R, List[R]) =
                         Complex.nthRootComplex(complexPowerNumerator, rat.den)
                    val first: R = nRootsList.head

                    modulusRoot * Complex(first.cos(), first.sin())
               }

               val complexPower: Complex[R] = exp match {
                    case r:Rational => rationalPowerToComplex(r)
                    case Real(d) => rationalPowerToComplex(Rational(d))
                    case d:Double => rationalPowerToComplex(Rational(d))
                    case n: Int => Complex.powerDeMoivre(base, n)
               }

               complexPower
          }



     }

     /*trait ComplexIsAbsoluteComplex extends Absolute[Complex[R], Complex[R]]{
          def absoluteValue(z: Complex[R]): Complex[R] =
               Complex(Complex.magnitude(z), RealNumber[R].zero)
     }


     //TODO is this right??
     trait ComplexIsRootComplex extends Root[Complex[R], Complex[R]]{
          def power(base: Complex[R], exp: Complex[R]): Complex[R] =
               Complex(Complex.magnitude(base) ^ Complex.magnitude(exp),
                    Complex.angle(base) * Complex.magnitude(exp))
     }*/

     trait ComplexHasEq extends Eq[Complex[R]] {
          def eqv(x: Complex[R], y: Complex[R]): Boolean = x.re === y.re && x.im === y.im
     }

     trait ComplexHasEquality extends ComplexHasEq with Equality[Complex[R]] {
          def lessThan(x: Complex[R], y: Complex[R]): Boolean = x.re < y.re || (x.re === y.re && x.im < y.im)
     }

     trait ComplexIsMonoid extends Monoid[Complex[R]] {
          val zero: Complex[R] = Complex.ZERO[R]

          def plus(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)
     }

     trait ComplexIsAbelianGroup extends ComplexIsMonoid with AbelianGroup[Complex[R]] {
          def negate(x: Complex[R]): Complex[R] = Complex(x.re.negate(), x.im.negate())
     }

     trait ComplexIsRing extends ComplexIsAbelianGroup with Ring[Complex[R]] {
          def times(x: Complex[R], y: Complex[R]): Complex[R] =
               Complex(x.re * y.im - y.re * x.im, x.re * y.re + y.im * x.im)
     }

     trait ComplexIsField extends ComplexIsRing with Field[Complex[R]] {
          val one: Complex[R] = Complex.ONE[R]

          def divide(x: Complex[R], y: Complex[R]): Complex[R] = {
               val prod: Complex[R] = times(x, y)
               val absDenom: R = Complex.magnitude(y)
               Complex(prod.re / absDenom, prod.im / absDenom)
          }
     }


     //note only putting the necessary inheritances here e.g. field,trig -
     // note use traits to extend additional ones like abeliangroup, no need to repeat them here.

     class ComplexIsNumber extends ComplexIsField
          with ComplexIsAbsolute //with ComplexIsAbsoluteComplex
          with ComplexIsRoot //with ComplexIsRootComplex
          with ComplexHasEq
          with ComplexHasEquality
          with Number[Complex[R]]  {

          def conjugate(x: Complex[R]): Complex[R] = Complex(x.re, x.im.negate())
          def isZero(x: Complex[R]): Boolean = x.re.isZero && x.im.isZero
          def isNegative(x: Complex[R]): Boolean = x.re.isNegative && x.im.isNegative
          def doubleValue(x: Complex[R]): Double = Complex.magnitude(x).toDouble
          def denominator(x: Complex[R]): Int = 1
          def from(x: Int): Complex[R] = Complex(RealNumber[R].from(x))
     }

     val number = new ComplexIsNumber
}

trait ComplexInstances {

     implicit final def complexIsNumber[R: RealNumber] = new ComplexThings[R].number
}