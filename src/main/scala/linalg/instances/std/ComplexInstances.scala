package linalg.instances.std

import linalg.implicits._
import linalg._
import linalg.kernel.{Complex}
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
          def power(base: Complex[R], exp: R): Complex[R] =
               Complex(Complex.magnitude(base) ^ exp, Complex.angle(base) * exp)
     }

     trait ComplexHasEquality extends Equality[Complex[R]] {
          def eqv(x: Complex[R], y: Complex[R]): Boolean = x.re :==: y.re && x.im :==: y.im

          def lessThan(x: Complex[R], y: Complex[R]): Boolean = x.re < y.re || (x.re :==: y.re && x.im < y.im)
     }

     //TODO ok to extend Number ? or cyclic reference?

     /*trait ComplexIsTrig extends Trig[Complex[R]] {
          //val E: Complex[R] = ??? //Complex(scala.math.E).asInstanceOf[Complex[R]] //todo more graceful way?
          //val PI: Complex[R] = ??? //Complex(scala.math.Pi).asInstanceOf[Complex[R]]

          //todo major todo
          def sin(x: Complex[R]): Complex[R] = Complex(scala.math.sin(x.re.toDouble),
               scala.math.sin(x.im.toDouble))
          def cos(x: Complex[R]): Complex[R] = ???
          def tan(x: Complex[R]): Complex[R] = ???

          def arcsin(x: Complex[R]): Complex[R] = ???
          def arccos(x: Complex[R]): Complex[R] = ???
          def arctan(x: Complex[R]): Complex[R] = ???
     }*/

     trait ComplexIsMonoid extends Monoid[Complex[R]] {
          val zero: Complex[R] = Complex.ZERO[R]

          def plus(x: Complex[R], y: Complex[R]): Complex[R] = Complex(x.re + y.re, x.im + y.im)
     }

     //todo what to do here since class must be first , can't have more than one class

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
          //with ComplexIsTrig
          with ComplexIsAbsolute
          with ComplexIsRoot
          with ComplexHasEquality
          with Number[Complex[R]]  {

          def isZero(x: Complex[R]): Boolean = x.re.isZero && x.im.isZero
          def isNegative(x: Complex[R]): Boolean = x.re.isNegative && x.im.isNegative
          def doubleValue(x: Complex[R]): Double = Complex.magnitude(x).toDouble
          def from(x: Int): Complex[R] = Complex(RealNumber[R].from(x))
     }

     val number = new ComplexIsNumber
}

trait ComplexInstances {

     implicit final def complexIsNumber[R: RealNumber] = new ComplexThings[R].number
}