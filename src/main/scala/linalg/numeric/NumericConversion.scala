package linalg.numeric

import linalg.syntax.NumberSyntax._
/**
  *
  */

//represents a conversion between numbers
//todo is it weird that it has similar methods as Number? Repetitive? Maybe have general number type with
// SimpleNumber[R] ext Number[R, R]

trait NumericConversion[F, T] {
     def plus(from: F, to: T): T
     def minus(from: F, to: T): T
     def times(from: F, to: T): T
     def divide(from: F, to: T): T
     def exponentiate(base: T, exp: F): T
}
object NumericConversion {
     //mechanism: takes something that implements RealNumber and gives it .i accessor, returning Imaginary.
     implicit class ToImaginary[R: RealLike](private val imaginaryPart: R){

          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }

     //mechanism: takes something that implements RealNumber and makes it addable with Imaginary (which BTW cannot
     // implement Number because i*i = -1, not imaginary)
     implicit class ToComplex[R: RealLike](private val realPart: R)/*(implicit compLike: ComplexLike[Imaginary[R], R])*/ {

          def +(that: Imaginary[R]) = Complex(realPart, that.im) //compLike.imag(that)) //can just do that.im
          def -(that: Imaginary[R]) = Complex(realPart, that.im.negate()) //compLike.imag(that).negate())
     }

     // ---------------------------------------------------------------------------------------------

     implicit def GeneralRealToComplex[R: RealLike]
     /*(implicit root0: Root0[Complex[R], R])*/: NumericConversion[R, Complex[R]] = new NumericConversion[R, Complex[R]]{

          def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
          def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
          def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
          def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
          def exponentiate(base: Complex[R], exp: R): Complex[R] = base ^ exp
          //root0.power(base, exp) //todo why cannot base ^ exp
     }

     implicit class ConvertFrom[F, T](val from: F)(implicit conv: NumericConversion[F, T]){
          def +(to: T): T = conv.plus(from, to)
          def -(to: T): T = conv.minus(from, to)
          def *(to: T): T = conv.times(from, to)
          def /(to: T): T = conv.divide(from, to)
          def ^(exp: T): T = conv.exponentiate(exp, from)
     }
     implicit class ConvertTo[F, T](val to: T)(implicit conv: NumericConversion[F, T]){
          def +(from: F): T = conv.plus(from, to)
          def -(from: F): T = conv.minus(from, to)
          def *(from: F): T = conv.times(from, to)
          def /(from: F): T = conv.divide(from, to)
          def ^(exp: F): T = conv.exponentiate(to, exp)
     }
}