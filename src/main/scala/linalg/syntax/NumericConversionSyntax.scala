package linalg.syntax

import linalg.implicits._
import linalg.kernel.{Complex, Imaginary, NumericConversion, RealNumber}

/**
  *
  */

trait NumericConversionSyntax {

     //mechanism: takes something that implements RealNumber and gives it .i accessor, returning Imaginary.
     implicit class ToImaginary[R: RealNumber](private val imaginaryPart: R){

          def i: Imaginary[R] = Imaginary(imaginaryPart)
     }

     //mechanism: takes something that implements RealNumber and makes it addable with Imaginary (which BTW cannot
     // implement Number because i*i = -1, not imaginary)
     implicit class ToComplex[R: RealNumber](private val realPart: R)/*(implicit compLike: ComplexLike[Imaginary[R], R])*/ {

          def +(that: Imaginary[R]) = Complex(realPart, that.im) //compLike.imag(that)) //can just do that.im
          def -(that: Imaginary[R]) = Complex(realPart, that.im.negate()) //compLike.imag(that).negate())
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
