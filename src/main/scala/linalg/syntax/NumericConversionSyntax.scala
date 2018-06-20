package linalg.syntax

import linalg.implicits._
import linalg.kernel.{Complex, Imaginary, NumericConversion, NumericConversion2, RealNumber}

import scala.language.higherKinds
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

     implicit class ConvertFrom[F, T[_]](val from: F)(implicit conv: NumericConversion[F, T[F]]){
          def +(to: T[F]): T[F] = conv.plus(from, to)
          def -(to: T[F]): T[F] = conv.minus(from, to)
          def *(to: T[F]): T[F] = conv.times(from, to)
          def /(to: T[F]): T[F] = conv.divide(from, to)
          def ^(exp: T[F]): T[F] = conv.exponentiate(exp, from)
     }
     //?? TODO need this thing?
     /*implicit class ConvertTo[F, T](val to: T)(implicit conv: NumericConversion[F, T]){
          def +(from: F): T = conv.plus(from, to)
          def -(from: F): T = conv.minus(from, to)
          def *(from: F): T = conv.times(from, to)
          def /(from: F): T = conv.divide(from, to)
          def ^(exp: F): T = conv.exponentiate(to, exp)
     }*/

     implicit class ConvertTo2[T[_], F](val to: T[F])(implicit conv: NumericConversion2[T[F], F]){
          def +(from: F): T[F] = conv.plus(to, from)
          def -(from: F): T[F] = conv.minus(to, from)
          def *(from: F): T[F] = conv.times(to, from)
          def /(from: F): T[F] = conv.divide(to, from)
          //def ^(exp: F): T[F] = conv.exponentiate(to, exp)
     }
}
