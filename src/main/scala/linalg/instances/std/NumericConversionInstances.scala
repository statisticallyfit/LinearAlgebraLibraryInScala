package linalg.instances.std

import linalg.implicits._
import linalg.kernel._

/**
  *
  */

trait NumericConversionInstances {

     implicit def GeneralRealToComplex[R: RealNumber](implicit root: Root[Complex[R], R]):
     NumericConversion[R, Complex[R], Complex[R]] = new NumericConversion[R, Complex[R], Complex[R]]{

          def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
          def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
          def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
          def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
     }

     implicit def ComplexPlusRealToComplex[R:RealNumber](implicit root: Root[Complex[R], R]):
     NumericConversion[Complex[R], R, Complex[R]] = new NumericConversion[Complex[R], R, Complex[R]] {

          def plus(upper: Complex[R], from: R): Complex[R] = Complex(from + upper.re, upper.im)
          def minus(upper: Complex[R], from: R): Complex[R] = Complex(from - upper.re, upper.im)
          def times(upper: Complex[R], from: R): Complex[R] = Complex(from * upper.re, from * upper.im)
          def divide(upper: Complex[R], from: R): Complex[R] = Complex(upper.re / from, upper.im / from)
     }
}

//TODO learn more about spire conversion here:
// https://insight.io/github.com/non/spire/blob/master/core/shared/src/main/scala/spire/math/Convertable.scala
