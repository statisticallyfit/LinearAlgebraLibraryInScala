package linalg.instances.std

import linalg.implicits._
import linalg.kernel._

/**
  *
  */

trait NumericConversionInstances {

     implicit def GeneralRealToComplex[R: RealNumber](implicit root: Root[Complex[R], R]):
     NumericConversion[R, Complex[R]] = new NumericConversion[R, Complex[R]]{

          def plus(from: R, to: Complex[R]): Complex[R] = Complex(from + to.re, to.im)
          def minus(from: R, to: Complex[R]): Complex[R] = Complex(from - to.re, to.im)
          def times(from: R, to: Complex[R]): Complex[R] = Complex(from * to.re, from * to.im)
          def divide(from: R, to: Complex[R]): Complex[R] = Complex(to.re / from, to.im / from)
          //def exponentiate(base: Complex[R], exp: R): Complex[R] = root.power(base, exp)
               //todo why error compile? base ^ exp
     }

     implicit def ComplexPlusRealToComplex[R:RealNumber](implicit root: Root[Complex[R], R]):
     NumericConversion2[Complex[R], R] = new NumericConversion2[Complex[R], R] {

          def plus(upper: Complex[R], from: R): Complex[R] = Complex(from + upper.re, upper.im)
          def minus(upper: Complex[R], from: R): Complex[R] = Complex(from - upper.re, upper.im)
          def times(upper: Complex[R], from: R): Complex[R] = Complex(from * upper.re, from * upper.im)
          def divide(upper: Complex[R], from: R): Complex[R] = Complex(upper.re / from, upper.im / from)
          //def exponentiate(base: Complex[R], exp: R): Complex[R] = root.power(base, exp)
     }
}


//represents a conversion between numbers
//todo is it weird that it has similar methods as Number? Repetitive? Maybe have general number type with
// SimpleNumber[R] ext Number[R, R]

//TODO learn more about spire conversion here:
// https://insight.io/github.com/non/spire/blob/master/core/shared/src/main/scala/spire/math/Convertable.scala
